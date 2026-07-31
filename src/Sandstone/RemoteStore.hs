{-# Language NamedFieldPuns #-}
-- | Store operations over the Nix daemon protocol.
--
-- Builds with the @builder-rpc-v0@ system feature get a restricted
-- daemon socket via @NIX_REMOTE@. The daemon scans references when
-- store objects are added, and outputs are registered explicitly
-- rather than left behind for Nix to pick up on exit.
--
-- Outside a build there is no scanning and no output submission, so
-- 'declaredStoreOps' states references explicitly and
-- 'realiseDerivation' asks the daemon to build.
module Sandstone.RemoteStore
  ( BuilderRpcM
  , runBuilderRpc
  , runStoreRpc
  , builderSocketPath
  , remoteStoreOps
  , declaredStoreOps
  , registerOutput
  , realiseDerivation
  , storeDir
  ) where

import Control.Exception (bracket)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Default
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Some
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Network.Socket qualified as S
import System.Environment (getEnv)
import System.Nix.Build (BuildMode(..))
import System.Nix.ContentAddress
import System.Nix.Derivation
import System.Nix.Derivation.ATerm qualified as ATerm
import System.Nix.Derivation.Traditional
import System.Nix.DerivedPath
import System.Nix.Hash
import System.Nix.Nar qualified as Nar
import System.Nix.OutputName
import System.Nix.StorePath
import System.Nix.Store.Remote
import System.Nix.Store.Remote.MonadStore (RemoteStoreError(..))

import Sandstone.WriteDerivation (StoreOperations(..))

storeDir :: StoreDir
storeDir = def

-- | 'RemoteStoreT' has no 'MonadFail', which the derivation writer
-- needs, so this routes pattern-match failures through
-- 'RemoteStoreError'.
newtype BuilderRpcM a = BuilderRpcM (RemoteStoreT IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError RemoteStoreError
    , MonadRemoteStore
    )

instance MonadFail BuilderRpcM where
  fail = throwError . RemoteStoreError_Fixme

-- | The daemon socket a @builder-rpc-v0@ builder is given.
builderSocketPath :: IO FilePath
builderSocketPath = do
  remote <- getEnv "NIX_REMOTE"
  case List.stripPrefix "unix://" remote of
    Just p -> pure p
    Nothing -> fail $ "NIX_REMOTE is not a unix:// daemon socket: " <> remote

-- | Connect to the builder's daemon socket and run an action against
-- it, with the protocol profile pinned to 'builderRpcV0' ahead of the
-- handshake so that 'submitOutput' and 'addToStoreScanning' are
-- negotiated.
runBuilderRpc :: FilePath -> BuilderRpcM a -> IO (Either RemoteStoreError a)
runBuilderRpc = runRpcWith $ setProtoVersion builderRpcV0

-- | Connect to an ordinary daemon socket with the default protocol
-- profile.
runStoreRpc :: FilePath -> BuilderRpcM a -> IO (Either RemoteStoreError a)
runStoreRpc = runRpcWith $ pure ()

runRpcWith :: RemoteStoreT IO () -> FilePath -> BuilderRpcM a -> IO (Either RemoteStoreError a)
runRpcWith beforeGreeting socketPath (BuilderRpcM action) =
  bracket open S.close $ \sock ->
    fmap fst $ runRemoteStoreT sock $ do
      beforeGreeting
      _ <- greetServer
      action
  where
    open = do
      sock <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect sock $ S.SockAddrUnix socketPath
      pure sock

remoteStoreOps :: StoreOperations BuilderRpcM
remoteStoreOps = StoreOperations
  { insertDerivation = \drv ->
      withValidName (unStorePathName (name drv) <> ".drv") $ \drvName -> do
        sd <- getStoreDir
        aterm <- derivationATerm sd drv
        addToStoreScanning
          drvName
          -- The daemon reads text and flat adds as raw file contents,
          -- only nar-method adds are NAR-encoded.
          (\yield -> yield $ T.encodeUtf8 aterm)
          ContentAddressMethod_Text
          (Some HashAlgo_SHA256)
  , insertFileFromPath = \fp fileName ->
      withValidName fileName $ \fileName' ->
        addToStoreScanning
          fileName'
          (Nar.dumpPath fp)
          ContentAddressMethod_NixArchive
          (Some HashAlgo_SHA256)
  }

-- | Like 'remoteStoreOps' but with references declared up front, since
-- ordinary daemon connections don't offer the scanning operation.
declaredStoreOps :: StoreOperations BuilderRpcM
declaredStoreOps = StoreOperations
  { insertDerivation = \drv ->
      withValidName (unStorePathName (name drv) <> ".drv") $ \drvName -> do
        sd <- getStoreDir
        aterm <- derivationATerm sd drv
        addTextToStore
          (StoreText drvName aterm)
          (inputStorePaths drv)
          RepairMode_DontRepair
  , insertFileFromPath = \fp fileName ->
      withValidName fileName $ \fileName' ->
        addToStore
          fileName'
          (Nar.dumpPath fp)
          ContentAddressMethod_NixArchive
          (Some HashAlgo_SHA256)
          mempty
          RepairMode_DontRepair
  }

withValidName
  :: Applicative m
  => Text
  -> (StorePathName -> m StorePath)
  -> m (Either InvalidPathError StorePath)
withValidName n k = case mkStorePathName n of
  Left e -> pure $ Left $ PathNameInvalid e
  Right n' -> Right <$> k n'

inputStorePaths :: Derivation -> HashSet StorePath
inputStorePaths = foldMap (HashSet.singleton . rootPath) . derivationInputsToDerivedPaths . inputs
  where
    rootPath = \case
      SingleDerivedPath_Opaque p -> p
      SingleDerivedPath_Built s _ -> rootPath s

-- | Register an already-added store object as an output of the
-- currently running derivation.
registerOutput :: StorePath -> OutputName -> BuilderRpcM ()
registerOutput path = submitOutput (SingleDerivedPath_Opaque path)

-- | Ask the daemon to build all outputs of a derivation.
realiseDerivation :: StorePath -> BuilderRpcM ()
realiseDerivation drv =
  buildPaths
    (Set.singleton $ DerivedPath_Built (SingleDerivedPath_Opaque drv) OutputsSpec_All)
    BuildMode_Normal

derivationATerm :: MonadFail m => StoreDir -> Derivation -> m Text
derivationATerm sd drv = do
  tradInputs <- case inputsToTraditional (inputs drv) of
    Left badPath -> fail $ "dynamic derivation inputs cannot be rendered as ATerm: " <> show badPath
    Right is -> pure is
  pure $ TL.toStrict $ TLB.toLazyText $ ATerm.buildTraditionalDerivation sd $ withoutName drv
    { outputs = fromSpecificOutputs sd (name drv) (outputs drv)
    , inputs = tradInputs
    }
