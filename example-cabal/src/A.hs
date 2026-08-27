module A (answer, factor) where

import Nested.C

answer :: Factor -> Int
answer (Factor n) = 42 * n
