module Util where

import Data.Eq ((==))
import Data.EuclideanRing ((/))

-- Division, but when the denominator is zero, returns zero
zeroDiv :: Int -> Int -> Int
zeroDiv n d = if d == 0 then 0 else n / d
