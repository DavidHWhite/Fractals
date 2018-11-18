module Fractals
   ( PointResult
   , mandelbrotPoint
   )
where

import           Data.Complex

data PointResult = Convergent | Divergent Int (Complex Double)

mandelbrotPoint :: Int -> Double -> Complex Double -> Maybe Double
mandelbrotPoint maxIterations power = normalize . iteratePoint
 where
  maxMagnitude = 50
  -- Prevents the aliasing present in generators which color based purely on escape time
  normalize Convergent      = Nothing
  normalize (Divergent 0 _) = Just 0
  normalize (Divergent iterations final) =
     Just
        $ ((fromIntegral iterations + 1) -)
        . logBase power
        . logBase maxMagnitude
        $ (magnitude final)
  iteratePoint point =
     case
           dropWhile ((< maxMagnitude) . magnitude)
           . take (maxIterations + 1)
           $ iterate (\z -> z ** (power :+ 0) + point) point
        of
           []       -> Convergent
           (x : xs) -> Divergent (maxIterations - length xs) x
