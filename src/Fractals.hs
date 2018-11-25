module Fractals
   ( PointResult
   , mandelbrotPoint
   )
where

import           Data.Complex

data PointResult = Convergent | Divergent Int (Complex Double)

mandelbrotPoint :: Int -> Double -> Complex Double -> Maybe Double
mandelbrotPoint maxIterations power point =
   normalize . iteratePoint $ point
   where
  maxMagnitude = 50
  -- Main iteration function
  iteratePoint point' =
     case
           dropWhile ((< maxMagnitude) . magnitude)
           . take (maxIterations + 1)
           $ iterate (\z -> z ** (power :+ 0) + point') point'
        of
           []       -> Convergent
           (x : xs) -> Divergent (maxIterations - length xs) x
  -- Prevents the aliasing present in generators which color based purely on escape time
  normalize Convergent      = Nothing
  normalize (Divergent 0 _) = Just 0
  normalize (Divergent iterations final) =
     Just
        $ ((fromIntegral iterations + 1) -)
        . logBase power
        . logBase maxMagnitude
        $ (magnitude final)
  -- Function to prevent aliasing in highly "busy" areas of the fractal
--   antiAlias :: Maybe Double -> Maybe Double
--   antiAlias Nothing  = Nothing
--   antiAlias (Just x) = Just $ (\(sum, count) -> sum / count) $ foldl
--      (\(sum, count) y -> case normalize . iteratePoint $ point + y of
--         Nothing -> (sum, count)
--         Just z  -> (sum + z, count + 1)
--      )
--      (x, 1)
--      [ (-p) :+ (-p)
--      , (-p) :+ 0
--      , (-p) :+ p
--      , 0 :+ (-p)
--      , 0 :+ p
--      , p :+ (-p)
--      , p :+ 0
--      , p :+ p
--      ]
--      where p = pixelSize / 3
-- I'm an idiot: averaging a supersample before colorization would just give another effectively random color due to the circular palatte.