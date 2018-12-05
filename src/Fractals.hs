module Fractals
   ( mandelbrotPoint
   , juliaPoint
   )
where

import           Data.Complex

data PointResult = Convergent | Divergent Int (Complex Double)

mandelbrotPoint :: Bool -> Double -> Int -> Double -> Complex Double -> Maybe Double
mandelbrotPoint aa pixelSize maxIterations power point =
   fPoint aa pixelSize maxIterations power (\d z -> z ** (d :+ 0) + point) point

juliaPoint :: Bool -> Double -> Int -> Double -> Complex Double -> Complex Double -> Maybe Double
juliaPoint aa pixelSize maxIterations power c point =
   fPoint aa pixelSize maxIterations power (\d z -> z ** (d :+ 0) + c) point

fPoint
   :: Bool
   -> Double
   -> Int
   -> Double
   -> (Double -> Complex Double -> Complex Double)
   -> Complex Double
   -> Maybe Double
fPoint aaEnable pixelSize maxIterations power f z0 =
   (if aaEnable then antiAlias else id) . normalize . iteratePoint $ z0
 where
  maxMagnitude = 50
  -- Main iteration function
  iteratePoint point' =
     case
           dropWhile ((< maxMagnitude) . magnitude) . take (maxIterations + 1) $ iterate
              (f power)
              point'
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
        $ magnitude final
  -- Function to prevent aliasing in highly "busy" areas of the fractal
  antiAlias :: Maybe Double -> Maybe Double
  antiAlias Nothing = Nothing
  antiAlias (Just x) =
     Just
        $ (\(sum, count) -> sum / count)
        $ foldl
             (\(sum, count) y -> case normalize . iteratePoint $ z0 + y of
                Nothing -> (sum, count)
                Just z  -> (sum + z, count + 1)
             )
             (x, 1)
             [ (-p) :+ (-p)
   --   , (-p) :+ 0
             , (-p) :+ p
   --   , 0 :+ (-p)
   --   , 0 :+ p
             , p :+ (-p)
   --   , p :+ 0
             , p :+ p
             ]
     where p = pixelSize / 3