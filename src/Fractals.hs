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
fPoint aaEnable pixelSize maxIterations power f z0 = if not aaEnable
   then normalize . iteratePoint $ z0
   else
      (\(sum, count', isConvergent) -> if isConvergent
            then Nothing
            else Just $ (sum /) $ subtract count' $ fromIntegral $ length offsets
         )
         $ foldl
              (\(sum, countConv, isConvergent) y -> if countConv > maxConvergent
                 then (sum, countConv, True)
                 else case normalize . iteratePoint $ z0 + y of
                    Nothing -> (sum, countConv + 1, False)
                    Just z  -> (sum + z, countConv, False)
              )
              (0, 0, False)
              offsets
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
     Just $ ((fromIntegral iterations + 1) -) . logBase power . logBase maxMagnitude $ magnitude
        final
  size' = pixelSize / 3
  offsets =
     [ (-size') :+ (-size')
  --   , (-size') :+ 0
     , (-size') :+ size'
  --   , 0 :+ (-size')
     , 0 :+ 0
  --   , 0 :+ size'
     , size' :+ (-size')
  --   , size' :+ 0
     , size' :+ size'
     ]
  maxConvergent = 2
