module Fractals
   ( pMandelbrot
   , pJulia
   )
where

import           Data.Complex

data PointResult = Convergent | Divergent Int (Complex Double)

pMandelbrot :: Bool -> Double -> Int -> Double -> Complex Double -> Maybe Double
pMandelbrot aa pixelSize maxIterations power point =
   fPoint aa pixelSize maxIterations power (\d z -> z ** (d :+ 0) + point) point

pJulia :: Bool -> Double -> Int -> Double -> Complex Double -> Complex Double -> Maybe Double
pJulia aa pixelSize maxIterations power c point =
   fPoint aa pixelSize maxIterations power (\d z -> z ** (d :+ 0) + c) point

fPoint
   :: Bool
   -> Double
   -> Int
   -> Double
   -> (Double -> Complex Double -> Complex Double)
   -> Complex Double
   -> Maybe Double
fPoint bAAEnabled pixelSize maxIterations power f z0 = if not bAAEnabled
   then normalize $ iterPoint 0 z0
   else
      (\(sum, count', isConvergent) ->
            if isConvergent then Nothing else Just $ (sum / (numberOfSubpoints - count'))
         )
         $ foldl
              (\(sum, countConv, isConvergent) y -> if countConv > maxConvergent
                 then (sum, countConv, True)
                 else case normalize . iterPoint 0 $ z0 + y of
                    Nothing -> (sum, countConv + 1, False)
                    Just z  -> (sum + z, countConv, False)
              )
              (0, 0, False)
              [ (-size') :+ (-size')
              , (-size') :+ 0
              , (-size') :+ size'
              , 0 :+ (-size')
              , 0 :+ 0
              , 0 :+ size'
              , size' :+ (-size')
              , size' :+ 0
              , size' :+ size'
              ]
 where
  maxMagnitude = 50
  -- Main iteration function
  iterPoint n z | n > maxIterations          = Convergent
                | magnitude z > maxMagnitude = Divergent n z
                | otherwise                  = iterPoint (n + 1) (f power z)
  -- Prevents the aliasing present in generators which color based purely on escape time
  normalize Convergent      = Nothing
  normalize (Divergent 0 _) = Just 0
  normalize (Divergent iterations final) =
     Just
        . (fromIntegral iterations + 1 -)
        . logBase power
        . logBase maxMagnitude
        . magnitude
        $ final
  size'             = pixelSize / 3
  numberOfSubpoints = 9
  maxConvergent     = 4
