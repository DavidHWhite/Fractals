{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import           Fractals
import           Data.Complex
import qualified Graphics.Image                as I
import qualified Graphics.Image.ColorSpace     as I
import qualified Graphics.Image.Types          as I
import           System.IO
import           Data.Word

main :: IO ()
main = do
   -- Fixes some issues with output text not displaying in the console
   hSetBuffering stdout NoBuffering

   putStrLn "Select a fractal to generate:"
   putStrLn "   1. Mandelbrot Set"
   putStrLn "   2. Julia Set"
   putStr "Type = "
   !fracType <- fmap read getLine
   if fracType < 1 || 2 < fracType then error "Invalid fractal type" else return ()
   putStr "Pixels across = "
   !pH <- fmap read getLine
   putStr "Pixels down = "
   !pV <- fmap read getLine
   putStr "Center r = "
   !rC <- fmap read getLine
   putStr "Center i = "
   !iC <- fmap read getLine
   putStr "Horizontal range from center = "
   !distance <- fmap read getLine
   putStr "Maximum iterations = "
   !maxIterations <- fmap read getLine
   putStr "Power = "
   !power <- fmap read getLine
   putStr "File path = "
   !path <- getLine
   putStr "Should antialiasing be used? (True/False) "
   !aaEnable <- fmap read getLine
   putStrLn "Select a color function:"
   putStrLn "  1. Greyscale"
   putStrLn "  2. Linear Greyscale"
   putStrLn "  3. Full Hue"
   putStr "Color function = "
   !colorFunc <- fmap (([colorGrey, colorScalarGrey, colorHue] !!) . subtract 1 . read) getLine
   putStr "Color palette length = "
   !paletteLength <- fmap read getLine
   putStrLn "Select an animation type:"
   putStrLn "  1. Power"
   putStrLn "  2. Horizontal range"
   putStrLn "  3. Maximum Iterations"
   if fracType == 2 then putStrLn "  4. Polar C-Value" else return ()
   putStr "Type = "
   !animType <- fmap read getLine
   if animType < 1 || (3 == animType && fracType == 1) || 4 < animType
      then error "Invalid animation type"
      else return ()
   if animType == 3
      then
         putStrLn
         $  "Be aware that a frame's maximum iteration count must be an "
         ++ "integral value, potentially causing unexpected behavior in "
         ++ "your animation if your frame count is > (maxValue - minValue + 1)."
      else return ()

   (startVal, endVal, increment, frames) <- if fracType == 2 && animType == 4
      then return (0, 0, 0, 0)
      else getAnimPrefs
         (case animType of
            1         -> power
            2         -> distance
            3         -> fromIntegral maxIterations
            otherwise -> 1.0
         )
   !(juliaC, juliaMagnitude, juliaFrames) <- if fracType == 2
      then if animType == 4
         then do
            putStr "C-Value magnitude = "
            m <- fmap read getLine
            putStr "Frames = "
            f <- fmap read getLine
            return (0, m, f)
         else do
            putStrLn "Select a C-Value definition method"
            putStrLn "  1. Rectangular"
            putStrLn "  2. Polar"
            putStr "Method = "
            getLine
               >>= (\x -> case x of
                      "1" -> do
                         putStr "C-Value r = "
                         r <- fmap read getLine
                         putStr "C-Value i = "
                         i <- fmap read getLine
                         return $ r :+ i
                      "2" -> do
                         putStr "C-Value magnitude = "
                         m <- fmap read getLine
                         putStr "C-Value phase = "
                         theta <- fmap read getLine
                         return $ mkPolar m theta
                      otherwise -> error "Invalid definition method"
                   )
               >>= \x -> return (x, 0, 0)
      else return (0, 0, 0)
   let pixelSize = (2 * distance) / (fromIntegral $ pH - 1)
   let rMin      = rC - distance
   let iMax = iC + (pixelSize * (fromIntegral (pV - 1) / 2))
   let halfV     = 0.5 * fromIntegral pV
   let halfH     = 0.5 * fromIntegral pH

   let
      fractalFunction = case (fracType, animType) of
         -- Mandelbrot power animation
         (1, 1) -> \power' (r, c) ->
            mandelbrotPoint aaEnable pixelSize maxIterations power'
               $  (rMin + fromIntegral c * pixelSize)
               :+ (iMax - fromIntegral r * pixelSize)
         -- Mandelbrot zoom animation
         (1, 2) -> \zoomFactor (r, c) ->
            mandelbrotPoint aaEnable (pixelSize * zoomFactor) maxIterations power
               $  ((fromIntegral c - halfH) * pixelSize * zoomFactor + rC)
               :+ ((fromIntegral r - halfV) * pixelSize * zoomFactor - iC)
         -- Mandelbrot maximum iteration count animation
         (1, 3) -> \maxIterations' (r, c) ->
            mandelbrotPoint aaEnable pixelSize (floor maxIterations') power
               $  (fromIntegral c * pixelSize + rMin)
               :+ (iMax - fromIntegral r * pixelSize)
         -- Julia power animation
         (2, 1) -> \power' (r, c) ->
            juliaPoint aaEnable pixelSize maxIterations power' juliaC
               $  (rMin + fromIntegral c * pixelSize)
               :+ (iMax - fromIntegral r * pixelSize)
         -- Julia zoom animation
         (2, 2) -> \zoomFactor (r, c) ->
            juliaPoint aaEnable (pixelSize * zoomFactor) maxIterations power juliaC
               $  ((fromIntegral c - halfH) * pixelSize * zoomFactor + rC)
               :+ ((fromIntegral r - halfV) * pixelSize * zoomFactor - iC)
         -- Julia maximum iteration count animation
         (2, 3) -> \maxIterations' (r, c) ->
            juliaPoint aaEnable pixelSize (floor maxIterations') power juliaC
               $  (fromIntegral c * pixelSize + rMin)
               :+ (iMax - fromIntegral r * pixelSize)
         -- Julia polar c-value animation
         (2, 4) -> \theta (r, c) ->
            juliaPoint aaEnable pixelSize maxIterations power (mkPolar juliaMagnitude $ theta)
               $  (fromIntegral c * pixelSize + rMin)
               :+ (iMax - fromIntegral r * pixelSize)
         -- Invalid input handling
         otherwise -> error "Invalid fractal type or animation type."

   -- Only used if animating zoom
   let pixelScale = exp $ (log (endVal / startVal)) / (frames - 1)

   let range = if animType == 2
          then map (pixelScale **) [0 .. frames - 1]
          else if frames == 1 || juliaFrames == 1
             then [startVal]
             else if animType == 4
                then map ((*) $ 2 * pi / juliaFrames) [0 .. juliaFrames - 1]
                else [startVal, startVal + increment .. endVal]

   let nameLength = length . show $ ((!!) range $ length range - (min 2 $ length range))

   mapM_
      (\frame ->
         let value = range !! frame
         in
            I.writeImage
               (path ++ '\\' : show frame ++ ".tif")
               (I.makeImageR I.RPU
                             (pV, pH)
                             (\point -> colorFunc paletteLength $ fractalFunction value point)
               )
      )
      [0 .. floor (if animType == 4 then juliaFrames else frames) - 1]

getAnimPrefs startVal = do
   putStr "Ending value = "
   !endVal <- fmap read getLine
   if startVal == endVal
      then return (startVal, endVal, 0.1, 1)
      else do
         putStr "Frames = "
         !frames <- fmap read getLine
         return (startVal, endVal, (endVal - startVal) / (frames - 1), frames)

mod' :: RealFloat a => a -> a -> a
mod' x y = (-) x . (*) y . fromIntegral . truncate $ x / y

colorScalarGrey :: Double -> Maybe Double -> I.Pixel I.RGB Double
colorScalarGrey _      Nothing  = I.PixelRGB 0 0 0
colorScalarGrey period (Just x) = I.PixelRGB val val val where val = x `mod'` period

colorGrey :: Double -> Maybe Double -> I.Pixel I.RGB Double
colorGrey _      Nothing  = I.PixelRGB 0 0 0
colorGrey period (Just x) = I.PixelRGB val val val
   where val = (1 - cos ((x `mod'` period) / period * 6.2831853)) / 2

colorHue :: Double -> Maybe Double -> I.Pixel I.RGB Double
colorHue _ Nothing                    = I.PixelRGB 0        0        0       
colorHue period (Just i) | x <= 1 / 6 = I.PixelRGB x'       0        1       
                         | x <= 1 / 3 = I.PixelRGB 1        0        (2 - x')
                         | x <= 1 / 2 = I.PixelRGB 1        (x' - 2) 0       
                         | x <= 2 / 3 = I.PixelRGB (4 - x') 1        0       
                         | x <= 5 / 6 = I.PixelRGB 0        1        (x' - 4)
                         | otherwise  = I.PixelRGB 0        (6 - x') 1       
 where
  x  = (i `mod'` period) / period
  x' = 6 * x

-- colorGradient :: I.Pixel I.RGB Double -> [I.Pixel I.RGB Double] -> Double
--                -> Maybe Double -> I.Pixel I.RGB Double
-- colorGradient convergent _ _ Nothing = convergent
-- colorGradient _ 
