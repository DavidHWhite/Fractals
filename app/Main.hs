{-# LANGUAGE FlexibleContexts #-}

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
   -- Fixes some issues with output text not displaying in the console.
   hSetBuffering stdout NoBuffering

   putStrLn "Select a fractal to generate:"
   putStrLn "   1. Mandelbrot Set"
   putStrLn "   2. Julia Set"
   putStrLn "   3. Buddhabrot"
   fracType <- fmap read getLine
   putStr "Pixels across = "
   pH <- fmap read getLine
   putStr "Pixels down = "
   pV <- fmap read getLine
   putStr "Center r = "
   rC <- fmap read getLine
   putStr "Center i = "
   iC <- fmap read getLine
   putStr "Horizontal range from center = "
   distance <- fmap read getLine
   putStr "Maximum iterations = "
   maxIterations <- fmap read getLine
   putStr "Power = "
   power <- fmap read getLine
   putStr "File path = "
   path <- getLine
   putStrLn "Select a color function:"
   putStrLn "  1. Greyscale"
   putStrLn "  2. Full Hue"
   putStr "Color function = "
   colorFunc <- fmap (([colorGrey, colorHue] !!) . subtract 1 . read) getLine
   putStr "Color palette length = "
   paletteLength <- fmap read getLine
   putStrLn "Select an animation type:"
   putStrLn "  1. Power"
   putStrLn "  2. Horizontal range"
   putStrLn "  3. Maximum Iterations"
   putStr "Type = "
   if fracType == 2 then putStrLn "  4. Circular C-Value" else return ()
   animType <- fmap read getLine
   if animType == 3
      then
         putStrLn
         $  "Be aware that a frame's maximum iteration count must be an "
         ++ "integral value, potentially causing unexpected behavior in "
         ++ "your animation if your frame count is > maxValue - minValue + 1."
      else return ()
   (startVal, endVal, increment, frames) <- getAnimPrefs
      (case animType of
         1         -> power
         2         -> distance
         3         -> fromIntegral maxIterations
         otherwise -> 1.0
      )
   let pixelSize = (2 * distance) / (fromIntegral $ pH - 1)
   let rMin      = rC - distance
   let iMax = iC + (pixelSize * (fromIntegral (pV - 1) / 2))

   let halfV     = 0.5 * fromIntegral pV
   let halfH     = 0.5 * fromIntegral pH
   fractalFunction <- case (fracType, animType) of
      -- Mandelbrot power animation
      (1, 1) -> return $ \power' (r, c) ->
         mandelbrotPoint maxIterations power'
            $  (rMin + fromIntegral c * pixelSize)
            :+ (iMax - fromIntegral r * pixelSize)
      -- Mandelbrot zoom animation
      (1, 2) -> return $ \value (r, c) ->
         mandelbrotPoint maxIterations power
            $  ((fromIntegral c - halfH) * pixelSize * value + rC)
            :+ ((fromIntegral r - halfV) * pixelSize * value - iC)
      -- Mandelbrot maximum iteration count animation
      (1, 3) -> return $ \maxIterations' (r, c) ->
         mandelbrotPoint (floor maxIterations') power
            $  (fromIntegral c * pixelSize + rMin)
            :+ (iMax - fromIntegral r * pixelSize)
      -- (2, 1) ->

   -- Only used if animating zoom
   let pixelScale = exp $ (log (endVal / startVal)) / (frames - 1)

   let range = if startVal == endVal
          then [startVal]
          else if animType == 2
             then map (pixelScale **) [0 .. frames - 1]
             else [startVal, startVal + increment .. endVal]

   let nameLength = length $ show $ ((!!) range $ length range - 2) - increment

   mapM_
      (\value -> I.writeImage
         (  path
         ++ '\\'
         -- :  (show pH)
         -- ++ ','
         -- :  (show pV)
         -- ++ ','
         -- :  (show rC)
         -- ++ ','
         -- :  (show iC)
         -- ++ ','
         -- :  (show distance)
         -- ++ ','
         -- :  (show maxIterations)
         -- ++ ','
         -- :  (show power)
         -- ++ ','
         -- :  (show paletteLength)
         -- ++ ','
         :  (take nameLength $ (show value) ++ (repeat '0'))
         ++ ".png"
         )
         (I.makeImageR
            I.RPU
            (pV, pH)
            (\point -> colorFunc paletteLength $ fractalFunction value point)
         )
      )
      range

printPixels image (rMax, cMax) (r, c) = do
   print $ I.index image (r, c)
   if c == cMax
      then if r == rMax
         then putStrLn "Done"
         else do
            putStrLn ""
            printPixels image (rMax, cMax) (r + 1, 0)
      else printPixels image (rMax, cMax) (r, c + 1)

getAnimPrefs startVal = do
   putStr "Ending value = "
   endVal <- fmap read getLine
   if startVal == endVal
      then return (startVal, endVal, 0.1, 1)
      else do
         putStr "Frames = "
         frames <- fmap read getLine
         return (startVal, endVal, (endVal - startVal) / (frames - 1), frames)

mod' :: RealFloat a => a -> a -> a
mod' x y = x - (y * (fromIntegral $ truncate (x / y)))

colorGrey :: Double -> Maybe Double -> I.Pixel I.RGB Double
colorGrey _ Nothing = I.PixelRGB 0 0 0
colorGrey period (Just x)
   | val < p = I.PixelRGB (val / p) (val / p) (val / p)
   | otherwise = I.PixelRGB ((period - val) / p)
                            ((period - val) / p)
                            ((period - val) / p)
 where
  p   = period / 2
  val = x `mod'` period

colorHue :: Double -> Maybe Double -> I.Pixel I.RGB Double
colorHue _ Nothing                    = I.PixelRGB 0         0         0        
colorHue period (Just i) | x <= 1 / 6 = I.PixelRGB 1         x'        0        
                         | x <= 1 / 3 = I.PixelRGB (2 - x')  1         0        
                         | x <= 1 / 2 = I.PixelRGB 0         1         (x' - 2) 
                         | x <= 2 / 3 = I.PixelRGB 0         (4 - x')  1        
                         | x <= 5 / 6 = I.PixelRGB (x' - 4)  0         1        
                         | otherwise  = I.PixelRGB 1         0         (6 - x')
 where
  x  = (i `mod'` period) / period
  x' = 6 * x

-- colorGradient :: I.Pixel I.RGB Double -> [I.Pixel I.RGB Double] -> Double
--                -> Maybe Double -> I.Pixel I.RGB Double
-- colorGradient convergent _ _ Nothing = convergent
-- colorGradient _ 
