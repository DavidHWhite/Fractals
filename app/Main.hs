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
   putStr "Maximum horizontal offset from center = "
   distance <- fmap read getLine
   putStr "Maximum iterations = "
   maxIterations <- fmap read getLine
   putStr "File path = "
   path <- getLine


   -- TODO Add color scheme selection


   putStrLn "Select an animation type:"
   putStrLn "  1. Power"
   putStrLn "  2. Zoom"
   putStrLn "  3. Maximum Iterations"
   if fracType == 2 then putStrLn "  4. Circular C-Value" else putStr ""
   animType                      <- fmap read getLine
   (startVal, endVal, increment) <- getAnimPrefs
   let pixelSize = (2 * distance) / (fromIntegral $ pH - 1)
   let rMin      = rC - distance
   let iMax = iC + (pixelSize * (fromIntegral (pV - 1) / 2))

   -- let (fractalFunction, complexGen) = case (fracType, animType) of
   --   (1, 1) ->
   --      ( mandelbrotPoint maxIterations
   --      , \(r, c) ->
   --         (fromIntegral c * pixelSize + rMin)
   --            :+ (fromIntegral r * pixelSize + iMin)
   --      )

   let fractalFunction = case (fracType, animType) of
          (1, 1) -> mandelbrotPoint maxIterations

   let indexToComplex (r, c) =
          (fromIntegral c * pixelSize + rMin)
             :+ (iMax - fromIntegral r * pixelSize)

   let range = if startVal == endVal
          then [startVal]
          else [startVal, startVal + increment .. endVal]

   mapM_
      (\value -> I.writeImage
         (  path
         ++ '\\'
         :  (show pH)
         ++ ','
         :  (show pV)
         ++ ','
         :  (show rC)
         ++ ','
         :  (show iC)
         ++ ','
         :  (show distance)
         ++ ','
         :  (take (length $ show $ endVal - increment)
                  ((show value) ++ (repeat '0'))
            )
         ++ ".png"
         )
         (I.makeImageR
            I.RPU
            (pV, pH)
            (\point -> colorHue 250 . fractalFunction value $ indexToComplex point)
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

getAnimPrefs = do
   putStr "Starting value = "
   startVal <- fmap read getLine
   putStr "Ending value = "
   endVal <- fmap read getLine
   if startVal == endVal
      then return (startVal, endVal, 0.1)
      else do
         putStr "Frames = "
         frames <- fmap read getLine
         return (startVal, endVal, (endVal - startVal) / (frames - 1))

mod' :: RealFloat a => a -> a -> a
mod' x y = x - (y * (fromIntegral $ truncate (x / y)))

colorGreyscale :: Double -> Maybe Double -> I.Pixel I.RGB Double
colorGreyscale _ Nothing = I.PixelRGB 0 0 0
colorGreyscale period (Just x)
   | val < p = I.PixelRGB (val / p) (val / p) (val / p)
   | otherwise = I.PixelRGB ((period - val) / p)
                            ((period - val) / p)
                            ((period - val) / p)
 where
  p   = period / 2
  val = mod' x period

colorHue :: Double -> Maybe Double -> I.Pixel I.RGB Double
colorHue _ Nothing                    = I.PixelRGB 0         0         0        
colorHue period (Just i) | x <= 1 / 6 = I.PixelRGB 1         x'        0        
                         | x <= 1 / 3 = I.PixelRGB (2 - x')  1         0        
                         | x <= 1 / 2 = I.PixelRGB 0         1         (x' - 2) 
                         | x <= 2 / 3 = I.PixelRGB 0         (4 - x')  1        
                         | x <= 5 / 6 = I.PixelRGB (x' - 4)  0         1        
                         | otherwise  = I.PixelRGB 1         0         (6 - x') 
   where x = (i `mod'` period) / period
         x' = 6 * x
