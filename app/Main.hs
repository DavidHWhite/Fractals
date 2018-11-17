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

   -- let !imgU =
   --        I.makeImage (1600, 1600) (\(x, y) -> 0.1) :: I.Image I.RPU I.Y Double
   -- I.writeImage "C:\\Users\\David White\\Pictures\\test\\test.png" imgU
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
   putStrLn "Select an animation:"
   putStrLn "  1. Power"
   putStrLn "  2. Zoom"
   putStrLn "  3. Maximum Iterations"
   if fracType == 2 then putStrLn "  4. Circular C-Value" else putStr ""
   animType                      <- fmap read getLine
   (startVal, endVal, increment) <- getAnimPrefs
   let pixelSize = (2 * distance) / (fromIntegral $ pH - 1)
   let rMin      = rC - distance
   let iMin = iC - (pixelSize * (fromIntegral (pV - 1) / 2))

   let fractalFunction = case (fracType, animType) of
          (1, 1) -> mandelbrotPoint maxIterations

  --         2 -> do
  --            getAnimPrefs
  --         3 -> do
  --            getAnimPrefs

   let indexToComplex (r, c) =
          (fromIntegral c * pixelSize + rMin)
             :+ (fromIntegral r * pixelSize + iMin)

   let range = if startVal == endVal
          then [startVal]
          else [startVal, startVal + increment, endVal]

   let test =
          I.makeImageR
             I.VS
             (pV, pH)
             (\point ->
                I.PixelRGB 0 0
                   . floor
                   . (* 255)
                   . colorGreyscale 10
                   . fractalFunction 2
                   $ indexToComplex point
             ) :: I.Image I.VS I.RGB Word8

   --print $ I.index test (0, 0)
   --printPixels test (pV - 1, pH - 1) (0, 0)

   I.displayImage test

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
            (\point ->
               I.PixelRGB 0 0
                  . colorGreyscale 10
                  . fractalFunction 2
                  $ indexToComplex point
            )
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

colorGreyscale :: Double -> Maybe Double -> Double
colorGreyscale _ Nothing = 0
colorGreyscale period (Just x) | val < p   = val / p
                               | otherwise = (period - val) / p
 where
  p   = period / 2
  val = mod' x period

mod' x y = x - (y * (fromIntegral $ truncate (x / y)))
