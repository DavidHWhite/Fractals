{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import           Input
import           Fractals
import           Colors
import           Data.Complex
import qualified Graphics.Image                as I
import           System.IO
import           Data.Word
import           System.Environment

main :: IO ()
main = do

   -- TODO add help

   options@(Options fractalType (numRows, numColumns) center range iterations power aa normalization color animation cvalue setColor framesIn) <-
      fmap processArgs getArgs
   let frames = case animation of
          NoAnimation -> 1
          otherwise   -> fromIntegral framesIn
   putStrLn $ pPrintOptions options
   let colorFunc =
          case color of
                Greyscale         -> colorGrey setColor
                Hue               -> colorHue setColor
                (Gradient colors) -> colorGrad setColor colors
             . fmap
                  (case normalization of
                     Linear                 -> (normLinear iterations)
                     (Sigmoid center power) -> normSigmoid center power
                     (Periodic period     ) -> normPeriodic period
                     (Sine     period     ) -> normSine period
                  )

   let
      animRange = map
         ( (case animation of
              (Power final     ) -> interpolate power final
              (Zoom final _    ) -> ((exp $ (log (final / range)) / (frames - 1)) **)
              (Iterations final) -> interpolate (fromIntegral iterations) (fromIntegral final)
              (Theta final)      -> interpolate (phase cvalue) final
              NoAnimation        -> id
           )
         . (/ (frames - 1))
         )
         [0 .. frames - 1]

   let zoomIterRange = case animation of
          (Zoom _ finalIter) -> map
             (round . (interpolate (fromIntegral iterations) (fromIntegral finalIter)) . (/ frames))
             [0 .. frames - 1]
          otherwise ->
             error "Why is zoomIterRange being used when the selected animation isn't Zoom?"

   -- let range = if animType == 2
   --        then [0 .. frames - 1]
   --        else if frames == 1 || juliaFrames == 1
   --           then [startVal]
   --           else if animType == 4
   --              then map ((*) $ 2 * pi / juliaFrames) [0 .. juliaFrames - 1]
   --              else [startVal, startVal + increment .. endVal]



   -- !(juliaC, juliaMagnitude, juliaFrames) <- if fracType == 2
   --    then if animType == 4
   --       then do
   --          putStr "C-Value magnitude = "
   --          m <- fmap read getLine
   --          putStr "Frames = "
   --          f <- fmap read getLine
   --          return (0, m, f)
   --       else do
   --          putStrLn "Select a C-Value definition method"
   --          putStrLn "  1. Rectangular"
   --          putStrLn "  2. Polar"
   --          putStr "Method = "
   --          getLine
   --             >>= (\x -> case x of
   --                    "1" -> do
   --                       putStr "C-Value r = "
   --                       r <- fmap read getLine
   --                       putStr "C-Value i = "
   --                       i <- fmap read getLine
   --                       return $ r :+ i
   --                    "2" -> do
   --                       putStr "C-Value magnitude = "
   --                       m <- fmap read getLine
   --                       putStr "C-Value phase = "
   --                       theta <- fmap read getLine
   --                       return $ mkPolar m theta
   --                    otherwise -> error "Invalid definition method"
   --                 )
   --             >>= \x -> return (x, 0, 0)
   --    else return (0, 0, 0)
   -- let pixelSize          = (2 * distance) / (fromIntegral $ pH - 1)
   -- let rMin               = rC - distance
   -- let iMax = iC + (pixelSize * (fromIntegral (pV - 1) / 2))
   -- let halfV              = 0.5 * fromIntegral pV
   -- let halfH              = 0.5 * fromIntegral pH


   -- -- Only used if animating zoom
   -- let pixelScale = exp $ (log (endVal / startVal)) / (frames - 1)
   -- let changeInIterations = (fromIntegral (endIterations - maxIterations)) / frames

   -- let
   --    fractalFunction = case (fracType, animType) of
   --       -- Mandelbrot power animation
   --       (1, 1) -> \power' (r, c) ->
   --          mandelbrotPoint aaEnable pixelSize maxIterations power'
   --             $  (rMin + fromIntegral c * pixelSize)
   --             :+ (iMax - fromIntegral r * pixelSize)
   --       -- Mandelbrot zoom animation
   --       (1, 2) -> \frame (r, c) ->
   --          let zoomFactor = pixelScale ** frame
   --          in
   --             mandelbrotPoint
   --                aaEnable
   --                (pixelSize * zoomFactor)
   --                (floor ((fromIntegral maxIterations) + changeInIterations * frame))
   --                power
   --             $  ((fromIntegral c - halfH) * pixelSize * zoomFactor + rC)
   --             :+ ((fromIntegral r - halfV) * pixelSize * zoomFactor - iC)
   --       -- Mandelbrot maximum iteration count animation
   --       (1, 3) -> \maxIterations' (r, c) ->
   --          mandelbrotPoint aaEnable pixelSize (floor maxIterations') power
   --             $  (fromIntegral c * pixelSize + rMin)
   --             :+ (iMax - fromIntegral r * pixelSize)
   --       -- Julia power animation
   --       (2, 1) -> \frame (r, c) ->
   --          let zoomFactor = pixelScale ** frame
   --          in  juliaPoint aaEnable
   --                         (pixelSize * zoomFactor)
   --                         (floor ((fromIntegral maxIterations) + changeInIterations * frame))
   --                         power
   --                         juliaC
   --              $  ((fromIntegral c - halfH) * pixelSize * zoomFactor + rC)
   --              :+ ((fromIntegral r - halfV) * pixelSize * zoomFactor - iC)
   --       -- Julia zoom animation
   --       (2, 2) -> \zoomFactor (r, c) ->
   --          juliaPoint aaEnable (pixelSize * zoomFactor) maxIterations power juliaC
   --             $  ((fromIntegral c - halfH) * pixelSize * zoomFactor + rC)
   --             :+ ((fromIntegral r - halfV) * pixelSize * zoomFactor - iC)
   --       -- Julia maximum iteration count animation
   --       (2, 3) -> \maxIterations' (r, c) ->
   --          juliaPoint aaEnable pixelSize (floor maxIterations') power juliaC
   --             $  (fromIntegral c * pixelSize + rMin)
   --             :+ (iMax - fromIntegral r * pixelSize)
   --       -- Julia polar c-value animation
   --       (2, 4) -> \theta (r, c) ->
   --          juliaPoint aaEnable pixelSize maxIterations power (mkPolar juliaMagnitude $ theta)
   --             $  (fromIntegral c * pixelSize + rMin)
   --             :+ (iMax - fromIntegral r * pixelSize)
   --       -- Invalid input handling
   --       otherwise -> error "Invalid fractal type or animation type."

   -- let nameLength = length . show $ ((!!) range $ length range - (min 2 $ length range))

   -- mapM_
   --    (\frame ->
   --       let value = range !! frame
   --       in  I.writeImage
   --              (path ++ '\\' : show frame ++ ".png")
   --              (I.makeImageR I.RPU (pV, pH) (\point -> colorFunc $ fractalFunction value point))
   --    )
   --    [0 .. floor (if animType == 4 then juliaFrames else frames) - 1]

   -- *************************************************
   putStrLn "Nonsense!"
   -- *************************************************

interpolate :: Double -> Double -> Double -> Double
interpolate i f p = (i +) $ (* p) $ (f - i)
