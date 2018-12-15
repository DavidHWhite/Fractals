{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Input
import           Fractals
import           Colors
import           Data.Complex
import qualified Graphics.Image                as I
import           System.IO
import           Data.Word
import           System.Environment
import           System.Directory
import           Text.Printf
import           Data.Time

main :: IO ()
main = do
   args <- getArgs
   let options = processArgs args
   putStrLn $ pPrintOptions options
   fractal args options

fractal :: [String] -> Options -> IO ()
fractal args (Options fractalType (numRows, numColumns) center range iterations power aa normalization color animation cvalue setColor framesIn startingFrame)
   = do
      currentDir <- getCurrentDirectory
      path <- fmap (((currentDir ++ "\\Generated Fractals\\") ++) . reverse . drop 12 . reverse . (map (\case ':' -> '\xA789'; x -> x)) . show) getCurrentTime
      createDirectoryIfMissing True path
      setCurrentDirectory path
      writeFile ("arguments.txt") (unwords args)
      mapM_
         (\frame ->
            let value = animRange !! frame
            in
               I.writeImage
                  (path ++ '\\' : show (frame + 1) ++ ".png")
                  (I.makeImageR I.RPU
                                (numRows, numColumns)
                                (\point -> colorFunc $ fractalFunction value point)
                  )
         )
         [0 .. (truncate frames) - 1]
 where
   -- TODO add help
  frames = case animation of
     NoAnimation -> 1
     otherwise   -> fromIntegral framesIn
  colorFunc =
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
  animRange = map
     ( (case animation of
          (Power final     ) -> interpolate power final
          (Zoom final _    ) -> ((exp $ (log (final / range)) / (frames - 1)) **)
          (Iterations final) -> interpolate (fromIntegral iterations) (fromIntegral final)
          (Theta      final) -> interpolate (phase cvalue) final
          NoAnimation        -> id
       )
     . (/ (frames - 1))
     )
     [fromIntegral startingFrame - 1 .. frames - 1]

  zoomIterRange = case animation of
     (Zoom _ finalIter) -> map
        (round . (interpolate (fromIntegral iterations) (fromIntegral finalIter)) . (/ frames))
        [fromIntegral startingFrame - 1 .. frames - 1]
     otherwise -> error "Why is zoomIterRange being used when the selected animation isn't Zoom?"
  pixelSize       = (2 * range) / (fromIntegral $ numColumns - 1)
  rMin            = (realPart center) - range
  iMax            = (imagPart center) + (pixelSize * (fromIntegral (numRows - 1) / 2))
  halfV           = 0.5 * fromIntegral numRows
  halfH           = 0.5 * fromIntegral numColumns
  fractalFunction = case (fractalType, animation)

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

interpolate :: Double -> Double -> Double -> Double
interpolate i f p = (i +) $ (* p) $ (f - i)
