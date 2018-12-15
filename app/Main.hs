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

   -- TODO add help

   -- TODO add parametric cval animation using hint package?

   let options = processArgs args
   putStrLn $ pPrintOptions options
   fractal args options

fractal :: [String] -> Options -> IO ()
fractal args options@(Options fractalType (numRows, numColumns) (cReal :+ cImag) range iterations power aaIn normalization color animation cValue setColor framesIn startingFrame)
   = do
      currentDir <- getCurrentDirectory
      path       <- fmap
         ( ((currentDir ++ "\\Generated Fractals\\") ++)
         . reverse . drop 12 . reverse
         . map (\case ':' -> '\xA789'; x -> x)
         . show
         )
         getCurrentTime
      createDirectoryIfMissing True path
      setCurrentDirectory path
      writeFile ("data.txt") ("Arguments: " ++ (unwords args) ++ "\n" ++ pPrintOptions options)
      mapM_
         (\frame -> do
            I.writeImage
                  (path ++ '\\' : show (frame + 1) ++ ".png")
                  (I.makeImageR I.RPU
                                (numRows, numColumns)
                                (\point -> colorFunc $ fractalFunction frame point)
                  )
            putStr ('\r' : (show $ frame + 1))
         )
         [0 .. (truncate frames) - 1]
 where
  frames = case animation of NoAnimation -> 1; otherwise -> fromIntegral framesIn
  colorFunc =
     case color of
           Greyscale                    -> colorGrey setColor
           Hue                          -> colorHue setColor
           (Gradient isCircular colors) -> colorGrad setColor isCircular colors
        . fmap
             (case normalization of
                Linear                 -> (normLinear iterations)
                (Sigmoid center power) -> normSigmoid center power
                (Periodic period     ) -> normPeriodic period
                (Sine     period     ) -> normSine period
             )
  aa = case aaIn of AAEnabled  -> True; AADisabled -> False
  animVals = map
     (case animation of
        (Zoom final _) -> ((exp $ (log (final / range)) / (frames - 1)) **)
        otherwise ->
           case animation of
                 NoAnimation        -> \x -> 1
                 (Power      final) -> interpolate power final
                 (Iterations final) -> interpolate (fromIntegral iterations) (fromIntegral final)
                 (Theta      final) -> interpolate (phase cValue) (final * pi / 180)
              . (/ (frames - 1))
     )
     [fromIntegral startingFrame - 1 .. frames - 1]
  zoomIVals = case animation of
     (Zoom _ finalIter) -> map
        (round . (interpolate (fromIntegral iterations) (fromIntegral finalIter)) . (/ frames))
        [fromIntegral startingFrame - 1 .. frames - 1]
  fractalFunction :: Int -> (Int, Int) -> Maybe Double
  fractalFunction frame (r, c) = case (fractalType, animation) of
     -- All Mandelbrot animation functions
     (Mandelbrot, NoAnimation) ->
        pMandelbrot aa pixelSize iterations power $ pairToComplex (r, c)
     (Mandelbrot, Power _) ->
        pMandelbrot aa pixelSize iterations aVAL $ pairToComplex (r, c)
     (Mandelbrot, Zoom _ _) ->
        pMandelbrot aa (pixelSize * aVAL) (zoomIVals !! frame) power $ pairToComplexZ (r, c) aVAL
     (Mandelbrot, Iterations _) ->
        pMandelbrot aa pixelSize (round aVAL) power $ pairToComplex (r, c)
     (Mandelbrot, Theta _    ) ->
        error "Theta animations can only be generated for Julia fractals"
     -- All Julia animation functions
     (Julia, NoAnimation) ->
        pJulia aa pixelSize iterations power cValue $ pairToComplex (r, c)
     (Julia, Power _ ) ->
        pJulia aa pixelSize iterations aVAL cValue $ pairToComplex (r, c)
     (Julia, Zoom _ _) ->
        pJulia aa (pixelSize * aVAL) (zoomIVals !! frame) power cValue $ pairToComplexZ (r, c) aVAL
     (Julia, Iterations _) ->
        pJulia aa pixelSize (round aVAL) power cValue $ pairToComplex (r, c)
     (Julia, Theta _) ->
        pJulia aa pixelSize iterations power (mkPolar cMag aVAL) $ pairToComplex (r, c)
   where
    aVAL      = animVals !! frame
    pixelSize = (2 * range) / (fromIntegral $ numColumns - 1)
    cMag      = magnitude cValue
    halfV     = 0.5 * fromIntegral numRows
    halfH     = 0.5 * fromIntegral numColumns
    pairToComplex (r, c) =
       (cReal - range + fromIntegral c * pixelSize)
          :+ (cImag + (pixelSize * (fromIntegral (numRows - 1) / 2)) - fromIntegral r * pixelSize)
    pairToComplexZ (r, c) zoomFactor =
       ((fromIntegral c - halfH) * pixelSize * zoomFactor + cReal)
          :+ ((fromIntegral r - halfV) * pixelSize * zoomFactor - cImag)

interpolate :: Double -> Double -> Double -> Double
interpolate i f p = (i +) $ (* p) $ (f - i)
