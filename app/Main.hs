{-# LANGUAGE BangPatterns, LambdaCase #-}

module Main where

import           Input
import           Fractals
import           Colors
import           Data.Complex
import qualified Graphics.Image                as I
import qualified Graphics.Image.Interface      as I
import           System.IO
import           Data.Word
import           System.Environment
import           System.Directory
import           Text.Printf
import           Data.Time

-- Entry point for program
main :: IO ()
main = do
   argsIn <- getArgs

   if not (length argsIn == 0) && (
      "h"     `elem` argsIn ||
      "-h"    `elem` argsIn ||
      "help"  `elem` argsIn ||
      "-help" `elem` argsIn ||
      "?"     `elem` argsIn ||
      "-?"    `elem` argsIn )
     then do
        putStrLn help
        hFlush stdout
     else do
        args <- getArgs
        let options = processArgs args
        putStrLn $ pPrintOptions options
        hFlush stdout
        fractal args options

   -- TODO add parametric cval animation using hint package?

fractal :: [String] -> Options -> IO ()
fractal args options@(Options fractalType (numRows, numColumns) (cReal :+ cImag)
                              range iterations power aaIn normalization color
                              animation cValue setColor framesIn startingFrame)
   = do
      currentDir <- getCurrentDirectory
      {- Path is generated by finding the current date/time, replacing colons (an invalid character in
       Windows paths) with an alternate Unicode colon, and removing the last 12 digits (which contain
       the fractional part of the seconds and the "UTC" code) -}
      path       <- fmap
         ( ((currentDir ++ "\\Generated Fractals\\") ++)
         . reverse . drop 12 . reverse
         . map (\case ':' -> '\xA789'; x -> x)
         . show
         )
         getCurrentTime
      createDirectoryIfMissing True path
      setCurrentDirectory path
      writeFile ("data.txt") $ (pPrintOptions options) ++ '\n' : "Arguments:       " ++ (unwords args)
      putStr $ "0/" ++ (show frames)
      hFlush stdout
      mapM_
         (\frame -> do
            I.writeImageExact (I.PNG) []
                  (path ++ '\\' : show (frame + startingFrame) ++ ".png") $
                  I.toManifest $ I.toWord8I $ I.makeImageR I.RPS
                                       (numRows, numColumns)
                                       (\point -> colorFunc $ fractalFunction frame point)
            putStr ('\r' : (show $ frame + 1) ++ '/' : (show frames))
            hFlush stdout
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
                 (Magnitude  final) -> interpolate (magnitude cValue) final
              . (if frames /= 1 then (/ (frames - 1)) else id)
     )
     [fromIntegral startingFrame - 1 .. frames - 1]
  zoomIVals = case animation of
     (Zoom _ finalIter) -> map
        (round . (interpolate (fromIntegral iterations) (fromIntegral finalIter)) . (/ frames))
        [fromIntegral startingFrame - 1 .. frames - 1]
  fractalFunction :: Int -> (Int, Int) -> Maybe Double
  fractalFunction frame (r, c) = case (fractalType, animation) of
     -- Mandelbrot animation functions
     (Mandelbrot, NoAnimation) ->
        pMandelbrot aa pixelSize iterations power $ pairToComplex (r, c)
     (Mandelbrot, Power _) ->
        pMandelbrot aa pixelSize iterations value $ pairToComplex (r, c)
     (Mandelbrot, Zoom _ _) ->
        pMandelbrot aa (pixelSize * value) (zoomIVals !! frame) power $ pairToComplexZ (r, c) value
     (Mandelbrot, Iterations _) ->
        pMandelbrot aa pixelSize (round value) power $ pairToComplex (r, c)
     (Mandelbrot, Theta _) ->
        error "Theta animations can only be generated for Julia fractals"
     (Mandelbrot, Magnitude _) ->
        error "Magnitude animations can only be generated for Julia fractals"
     -- Julia animation functions
     (Julia, NoAnimation) ->
        pJulia aa pixelSize iterations power cValue $ pairToComplex (r, c)
     (Julia, Power _ ) ->
        pJulia aa pixelSize iterations value cValue $ pairToComplex (r, c)
     (Julia, Zoom _ _) ->
        pJulia aa (pixelSize * value) (zoomIVals !! frame) power cValue $ pairToComplexZ (r, c) value
     (Julia, Iterations _) ->
        pJulia aa pixelSize (round value) power cValue $ pairToComplex (r, c)
     (Julia, Theta _) ->
        pJulia aa pixelSize iterations power (mkPolar cMag value) $ pairToComplex (r, c)
     (Julia, Magnitude _) ->
        pJulia aa pixelSize iterations power (mkPolar value cPhase) $ pairToComplex (r, c)
   where
    -- The value being animated
    value      = animVals !! frame
    -- Other values
    pixelSize = (2 * range) / (fromIntegral $ numColumns - 1)
    cMag      = magnitude cValue
    cPhase    = phase cValue
    halfV     = 0.5 * fromIntegral numRows
    halfH     = 0.5 * fromIntegral numColumns
    -- Function to get a complex point from a pixel coordinate pair
    pairToComplex (r, c) =
       (cReal - range + fromIntegral c * pixelSize)
          :+ (cImag + (pixelSize * (fromIntegral (numRows - 1) / 2)) - fromIntegral r * pixelSize)    
    -- Same as above but for zoom animations
    pairToComplexZ (r, c) zoomFactor =
       ((fromIntegral c - halfH) * pixelSize * zoomFactor + cReal)
          :+ ((fromIntegral r - halfV) * pixelSize * zoomFactor - cImag)

-- Helper function for interpolating from the start to the end of an animation's value range
interpolate :: Double -> Double -> Double -> Double
interpolate i f p = (i +) $ (* p) $ (f - i)

-- The text that gets printed if the user asks for help in their arguments
help :: String
help = 
 "** Mayge's Fractal Generator - Help **                                                           \n\
 \                                                                                                 \n\
 \By default, this program generates a fractal with the following attributes:                      \n"
 ++ pPrintOptions defaultOptions ++
 "                                                                                                 \n\
 \These defaults can be changed through a series of command line arguments:                        \n\
 \                                                                                                 \n\
 \   -fractal |mandelbrot             The type of fractal to be generated                          \n\
 \            |julia                                                                               \n\
 \                                                                                                 \n\
 \   -resolution horix vert           The resolution of your images                                \n\
 \                                                                                                 \n\
 \   -center |rectangular real imag   The complex center of your images                            \n\
 \           |polar mag phase                                                                      \n\
 \                                                                                                 \n\
 \   -range r                         The real distance from the center to the edge of your picture\n\
 \                                                                                                 \n\
 \   -iterations i                    The maximum number of times a point should be iterated       \n\
 \                                                                                                 \n\
 \   -power p                         The power used in the iteration equation                     \n\
 \                                                                                                 \n\
 \   -aa |enabled                     Whether subsampling antialiasing (9 subpixels) should be used\n\
 \       |disabled                                                                                 \n\
 \                                                                                                 \n\
 \   -normalization |linear           The method used to map iteration counts to [0,1] for coloring\n\
 \                  |sigmoid mid pwr                                                               \n\
 \                  |periodic period                                                               \n\
 \                  |sine period                                                                   \n\
 \                                                                                                 \n\
 \   -color |greyscale                The color scheme used to generate the images                 \n\
 \          |hue                                                                                   \n\
 \          |gradient [r,g,b]                                                                      \n\
 \          |gradient linear [r,g,b]                                                               \n\
 \                                                                                                 \n\
 \   -animation |none                 The animation that should be generated                       \n\
 \              |power fnl frms                                                                    \n\
 \              |zoom fnl frms fIter                                                               \n\
 \              |iterations fnl frms                                                               \n\
 \              |theta fnl frms                                                                    \n\
 \              |magnitude fnl frms                                                                \n\
 \                                                                                                 \n\
 \   -cvalue  |rectangular real imag  The point c used for ccreation of a Julia fractal            \n\
 \            |polar mag phase                                                                     \n\
 \                                                                                                 \n\
 \   -setcolor r,g,b                  The color used for points within the set                     \n\
 \                                                                                                 \n\
 \   -startingframe f                 The frame to begin on                                        \n\
 \                                                                                                 \n\
 \Only the first 3 characters (including dashes) of any argument are required.                     "