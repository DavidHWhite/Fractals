{-# LANGUAGE BangPatterns, LambdaCase #-}

module Main where

import           Help
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
import           Data.Time

-- Entry point for program
main :: IO ()
main = do
   args <- getArgs
   if not (length args == 0) &&
      (  head args == "h"
      || head args == "-h"
      || head args == "help"
      || head args == "-help"
      || head args == "?"
      || head args == "-?"
      )
     then do
        putStrLn $ help args
        hFlush stdout
     else do
        let options = processArgs args
        putStrLn $ pPrintOptions options
        hFlush stdout
        fractal args options

fractal :: [String] -> Options -> IO ()
fractal args options@(Options fractalType (numRows, numColumns) (centerReal :+ centerImag)
                      range iterations power aaIn normalization color animation
                      cValue setColor framesIn startingFrame)
   = do
      currentDir <- getCurrentDirectory
      {- Path is generated by finding the current date/time, replacing colons (an invalid character in
       Windows paths) with an alternate Unicode colon, and removing the last 12 digits (which contain
       the fractional part of the seconds and the "UTC" code) -}
      path  <- fmap
         ( ((currentDir ++ "\\Generated Fractals\\") ++)
         . reverse . drop 12 . reverse
         . map (\case ':' -> '\xA789'; x -> x)
         . show
         )
         getCurrentTime
      createDirectoryIfMissing True path
      setCurrentDirectory path
      writeFile "data.txt" $ (pPrintOptions options) ++ '\n' : "Arguments:       " ++ (unwords args)
      putStr $ (show $ startingFrame - 1) ++ '/' : (show $ floor frames)
      hFlush stdout
      mapM_
         (\frame -> do
            I.writeImageExact (I.PNG) []
                  (path ++ '\\' : (take ((countDigits $ floor frames) - (countDigits $ frame)) $ repeat '0')
                   ++ show (frame) ++ ".png") $
                  I.toManifest $ I.toWord8I $ I.makeImageR I.RPS
                                       (numRows, numColumns)
                                       (\point -> colorFunc $ fractalFunction frame point)
            putStr $ '\r' : (show $ frame + 1) ++ '/' : (show $ floor frames)
            hFlush stdout
         )
         [startingFrame - 1 .. (truncate frames) - 1]
      putStrLn ""
 where
   frames = case animation of
      NoAnimation -> 1
      otherwise   -> fromIntegral framesIn
   colorFunc =
      case color of
            Greyscale                  -> colorGrey setColor
            Hue                        -> colorHue setColor
            Gradient isCircular colors -> colorGrad setColor isCircular colors
         . fmap
            (case normalization of
               Linear               -> normLinear iterations
               Sigmoid center power -> normSigmoid center power
               Periodic period      -> normPeriodic period
               Sine     period      -> normSine period
            )
   aa = case aaIn of
      AAEnabled  -> True
      AADisabled -> False
   animVals = map
      (case animation of
         Zoom final _ -> ((exp $ (log (final / range)) / (frames - 1)) **)
         Grid rIni rFnl _ _ rNum _ ->
            interpolate rIni rFnl . (/ (fromIntegral rNum - 1)) . fromIntegral . (`mod` rNum) . floor
         otherwise ->
            case animation of
                NoAnimation      -> \x -> 1
                Power final      -> interpolate power final
                Iterations final -> interpolate (fromIntegral iterations) (fromIntegral final)
                Theta final _    -> interpolate (phase cValue) (final * pi / 180)
                LinearC final    -> interpolate (realPart cValue) (realPart final)
            . if frames /= 1 then (/ (frames - 1)) else id
      )
      [fromIntegral startingFrame - 1 .. frames - 1]
   altAnimVals = map
      (case animation of
         Grid _ _ iIni iFnl rNum iNum ->
            interpolate iIni iFnl . (/ (fromIntegral iNum - 1)) . fromIntegral . (`div` rNum) . floor
         otherwise ->
            case animation of
                Zoom _ (Just finalIter) -> interpolate (fromIntegral iterations) (fromIntegral finalIter)
                Zoom _ Nothing          -> \x -> fromIntegral iterations
                Theta _ (Just final)    -> interpolate (magnitude cValue) final
                Theta _ Nothing         -> \x -> magnitude cValue
                LinearC final           -> interpolate (imagPart cValue) (imagPart final)
            . if frames /= 1 then (/ (frames - 1)) else id
      )
      [fromIntegral startingFrame - 1 .. frames - 1]
   fractalFunction :: Int -> (Int, Int) -> Maybe Double
   fractalFunction frame (r, c) = case (fractalType, animation) of
      -- Mandelbrot animation functions
      (Mandelbrot, NoAnimation) ->
         pMandelbrot aa pixelSize iterations power $ pairToComplex (r, c)
      (Mandelbrot, Power _) ->
         pMandelbrot aa pixelSize iterations value $ pairToComplex (r, c)
      (Mandelbrot, Zoom _ _) ->
         pMandelbrot aa (pixelSize * value) (round $ altValue) power $ pairToComplexZ (r, c) value
      (Mandelbrot, Iterations _) ->
         pMandelbrot aa pixelSize (round value) power $ pairToComplex (r, c)
      (Mandelbrot, Theta _ _) ->
         error "Theta animations can only be generated for Julia fractals"
      (Mandelbrot, LinearC _) ->
         error "Linear C-Value animations can only be generated for Julia fractals"
      (Mandelbrot, Grid _ _ _ _ _ _) ->
         error "Grided c-values can only be generated for Julia fractals"
      -- Julia animation functions
      (Julia, NoAnimation) ->
         pJulia aa pixelSize iterations power cValue $ pairToComplex (r, c)
      (Julia, Power _) ->
         pJulia aa pixelSize iterations value cValue $ pairToComplex (r, c)
      (Julia, Zoom _ _) ->
         pJulia aa (pixelSize * value) (round $ altValue) power cValue $ pairToComplexZ (r, c) value
      (Julia, Iterations _) ->
         pJulia aa pixelSize (round value) power cValue $ pairToComplex (r, c)
      (Julia, Theta _ _) ->
         pJulia aa pixelSize iterations power (mkPolar altValue value) $ pairToComplex (r, c)
      (Julia, LinearC _) ->
         pJulia aa pixelSize iterations power (value :+ altValue) $ pairToComplex (r, c)
      (Julia, Grid _ _ _ _ _ _) ->
         pJulia aa pixelSize iterations power (value :+ altValue) $ pairToComplex (r, c)
      where
         -- The value being animated
         value     = animVals !! frame
         altValue  = altAnimVals !! frame
         -- Other values
         pixelSize = (2 * range) / (fromIntegral $ numColumns - 1)
         halfV     = 0.5 * fromIntegral numRows
         halfH     = 0.5 * fromIntegral numColumns
         -- Function to get a complex point from a pixel coordinate pair
         pairToComplex (r, c) =
            (centerReal - range + fromIntegral c * pixelSize)
               :+ (centerImag + (pixelSize * (fromIntegral (numRows - 1) / 2)) - fromIntegral r * pixelSize)
         -- Same as above but for zoom animations
         pairToComplexZ (r, c) zoomFactor =
            ((fromIntegral c - halfH) * pixelSize * zoomFactor + centerReal)
               :+ ((fromIntegral r - halfV) * pixelSize * zoomFactor - centerImag)

-- Helper function for interpolating from the start to the end of an animation's value range
interpolate :: RealFloat a => a -> a -> a -> a
interpolate i f = (i +) . (*) (f - i)

-- Helper funtion to count the number of digits in an integer when expressed in base 10
countDigits :: Integral a => a -> a
countDigits = (+ 1) . floor . logBase 10 . fromIntegral
