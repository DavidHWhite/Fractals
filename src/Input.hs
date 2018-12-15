{-# LANGUAGE LambdaCase #-}

module Input
   ( Options(..)
   , FractalType(..)
   , AA(..)
   , Normalization(..)
   , Color(..)
   , Animation(..)
   , processArgs
   , pPrintOptions
   , defaultOptions
   )
where

import           Data.Complex
import           System.IO
import           Data.Word
import           System.Environment
import           Data.Char
import qualified Graphics.Image                as I

data FractalType
   = Mandelbrot
   | Julia
   deriving (Show)
data AA
   = AAEnabled
   | AADisabled
   deriving (Show)
data Normalization
   = Linear
   | Sigmoid Double Double
   | Periodic Double
   | Sine Double
   deriving (Show)
data Color
   = Greyscale
   | Hue
   | Gradient [I.Pixel I.RGB Double]
   deriving (Show)
data Animation
   = NoAnimation
   | Power Double
   | Zoom Double Int
   | Iterations Int
   | Theta Double
   deriving (Show)

data Options = Options
   { fractalType     :: FractalType
   , resolution      :: (Int, Int)
   , center          :: Complex Double
   , range           :: Double
   , iterations      :: Int
   , power           :: Double
   , aa              :: AA
   , normalization   :: Normalization
   , color           :: Color
   , animation       :: Animation
   , cvalue          :: Complex Double
   , setColor        :: I.Pixel I.RGB Double
   , frameCount      :: Int
   , startingFrame   :: Int
   } deriving (Show)

defaultOptions = Options Mandelbrot
                         (500, 500)
                         (0 :+ 0)
                         2
                         500
                         2
                         AAEnabled
                         Linear
                         Hue
                         NoAnimation
                         (0 :+ 0)
                         (I.PixelRGB 0 0 0)
                         100
                         1

processArgs :: [String] -> Options
processArgs = getOptionsFromArgs . separateArgs . normalizeArgs

normalizeArgs :: [String] -> [String]
normalizeArgs = map $ map toLower

separateArgs :: [String] -> [[String]]
separateArgs []       = []
separateArgs (x : xs) = case lookup (take 3 x) arguments of
   Nothing -> error $ "Invalid argument \"" ++ x ++ "\""
   otherwise ->
      let (subArgs, args) = span
             (\y -> case lookup (take 3 y) arguments of
                Nothing   -> True
                otherwise -> False
             )
             xs
      in  (x : subArgs) : separateArgs args

getOptionsFromArgs :: [[String]] -> Options
getOptionsFromArgs = foldl
   (\opt (arg : subargs) ->
      let func = lookup (take 3 arg) arguments
      in  case fmap (\f -> f (subargs, opt)) func of
             Just x    -> x
             otherwise -> error ""
   )
   defaultOptions

colorFromString :: String -> I.Pixel I.RGB Double
colorFromString s =
   case
         map ((/ 255) . read) . words $ foldr
            (\char acc -> (if char == ',' then ' ' else char) : acc)
            []
            s
      of
         (r : g : b : []) -> if r <= 1 && g <= 1 && b <= 1
            then I.PixelRGB r g b
            else error "All color values must be on the interval [0,255]"
         otherwise -> error "Invalid color for gradient"

-- Array of all command line arguments and functions to interpret their subarguments
-- It is assumed that arguments have been converted to lowercase and shortened to 3 characters
arguments :: [(String, (([String], Options) -> Options))]
arguments
   = [ ( "-fr"
       , \(x : xs, opt) -> case (take 3 x) : xs of
          ("man" : []) -> opt { fractalType = Mandelbrot }
          ("jul" : []) -> opt { fractalType = Julia }
          otherwise -> error $ "Invalid arguments for -fractalType: \"" ++ (show (x : xs)) ++ "\""
       )
     , ( "-re"
       , \case
          (r : c : [], opt) -> opt { resolution = (read r, read c) }
          x                 -> error $ "Invalid arguments for -resolution: \"" ++ (show x) ++ "\""
       )
     , ( "-ce"
       , \(x : xs, opt) -> case (take 3 x) : xs of
          ("rec" : r : i : []) -> opt { center = read r :+ read i }
          ("pol" : m : p : []) -> opt { center = mkPolar (read m) (read p) }
          otherwise -> error $ "Invalid arguments for -center: \"" ++ (show (x : xs)) ++ "\""
       )
     , ( "-ra"
       , \case
          (n : [], opt) -> opt { range = read n }
          x             -> error $ "Invalid arguments for -range: \"" ++ (show x) ++ "\""
       )
     , ( "-it"
       , \case
          (n : [], opt) -> opt { iterations = read n }
          x             -> error $ "Invalid arguments for -iterations: \"" ++ (show x) ++ "\""
       )
     , ( "-po"
       , \case
          (n : [], opt) -> opt { power = read n }
          x             -> error $ "Invalid arguments for -power: \"" ++ (show x) ++ "\""
       )
     , ( "-aa"
       , \(x : xs, opt) -> case (take 3 x) : xs of
          ("ena" : []) -> opt { aa = AAEnabled }
          ("dis" : []) -> opt { aa = AADisabled }
          otherwise    -> error $ "Invalid arguments for -aa: \"" ++ (show (x : xs)) ++ "\""
       )
     , ( "-no"
       , \(x : xs, opt) -> case (take 3 x) : xs of
          ("lin"         : []) -> opt { normalization = Linear }
          ("sig"     : c : []) -> opt { normalization = Sigmoid (read c) 2.5 }
          ("sig" : c : p : []) -> opt { normalization = Sigmoid (read c) (read p) }
          ("per"     : p : []) -> opt { normalization = Periodic (read p) }
          ("sin"     : p : []) -> opt { normalization = Sine (read p) }
          otherwise ->
             error $ "Invalid arguments for -normalization: \"" ++ (show (x : xs)) ++ "\""
       )
     , ( "-co"
       , \(x : xs, opt) -> case (take 3 x) : xs of
          ("gre" : []  ) -> opt { color = Greyscale }
          ("hue" : []  ) -> opt { color = Hue }
          ("gra" : args) -> if length args < 2
             then error "Insufficient colors for a gradient to be formed"
             else opt { color = (Gradient $ map colorFromString args) }
          otherwise -> error $ "Invalid arguments for -color: \"" ++ (show (x : xs)) ++ "\""
       )
     , ( "-an"
       , \(x : xs, opt) -> case (take 3 x) : xs of
          ("pow" : fn : fr : []) -> opt { animation = Power (read fn), frameCount = (read fr) }
          ("zoo" : fn : fr : fi : []) ->
             opt { animation = Zoom (read fn) (read fi), frameCount = (read fr) }
          ("ite" : fn : fr : []) -> opt { animation = Iterations (read fn), frameCount = (read fr) }
          ("the" : fn : fr : []) -> opt { animation = Theta (read fn), frameCount = (read fr) }
          ("non"           : []) -> opt { animation = NoAnimation }
          otherwise -> error $ "Invalid arguments for -animation: \"" ++ (show (x : xs)) ++ "\""
       )
     , ( "-cv"
       , \(x : xs, opt) -> case (take 3 x) : xs of
          ("rec" : r : i : []) -> opt { cvalue = read r :+ read i }
          ("pol" : m : p : []) -> opt { cvalue = mkPolar (read m) ((/ 180) . (* pi) $ read p) }
          otherwise -> error $ "Invalid arguments for -cvalue: \"" ++ (show (x : xs)) ++ "\""
       )
     , ( "-se"
       , \case
          ([x], opt) -> opt { setColor = colorFromString x }
          x          -> error $ "Invalid arguments for -setColor: \"" ++ (show x) ++ "\""
       )
     , ( "-st"
       , \case
          ([x], opt) -> opt { startingFrame = read x }
          x          -> error $ "Invalid arguments for -startingFrame: \"" ++ (show x) ++ "\""
       )
     ]

pPrintOptions :: Options -> String
pPrintOptions options =
   "Fractal Type:    " ++ (show $ fractalType   options) ++ '\n' :
   "Resolution:      " ++ (show $ resolution    options) ++ '\n' :
   "Center:          " ++ (show $ center        options) ++ '\n' :
   "Range:           " ++ (show $ range         options) ++ '\n' :
   "Iterations:      " ++ (show $ iterations    options) ++ '\n' :
   "Power:           " ++ (show $ power         options) ++ '\n' :
   "Antialiasing:    " ++ (show $ aa            options) ++ '\n' :
   "Normalization:   " ++ (show $ normalization options) ++ '\n' :
   "Color:           " ++ (show $ color         options) ++ '\n' :
   "Animation:       " ++ (show $ animation     options) ++ '\n' :
   "C-Value:         " ++ (show $ cvalue        options) ++ '\n' :
   "Set Color:       " ++ (show $ setColor      options) ++ '\n' :
   "Frames:          " ++ (show $ frameCount    options) ++ '\n' :
   "Starting frame:  " ++ (show $ startingFrame options)
