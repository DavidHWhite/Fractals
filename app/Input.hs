{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}

module Input
   ( Options
   , processArgs
   )   
where

import           Data.Complex
import           System.IO
import           Data.Word
import           System.Environment
import           Data.Char

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
   | Periodic Int
   | Sine Int
   deriving (Show)
data Color
   = Greyscale
   | Hue
   -- TODO | Gradient
   deriving (Show)
data Animation
   = Power Double Int Int
   | Zoom Double Int
   | Iterations Int Int
   | Theta Double Int
   | NoAnimation
   deriving (Show)

data Options = Options
   { fractalType :: FractalType
   , resolution :: (Int, Int)
   , center :: Complex Double
   , range :: Double
   , iterations :: Int
   , power :: Double
   , aa :: AA
   , normalization :: Normalization
   , color :: Color
   , animation :: Animation
   , cvalue :: Complex Double
   } deriving (Show)

defaultOptions =
   Options Mandelbrot (500, 500) (0 :+ 0) 2 500 2 AAEnabled Linear Hue NoAnimation (0 :+ 0)

main :: IO ()
main = do
   args <- fmap (separateArgs . map (take 3 . map toLower)) getArgs
   let options = foldl
          (\opt (arg : subargs) ->
             let func = lookup arg arguments
             in  case fmap (\f -> f (subargs, opt)) func of
                    Just x    -> x
                    otherwise -> error ""
          )
          defaultOptions
          args
   print options

processArgs :: [String] -> Options
processArgs = getOptionsFromArgs . separateArgs . normalizeArgs

normalizeArgs :: [String] -> [String]
normalizeArgs = map (take 3 . map toLower)

separateArgs :: [String] -> [[String]]
separateArgs []       = []
separateArgs (x : xs) = case lookup x arguments of
   Nothing -> error "Invalid arguments"
   otherwise ->
      let (subArgs, args) = span
             (\x -> case lookup x arguments of
                Nothing   -> True
                otherwise -> False
             )
             xs
      in  (x : subArgs) : separateArgs args

getOptionsFromArgs :: [[String]] -> Options
getOptionsFromArgs = foldl
      (\opt (arg : subargs) ->
         let func = lookup arg arguments
         in  case fmap (\f -> f (subargs, opt)) func of
               Just x    -> x
               otherwise -> error ""
      )
      defaultOptions

-- Array of all command line arguments and functions to interpret their subarguments
-- It is assumed that arguments have been converted to lowercase and shortened to 3 characters
arguments :: [(String, (([String], Options) -> Options))]
arguments =
   [ ( "-fr"
     , \case
        ("man" : [], opt) -> opt { fractalType = Mandelbrot }
        ("jul" : [], opt) -> opt { fractalType = Julia }
        otherwise         -> error "Invalid arguments for -fractalType"
     )
   , ( "-re"
     , \case
        (r : c : [], opt) -> opt { resolution = (read r, read c) }
        otherwise         -> error "Invalid arguments for -resolution"
     )
   , ( "-ce"
     , \case
        ("rec" : r : i : [], opt) -> opt { center = read r :+ read i }
        ("pol" : m : p : [], opt) -> opt { center = mkPolar (read m) (read p) }
        otherwise                 -> error "Invalid arguments for -center"
     )
   , ( "-ra"
     , \case
        (n : [], opt) -> opt { range = read n }
        otherwise     -> error "Invalid arguments for -range"
     )
   , ( "-it"
     , \case
        (n : [], opt) -> opt { iterations = read n }
        otherwise     -> error "Invalid arguments for -iterations"
     )
   , ( "-po"
     , \case
        (n : [], opt) -> opt { power = read n }
        otherwise     -> error "Invalid arguments for -power"
     )
   , ( "-aa"
     , \case
        ("ena" : [], opt) -> opt { aa = AAEnabled }
        ("dis" : [], opt) -> opt { aa = AADisabled }
        otherwise         -> error "Invalid arguments for -aa"
     )
   , ( "-no"
     , \case
        ("lin"         : [], opt) -> opt { normalization = Linear }
        ("sig"     : c : [], opt) -> opt { normalization = Sigmoid (read c) 2.5 }
        ("sig" : c : p : [], opt) -> opt { normalization = Sigmoid (read c) (read p) }
        ("per"     : p : [], opt) -> opt { normalization = Periodic (read p) }
        ("sin"     : p : [], opt) -> opt { normalization = Sine (read p) }
        otherwise                 -> error "Invalid arguments for -normalization"
     )
   , ( "-co"
     , \case
        ("gre" : [], opt) -> opt { color = Greyscale }
        ("hue" : [], opt) -> opt { color = Hue }
        otherwise         -> error "Invalid arguments for -color"
     )
   , ( "-an"
     , \case
        ("pow" : fn : fr : fi : [], opt) -> opt { animation = Power (read fn) (read fr) (read fi) }
        ("zoo"      : fn : fr : [], opt) -> opt { animation = Zoom (read fn) (read fr) }
        ("ite"      : fn : fr : [], opt) -> opt { animation = Iterations (read fn) (read fr) }
        ("the"      : fn : fr : [], opt) -> opt { animation = Theta (read fn) (read fr) }
        ("non"                : [], opt) -> opt { animation = NoAnimation }
        otherwise                        -> error "Invalid arguments for -animation"
     )
   , ( "-cv"
     , \case
        ("rec" : r : i : [], opt) -> opt { cvalue = read r :+ read i }
        ("pol" : m : p : [], opt) -> opt { cvalue = mkPolar (read m) (read p) }
        otherwise                 -> error "Invalid arguments for -cvalue"
     )
   ]
