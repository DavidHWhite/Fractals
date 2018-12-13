module Colors
   ( colorGrey
   , colorHue
   , normLinear
   , normSigmoid
   , normPeriodic
   , normSine
   )
where

import           Data.Complex
import qualified Graphics.Image                as I

mod' :: RealFloat a => a -> a -> a
mod' x y = (-) x . (*) y . fromIntegral . truncate $ x / y

-- Normalization Functions

normLinear :: Int -> Double -> Double
normLinear max = (/ fromIntegral max)

normSigmoid :: Double -> Double -> Double -> Double
normSigmoid center power = (1 /) . (1 +) . (power **) . (/ (center / 4)) . (center -)

normPeriodic :: Double -> Double -> Double
normPeriodic period = (/ period) . flip mod' period

normSine :: Double -> Double -> Double
normSine period = (/ 2) . (1 -) . cos . (* 6.2831853) . (/ period) . (flip mod' period)





colorGrey :: Maybe Double -> I.Pixel I.RGB Double
colorGrey Nothing    = I.PixelRGB 0 0 0
colorGrey (Just val) = I.PixelRGB val val val

colorHue :: Maybe Double -> I.Pixel I.RGB Double
colorHue Nothing              = I.PixelRGB 0        0        0       
colorHue (Just x) | x' <= 1   = I.PixelRGB x'       0        1       
                  | x' <= 2   = I.PixelRGB 1        0        (2 - x')
                  | x' <= 3   = I.PixelRGB 1        (x' - 2) 0       
                  | x' <= 4   = I.PixelRGB (4 - x') 1        0       
                  | x' <= 5   = I.PixelRGB 0        1        (x' - 4)
                  | otherwise = I.PixelRGB 0        (6 - x') 1       
 where
  x' = 6 * x


-- colorGradient :: I.Pixel I.RGB Double -> [I.Pixel I.RGB Double] -> Double
--                -> Maybe Double -> I.Pixel I.RGB Double
-- colorGradient convergent _ _ Nothing = convergent
-- colorGradient _ 