module Colors
   (
   -- * Normalization functions
     normLinear
   , normSigmoid
   , normPeriodic
   , normSine
   -- * Colorization functions
   , colorGrey
   , colorHue
   , colorGrad
   -- , mod'
   )
where

import           Data.Complex
import qualified Graphics.Image                as I

mod' :: RealFloat a => a -> a -> a
mod' x y = (-) x . (*) y . fromIntegral . truncate $ x / y

normLinear :: Int -> Double -> Double
normLinear max = (/ fromIntegral max)

normSigmoid :: Double -> Double -> Double -> Double
normSigmoid center power = (1 /) . (1 +) . (power **) . (/ (center / 4)) . (center -)

normPeriodic :: Double -> Double -> Double
normPeriodic period = (/ period) . flip mod' period

normSine :: Double -> Double -> Double
normSine period = (/ 2) . (1 -) . cos . (* 6.2831853) . (/ period) . (flip mod' period)

colorGrey :: I.Pixel I.RGB Double -> Maybe Double -> I.Pixel I.RGB Double
colorGrey setColor Nothing    = setColor
colorGrey _        (Just val) = I.PixelRGB val val val

colorGrad :: I.Pixel I.RGB Double -> [I.Pixel I.RGB Double] -> Maybe Double -> I.Pixel I.RGB Double
colorGrad convergentC _      Nothing  = convergentC
colorGrad _           colors (Just x) = interColors (colors !! section) (colors !! section + 1)
 where
  count   = fromIntegral $ (subtract 1) $ length colors
  section = truncate $ x * count
  p       = (* count) $ (x `mod'`) $ 1 / count
  inter i f = (i +) $ (* p) $ (f - i)
  interColors (I.PixelRGB iR iG iB) (I.PixelRGB fR fG fB) =
     I.PixelRGB (inter iR fR) (inter iG fG) (inter iB fB)

colorHue :: I.Pixel I.RGB Double -> Maybe Double -> I.Pixel I.RGB Double
colorHue setColor Nothing = setColor
colorHue _ (Just x) | x' <= 1   = I.PixelRGB x' 0 1
                    | x' <= 2   = I.PixelRGB 1 0 (2 - x')
                    | x' <= 3   = I.PixelRGB 1 (x' - 2) 0
                    | x' <= 4   = I.PixelRGB (4 - x') 1 0
                    | x' <= 5   = I.PixelRGB 0 1 (x' - 4)
                    | otherwise = I.PixelRGB 0 (6 - x') 1
   where x' = 6 * x
