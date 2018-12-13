module Colors
   ( colorGrey
   , colorHue
   , colorSigmoidGrey
   ) where

import           Data.Complex
import qualified Graphics.Image                as I

mod' :: RealFloat a => a -> a -> a
mod' x y = (-) x . (*) y . fromIntegral . truncate $ x / y

colorGrey :: Double -> Maybe Double -> I.Pixel I.RGB Double
colorGrey _      Nothing  = I.PixelRGB 0 0 0
colorGrey period (Just x) = I.PixelRGB val val val
   where val = (1 - cos ((x `mod'` period) / period * 6.2831853)) / 2

colorHue :: Double -> Maybe Double -> I.Pixel I.RGB Double
colorHue _ Nothing                    = I.PixelRGB 0        0        0       
colorHue period (Just i) | x <= 1 / 6 = I.PixelRGB x'       0        1       
                         | x <= 1 / 3 = I.PixelRGB 1        0        (2 - x')
                         | x <= 1 / 2 = I.PixelRGB 1        (x' - 2) 0       
                         | x <= 2 / 3 = I.PixelRGB (4 - x') 1        0       
                         | x <= 5 / 6 = I.PixelRGB 0        1        (x' - 4)
                         | otherwise  = I.PixelRGB 0        (6 - x') 1       
 where
  x  = (i `mod'` period) / period
  x' = 6 * x

colorSigmoidGrey :: Double -> Double -> Maybe Double -> I.Pixel I.RGB Double
colorSigmoidGrey _     _        Nothing  = I.PixelRGB 0 0 0
colorSigmoidGrey power midpoint (Just x) = I.PixelRGB val val val
   where val = (1 /) $ (1 +) $ (power **) $ (midpoint - x) / (midpoint / 4)


-- colorGradient :: I.Pixel I.RGB Double -> [I.Pixel I.RGB Double] -> Double
--                -> Maybe Double -> I.Pixel I.RGB Double
-- colorGradient convergent _ _ Nothing = convergent
-- colorGradient _ 