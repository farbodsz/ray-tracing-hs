--------------------------------------------------------------------------------

module Lib (main) where

import Control.Monad (forM_)
import GHC.Float (double2Int, int2Double)

--------------------------------------------------------------------------------

-- Entry point
main :: IO ()
main = renderImage testImage

debugImg :: Image -> IO ()
debugImg img = mapM_ putStrLn output
  where
    output = concatMap (map pxlToStr) img
    pxlToStr (Pixel r g b) = unwords $ show <$> [r, g, b]

--------------------------------------------------------------------------------

-- Image
imgW, imgH :: Int
imgW = 256
imgH = 256

testImage :: Image
testImage = map mkRow ys
  where
    mkRow j = map (\i -> calcPixel (int2Double i) (int2Double j)) xs

    xs = [0 .. imgW - 1]
    ys = [imgH - 1, imgH - 2 .. 0]

    calcPixel i j =
        mkPixel
            (i / (int2Double imgW - 1))
            (j / (int2Double imgH - 1))
            0.25

--------------------------------------------------------------------------------

-- | RGB in range [0, 255]
data Pixel = Pixel
    { pxlR :: Int
    , pxlG :: Int
    , pxlB :: Int
    }
    deriving (Eq, Show)

mkPixel :: Double -> Double -> Double -> Pixel
mkPixel r g b = Pixel (to256 r) (to256 g) (to256 b)
  where
    to256 x = double2Int $ x * 255.999

type Image = [[Pixel]]

-- Render
renderImage :: Image -> IO ()
renderImage img = do
    putStr $ concat ["P3\n", show imgW, " ", show imgH, "\n255\n"]
    forM_ img $ \row ->
        forM_ row $ \p -> putStrLn . unwords $ show <$> [pxlR p, pxlG p, pxlB p]

--------------------------------------------------------------------------------
