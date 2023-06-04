--------------------------------------------------------------------------------

module Lib (main) where

import Control.Monad (forM_)
import GHC.Float (double2Int, int2Double)
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------
-- IO

main :: IO ()
main = outputImage testImage

outputImage :: PPM -> IO ()
outputImage ppm = do
    forM_ (zip [length rows - 1, length rows - 2 .. 0] rows) $ \(j, row) -> do
        hPutStrLn stderr $ "Scanlines remaining: " ++ show j
        putStrLn row
    hPutStrLn stderr "Done."
  where
    rows = renderImage ppm

--------------------------------------------------------------------------------
-- Types

-- | PPM image (portable Pix Map has an image header and body).
data PPM = PPM
    { ppmFormat :: String
    , ppmCols :: Int
    , ppmRows :: Int
    , ppmMaxColorVal :: Int
    , ppmBody :: ImageBody
    }

type ImageBody = [[Pixel]]

-- | For now, this will be RGB in range [0, 255]
data Pixel = Pixel
    { pxlR :: Int
    , pxlG :: Int
    , pxlB :: Int
    }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Test image

imgW, imgH :: Int
imgW = 256
imgH = 256

testImage :: PPM
testImage =
    PPM
        { ppmFormat = "P3"
        , ppmCols = imgW
        , ppmRows = imgH
        , ppmMaxColorVal = 255
        , ppmBody = testImageBody
        }

testImageBody :: ImageBody
testImageBody = map mkRow ys
  where
    mkRow j = map (\i -> calcPixel (int2Double i) (int2Double j)) xs

    xs = [0 .. imgW - 1]
    ys = [imgH - 1, imgH - 2 .. 0]

    calcPixel i j =
        mkPixel
            (i / (int2Double imgW - 1))
            (j / (int2Double imgH - 1))
            0.25

mkPixel :: Double -> Double -> Double -> Pixel
mkPixel r g b = Pixel (to256 r) (to256 g) (to256 b)
  where
    to256 x = double2Int $ x * 255.999

--------------------------------------------------------------------------------
-- Rendering

renderImage :: PPM -> [String]
renderImage ppm = renderHeaders ppm ++ renderImageBody (ppmBody ppm)
  where
    renderHeaders PPM {..} =
        [ ppmFormat
        , show ppmCols ++ " " ++ show ppmRows
        , show ppmMaxColorVal
        ]

renderImageBody :: ImageBody -> [String]
renderImageBody = concatMap (map pxlToStr)
  where
    pxlToStr (Pixel r g b) = unwords $ show <$> [r, g, b]

--------------------------------------------------------------------------------
