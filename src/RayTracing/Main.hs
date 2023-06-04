--------------------------------------------------------------------------------

module RayTracing.Main (main) where

import Control.Monad (forM_)
import Data.List.Extra (chunksOf)
import GHC.Float (int2Double)
import RayTracing.Color (translateColor)
import RayTracing.Vec3 (Color, Vec3 (Vec3))
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------
-- IO

main :: IO ()
main = outputImage testImage

outputImage :: PPM -> IO ()
outputImage ppm = do
    forM_ (zip idxs rowGroups) $ \(j, row) -> do
        hPutStrLn stderr $ "Scanlines remaining: " ++ show j
        outputRow row
    hPutStrLn stderr "Done."
  where
    outputRow = mapM_ putStrLn
    idxs = [length rowGroups - 1, length rowGroups - 2 .. 0]
    rowGroups = chunksOf (ppmRows ppm) (renderImage ppm)

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

type ImageBody = [[Color Int]]

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
    mkRow j = map (\i -> mkColorPxl (int2Double i) (int2Double j)) xs

    xs = [0 .. imgW - 1]
    ys = [imgH - 1, imgH - 2 .. 0]

    mkColorPxl i j =
        translateColor $
            Vec3
                (i / (int2Double imgW - 1))
                (j / (int2Double imgH - 1))
                0.25

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
    pxlToStr (Vec3 r g b) = unwords $ show <$> [r, g, b]

--------------------------------------------------------------------------------
