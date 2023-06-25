----------------------------------------------------------------------------------------------------

module RayTracing.Main (main) where

import Control.Monad (forM_)
import Data.List.Extra (chunksOf)
import GHC.Float (double2Int, int2Double)
import RayTracing.Color (blue, linearBlend, white, writeColor)
import RayTracing.Ray (Ray (..))
import RayTracing.Vec3 (
    Color,
    Pixel (..),
    Point3,
    Vec3 (Vec3),
    vempty,
    vminus,
    vnegate,
    vplus,
    vscale,
 )
import System.IO (hPutStrLn, stderr)

----------------------------------------------------------------------------------------------------
-- IO

main :: IO ()
main = outputImage testImage

outputImage :: Image -> IO ()
outputImage ppm = do
    forM_ (zip idxs rowGroups) $ \(j, row) -> do
        hPutStrLn stderr $ "Scanlines remaining: " ++ show j
        outputRow row
    hPutStrLn stderr "Done."
  where
    outputRow = mapM_ putStrLn
    idxs = [length rowGroups - 1, length rowGroups - 2 .. 0]
    rowGroups = chunksOf (ppmRows ppm) (renderImage ppm)

----------------------------------------------------------------------------------------------------
-- Types

-- | PPM image (portable Pix Map has an image header and body).
data Image = PPM
    { ppmFormat :: String
    , ppmCols :: Int
    , ppmRows :: Int
    , ppmMaxColorVal :: Int
    , ppmBody :: ImageBody
    }

type ImageBody = [[Pixel]]

----------------------------------------------------------------------------------------------------
-- Image, camera, dimensions

aspectRatio :: Double
aspectRatio = 16 / 9

imgW :: Int
imgW = 400

imgH :: Int
imgH = double2Int $ int2Double imgW / aspectRatio

-- Camera

viewportH :: Double
viewportH = 2

viewportW :: Double
viewportW = aspectRatio * viewportH

focalLength :: Double
focalLength = 1

--

origin :: Point3
origin = vempty

horizontal :: Point3
horizontal = Vec3 viewportW 0 0

vertical :: Point3
vertical = Vec3 0 viewportH 0

lowerLeftCorner :: Point3
lowerLeftCorner =
    foldl1 vminus [origin, half horizontal, half vertical, Vec3 0 0 focalLength]
  where
    half = fmap (/ 2)

----------------------------------------------------------------------------------------------------
-- Image

testImage :: Image
testImage =
    PPM
        { ppmFormat = "P3"
        , ppmCols = imgW
        , ppmRows = imgH
        , ppmMaxColorVal = 255
        , ppmBody = testImageBody
        }

testImageBody :: ImageBody
testImageBody = mkRow <$> [imgH - 1, imgH - 2 .. 0]

mkRow :: Int -> [Pixel]
mkRow j = flip mkColorPxl j <$> [0 .. imgW - 1]

mkColorPxl :: Int -> Int -> Pixel
mkColorPxl i j = writeColor . rayColor $ Ray origin rayDir
  where
    rayDir = foldr1 vplus [lowerLeftCorner, vscale u horizontal, vscale v vertical, vnegate origin]
    u = int2Double i / int2Double (imgW - 1)
    v = int2Double j / int2Double (imgH - 1)

rayColor :: Ray -> Color Double
rayColor = linearBlend white blue

----------------------------------------------------------------------------------------------------
-- Rendering

renderImage :: Image -> [String]
renderImage ppm = renderHeaders ppm ++ renderImageBody (ppmBody ppm)
  where
    renderHeaders PPM {..} = [ppmFormat, show ppmCols ++ " " ++ show ppmRows, show ppmMaxColorVal]

renderImageBody :: ImageBody -> [String]
renderImageBody = concatMap (map pxlToStr)
  where
    pxlToStr (Pixel (Vec3 r g b)) = unwords $ show <$> [r, g, b]

--------------------------------------------------------------------------------
