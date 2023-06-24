--------------------------------------------------------------------------------

module RayTracing.Color (
    writeColor,
    linearBlend,
    white,
    blue,
) where

import GHC.Float (double2Int)
import RayTracing.Ray (Ray (..))
import RayTracing.Vec3 (
    Color,
    Pixel (..),
    Vec3 (..),
    unitVector,
    vplus,
    vscale,
 )

--------------------------------------------------------------------------------

-- | Translates each component in range [0, 1] to [0, 255].
writeColor :: Color Double -> Pixel
writeColor (Vec3 r g b) = Pixel $ Vec3 (to256 r) (to256 g) (to256 b)
  where
    to256 x = double2Int $ x * 255.999

-- | Linearly blends white and blue depending on the height of the $y$
-- coordinate, after scaling the ray direction.
--
-- Essentially:
--    blendedValue = (1 - t) * startValue  +  t * endValue
linearBlend :: Color Double -> Color Double -> Ray -> Color Double
linearBlend startValue endValue r =
    let
        (Vec3 _ unitDirY _) = unitVector (rayDir r)
        t = 0.5 * (unitDirY + 1)
     in
        vscale (1 - t) startValue `vplus` vscale t endValue

white :: Color Double
white = Vec3 1 1 1

blue :: Color Double
blue = Vec3 0.5 0.7 1.0

--------------------------------------------------------------------------------
