--------------------------------------------------------------------------------

module RayTracing.Color where

import GHC.Float (double2Int)
import RayTracing.Vec3 (
    Color,
    Vec3 (Vec3),
 )

--------------------------------------------------------------------------------

-- | Translates each component in range [0, 1] to [0, 255].
translateColor :: Color Double -> Color Int
translateColor (Vec3 r g b) = Vec3 (to256 r) (to256 g) (to256 b)
  where
    to256 x = double2Int $ x * 255.999

--------------------------------------------------------------------------------
