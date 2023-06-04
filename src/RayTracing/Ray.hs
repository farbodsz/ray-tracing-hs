--------------------------------------------------------------------------------

module RayTracing.Ray (
    Ray (..),
    rayAt,
) where

import RayTracing.Vec3 (Point3, vplus)

--------------------------------------------------------------------------------

data Ray = Ray
    { rayOrig :: Point3
    , rayDir :: Point3
    }
    deriving (Eq, Show)

rayAt :: Double -> Ray -> Point3
rayAt t Ray {..} = rayOrig `vplus` ((* t) <$> rayDir)

--------------------------------------------------------------------------------
