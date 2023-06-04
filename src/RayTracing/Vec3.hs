--------------------------------------------------------------------------------

module RayTracing.Vec3 (
    Vec3 (..),
    Point3,
    Color,
) where

--------------------------------------------------------------------------------

data Vec3 a = Vec3 a a a
    deriving (Eq, Show)

type Point3 = Vec3
type Color = Vec3

--------------------------------------------------------------------------------
