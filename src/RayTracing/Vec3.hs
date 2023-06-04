--------------------------------------------------------------------------------

module RayTracing.Vec3 (
    Vec3 (..),

    -- * Arithmetic operations
    vplus,
    vminus,
    vproduct,
    unitVector,

    -- * Aliases
    Point3,
    Color,
) where

--------------------------------------------------------------------------------

data Vec3 a = Vec3 a a a
    deriving (Eq, Show)

vplus :: Num a => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 p q r) `vplus` (Vec3 x y z) = Vec3 (p + x) (q + y) (r + z)

vminus :: Num a => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 p q r) `vminus` (Vec3 x y z) = Vec3 (p - x) (q - y) (r - z)

vproduct :: Num a => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 p q r) `vproduct` (Vec3 x y z) = Vec3 (p * x) (q * y) (r * z)

instance Functor Vec3 where
    fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

--------------------------------------------------------------------------------

unitVector :: a -> Vec3 a
unitVector x = Vec3 x x x

--------------------------------------------------------------------------------

type Point3 = Vec3 Double
type Color = Vec3

--------------------------------------------------------------------------------
