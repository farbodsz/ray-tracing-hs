--------------------------------------------------------------------------------

module RayTracing.Vec3 (
    Vec3 (..),

    -- * Arithmetic operations
    vplus,
    vminus,
    vproduct,
    vdot,
    (<+>),
    (<#>),
    (<.>),

    -- * Vector properties
    vempty,
    unitVector,
    vlength,
    vlengthSquared,

    -- * Utilities
    vnegate,
    vscale,

    -- * Aliases
    Point3,
    Color,
    Pixel (..),
) where

--------------------------------------------------------------------------------

data Vec3 a = Vec3 a a a
    deriving (Eq, Show)

vempty :: Num a => Vec3 a
vempty = Vec3 0 0 0

--------------------------------------------------------------------------------

vplus :: Num a => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 p q r) `vplus` (Vec3 x y z) = Vec3 (p + x) (q + y) (r + z)

vminus :: Num a => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 p q r) `vminus` (Vec3 x y z) = Vec3 (p - x) (q - y) (r - z)

vproduct :: Num a => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 p q r) `vproduct` (Vec3 x y z) = Vec3 (p * x) (q * y) (r * z)

vdot :: Num a => Vec3 a -> Vec3 a -> a
(Vec3 p q r) `vdot` (Vec3 x y z) = (p * x) + (q * y) + (r * z)

vscale :: Num a => a -> Vec3 a -> Vec3 a
vscale t = fmap (* t)

vnegate :: Num a => Vec3 a -> Vec3 a
vnegate = fmap negate

(<+>) :: Num a => Vec3 a -> Vec3 a -> Vec3 a
(<+>) = vplus

(<#>) :: Num a => Vec3 a -> Vec3 a -> Vec3 a
(<#>) = vproduct

(<.>) :: Num a => Vec3 a -> Vec3 a -> a
(<.>) = vdot

--------------------------------------------------------------------------------

vlength :: Floating a => Vec3 a -> a
vlength v = sqrt $ vlengthSquared v

vlengthSquared :: Num a => Vec3 a -> a
vlengthSquared (Vec3 x y z) = x * x + y * y + z * z

unitVector :: Floating a => Vec3 a -> Vec3 a
unitVector vec = (/ vlength vec) <$> vec

instance Functor Vec3 where
    fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

--------------------------------------------------------------------------------

-- | 3D point.
type Point3 = Vec3 Double

-- | RGB color.
type Color = Vec3

-- | RGB pixel value with color from (0, 1)
newtype Pixel = Pixel {unPixel :: Color Int}
    deriving (Eq, Show)

--------------------------------------------------------------------------------
