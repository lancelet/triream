{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Proto where

import           Linear        (V2 (V2), V3 (V3), V4 (V4))
import           Linear.Affine (Point (P))
import           Linear.Matrix (M44)

data Tri v a
  = Tri
    { triP1 :: Point v a
    , triP2 :: Point v a
    , triP3 :: Point v a
    }

data Quad v a
  = Quad
    { quadP1 :: Point v a
    , quadP2 :: Point v a
    , quadP3 :: Point v a
    , quadP4 :: Point v a
    }

type P2 a = Point V2 a

pattern P2 :: a -> a -> P2 a
pattern P2 x y <- P (V2 x y)
{-# COMPLETE P2 #-}

type P3 a = Point V3 a

type Tri2 a = Tri V2 a

type Tri3 a = Tri V3 a

type Quad3 a = Quad V3 a

-- | Construct a point from x,y,z coordinates.
mkp3 :: a -> a -> a -> P3 a
mkp3 x y z = P (V3 x y z)

-- | Cube made of quadrilaterals.
quadCube :: forall  a . Num a => [Quad3 a]
quadCube =
    [ Quad p1 p2 p3 p4
    , Quad p5 p8 p7 p6
    , Quad p1 p4 p8 p5
    , Quad p2 p6 p7 p3
    , Quad p1 p5 p6 p2
    , Quad p3 p7 p8 p4
    ]
  where
    p1, p2, p3, p4, p5, p6, p7, p8 :: P3 a
    p1 = mkp3 1 1 1
    p2 = mkp3 (-1) 1 1
    p3 = mkp3 (-1) (-1) 1
    p4 = mkp3 1 (-1) 1
    p5 = mkp3 1 1 (-1)
    p6 = mkp3 (-1) 1 (-1)
    p7 = mkp3 (-1) (-1) (-1)
    p8 = mkp3 1 (-1) (-1)


-- | Cube made of triangles.
triCube :: Num a => [Tri3 a]
triCube = concatMap pairList $ splitQuad <$> quadCube
  where
    pairList :: (b, b) -> [b]
    pairList (x, y) = [x, y]

-- | Split a quad into its pair of triangles.
splitQuad :: forall v a . Quad v a -> (Tri v a, Tri v a)
splitQuad (Quad p1 p2 p3 p4) = (tri1, tri2)
  where
    tri1, tri2 :: Tri v a
    tri1 = Tri p1 p2 p3
    tri2 = Tri p1 p3 p4

-- | Barycentric coordinates.
data Bary a
  = Bary
  { baryL1 :: a
  , baryL2 :: a
  , baryL3 :: a
  }

-- | Compute the barycentric coordinates of a point within a 2D triangle.
bary :: forall  a . Fractional a => Tri2 a -> P2 a -> Bary a
bary (Tri (P2 x1 y1) (P2 x2 y2) (P2 x3 y3)) (P2 x y) = Bary l1 l2 l3
  where
    l1, l2, l3 :: a
    l1 = ((y2 - y3) * dx + (x3 - x2) * dy) / det
    l2 = ((y3 - y1) * dx + (x1 - x3) * dy) / det
    l3 = 1 - l1 - l2

    dx, dy :: a
    dx = x - x3
    dy = y - y3

    det :: a
    det = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)

-- | Lookat projection.
lookAt
  :: forall a.
     P3 a   -- ^ Origin.
  -> V3 a   -- ^ Up vector.
  -> P3 a   -- ^ Camera position.
  -> M44 a  -- ^ 4x4 transformation matrix.
lookAt = undefined

-- | Translation projection.
translation :: forall a. Num a => V3 a -> M44 a
translation (V3 tx ty tz) = V4 r1 r2 r3 r4
  where
    r1, r2, r3, r4 :: V4 a
    r1 = V4 0 0 0 tx
    r2 = V4 0 0 0 ty
    r3 = V4 0 0 0 tz
    r4 = V4 0 0 0 1

-- | Perspective projection.
perspective
  :: forall a. (Floating a)
  => a      -- ^ Aspect ratio (width / height)
  -> a      -- ^ Field of view (degrees)
  -> a      -- ^ Near clipping plane
  -> a      -- ^ Far clipping plane
  -> M44 a  -- ^ 4x4 perspective transformation matrix
perspective aspect fov near far = V4 r1 r2 r3 r4
  where
    r1, r2, r3, r4 :: V4 a
    r1 = V4 a 0 0 0
    r2 = V4 0 b 0 0
    r3 = V4 0 0 c d
    r4 = V4 0 0 (-1) 0

    fovDeg, tfovDeg2, dz :: a
    fovDeg   = fov * 180 / pi
    tfovDeg2 = tan (fovDeg / 2)
    dz       = far - near

    a, b, c, d :: a
    a = 1 / (aspect * tfovDeg2)
    b = 1 / tfovDeg2
    c = -(far + near) / dz
    d = (2 * far * near) / dz
