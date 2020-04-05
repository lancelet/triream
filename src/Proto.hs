{-# LANGUAGE ScopedTypeVariables #-}
module Proto where

import           Linear                         ( V2(V2)
                                                , V3
                                                )
import           Linear.Affine                  ( Point(P) )

data Tri v a
  = Tri
    { triP1 :: Point v a
    , triP2 :: Point v a
    , triP3 :: Point v a
    }

type P2 a = Point V2 a

type Tri2 a = Tri V2 a

type Tri3 a = Tri V3 a

-- | Barycentric coordinates.
data Bary a
  = Bary
  { baryL1 :: a
  , baryL2 :: a
  , baryL3 :: a
  }

-- | Compute the barycentric coordinates of a point within a 2D triangle.
bary :: forall  a . Fractional a => Tri2 a -> P2 a -> Bary a
bary (Tri (P (V2 x1 y1)) (P (V2 x2 y2)) (P (V2 x3 y3))) (P (V2 x y)) = Bary
    l1
    l2
    l3
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
