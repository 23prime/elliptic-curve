{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Ellipse where

import           Data.List
import           Data.Proxy
--import           Data.Singletons
--import           Data.Singletons.TH
import           GHC.TypeLits

import           Field

-----------------
-- Define Frac --
-----------------
data Frac = Nat :/: Nat

data SFrac (n :: Frac) = SFrac {-# UNPACK #-} !Int {-# UNPACK #-} !Int

class KnownFrac (n :: Frac) where
  fracSing :: SFrac n

instance (KnownNat a, KnownNat b) => KnownFrac (a :/: b) where
  fracSing = SFrac
    (fromIntegral $ natVal (Proxy::Proxy a))
    (fromIntegral $ natVal (Proxy::Proxy b))

fracVal :: forall f n proxy. (KnownFrac n, Fractional f) => proxy n -> f
fracVal _ = case fracSing :: SFrac n of
              SFrac a b -> fromIntegral a / fromIntegral b

--------------------
-- Define Ellipse --
--------------------
data Ellipse :: * -> Frac -> * where
  E     :: Fractional a => (a, a) -> Ellipse a p
  Infty :: Fractional a => Ellipse a p

instance Eq a => Eq (Ellipse a p) where
  E p1  == E p2  = p1 == p2
  E _   == Infty = False
  Infty == E _   = False
  Infty == Infty = True

instance Ord a => Ord (Ellipse a p) where
  E p1  `compare` E p2 = p1 `compare` p2
  Infty `compare` E _ = LT
  E _   `compare` Infty = GT
  Infty `compare` Infty = EQ

  E p1  < E p2  = p1 < p2
  Infty < E _   = True
  _     < Infty = False

  E p1  <= E p2  = p1 <= p2
  Infty <= _     = True
  E _   <= Infty = False

  E p1  > E p2  = p1 > p2
  Infty > _     = False
  E _   > Infty = True

  E p1  >= E p2  = p1 >= p2
  Infty >= E _   = False
  _     >= Infty = True

  max (E p1) (E p2) = E $ max p1 p2
  max Infty e       = e
  max e Infty       = e

  min (E p1) (E p2) = E $ min p1 p2
  min Infty e       = Infty
  min e Infty       = Infty

instance (KnownFrac p, Fractional a, Show a) => Show (Ellipse a p) where
  show (E (x, y)) = show (x, y)
  show Infty      = "Intfy"

instance (KnownFrac p, Fractional a, Eq a) => Num (Ellipse a p) where
  (+)                     = addEllipse
  E (x1, y1) - E (x2, y2) = E (x1, y1) + (- E (x2, y2))

  negate (E (x, y)) = E (x, - y) -- only (F p)
  negate Infty      = Infty

  abs         = id
  (*)         = undefined
  signum      = undefined
  fromInteger = undefined


---------------------------
-- Functions for Ellipse --
---------------------------
-- Make a in "y^2 = x^3 + a x + b"
mkCoeff :: (KnownFrac p, Fractional a) => Ellipse a p -> a
mkCoeff = fracVal

-- Addition of Ellipse
addEllipse :: (KnownFrac p, Eq a, Fractional a) => Ellipse a p -> Ellipse a p -> Ellipse a p
addEllipse x Infty = x
addEllipse Infty y = y
addEllipse e@(E (x1, y1)) (E (x2, y2))
  | x1 == x2 && (y1 /= y2 || (y1, y2) == (0, 0)) = Infty
  | otherwise                                    = E (x, y)
  where
    diffs = (x2 - x1, y2 - y1)
    s     = case diffs of
              (0, 0) -> (3 * x1 ^ 2 + mkCoeff e) / (2 * y1)
              _      -> snd diffs / fst diffs
    x     = s ^ 2 - x1 - x2
    y     = s * (x1 - x) - y1

-- Scalar multiplication of Ellipse
scalarMul :: (KnownFrac p, Eq a, Fractional a) => Int -> Ellipse a p -> Ellipse a p
scalarMul n = foldl (+) Infty . replicate n

-- Generate [Point] with Addition from a Point in Ellipse
generateEs :: (KnownFrac p, Eq a, Fractional a) => Ellipse a p -> [Ellipse a p]
generateEs Infty = [Infty]
generateEs e     = make e e [Infty]
  where
    make e0 e es
      | e `elem` es = es
      | otherwise   = make e0 (e + e0) $ es ++ [e]

-- Like fmap
emap :: (Fractional a, Fractional b) => (a -> b) -> Ellipse a p -> Ellipse b p
emap f (E (x, y)) = E (f x, f y)

-------------
-- Samples --
-------------
-- y^2 = x^3 + x + 1 in F_5
s0 = E (0, 1) :: Ellipse (F 5) (1 :/: 1)
s1 = E (4, 2) :: Ellipse (F 5) (1 :/: 1)
s2 = E (2, 1) :: Ellipse (F 5) (1 :/: 1)
s3 = E (3, 4) :: Ellipse (F 5) (1 :/: 1)
s4 = E (3, 1) :: Ellipse (F 5) (1 :/: 1)
s5 = E (2, 4) :: Ellipse (F 5) (1 :/: 1)
s6 = E (4, 3) :: Ellipse (F 5) (1 :/: 1)
s7 = E (0, 4) :: Ellipse (F 5) (1 :/: 1)
ss = [Infty, s0, s1, s2, s3, s4, s5, s6, s7]

-- >s0 + s1
-- (2, 1)
-- >it == s2
-- True
-- >generateEs s0
-- [Intfy,(0,1),(4,2),(2,1),(3,4),(3,1),(2,4),(4,3),(0,4)]
-- >it == ss
-- True

-- y^2 = x^3 + 4 x in Q
t0 = E (0, 0)  :: Ellipse Rational (4 :/: 1)
t1 = E (-2, 0) :: Ellipse Rational (4 :/: 1)
t2 = E (2, 0)  :: Ellipse Rational (4 :/: 1)
ts = [Infty, t0, t1, t2]
