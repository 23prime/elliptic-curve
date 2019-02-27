{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module EllipticCurve where

import           Data.Ratio

import           EllipticCurve.Field
import           EllipticCurve.Group
import           EllipticCurve.TypeFrac

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
  E p1  `compare` E p2  = p1 `compare` p2
  Infty `compare` E _   = LT
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

instance (KnownFrac p, Fractional a, Eq a) => Group (Ellipse a p) where
  idElem  = Infty
  inverse = negate
  order e =  O $ fromIntegral $ length $ generateEs e


---------------------------
-- Functions for Ellipse --
---------------------------
-- Make a in "y^2 = x^3 + a x + b"
coeff :: (KnownFrac p, Fractional a) => Ellipse a p -> a
coeff = fracVal

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
              (0, 0) -> (3 * x1 ^ 2 + coeff e) / (2 * y1)
              _      -> snd diffs / fst diffs
    x     = s ^ 2 - x1 - x2
    y     = s * (x1 - x) - y1

-- Scalar multiplication of Ellipse
-- Want to make it faster by the binary-method
scalarMul :: (KnownFrac p, Eq a, Fractional a) => Int -> Ellipse a p -> Ellipse a p
scalarMul n = foldl (+) Infty . replicate n

n *. e = scalarMul n e

-- Generate [Point] with Addition from a Point in Ellipse
generateEs :: (KnownFrac p, Eq a, Fractional a) => Ellipse a p -> [Ellipse a p]
generateEs Infty = [Infty]
generateEs e     = gen e e [Infty]
  where
    gen _ Infty es = es
    gen e0 e es    = gen e0 (e + e0) (es ++ [e])

height :: Rational -> Integer
height r = max (abs $ numerator r) (abs $ denominator r)

height' :: Ellipse Rational p -> Integer
height' (E (x, y)) = height x


-------------
-- Samples --
-------------
-- y^2 = x^3 + x + 1 in F_5
s1 = E (0, 1) :: Ellipse (F 5) (1 :/: 1)
s2 = E (4, 2) :: Ellipse (F 5) (1 :/: 1)
s3 = E (2, 1) :: Ellipse (F 5) (1 :/: 1)
s4 = E (3, 4) :: Ellipse (F 5) (1 :/: 1)
s5 = E (3, 1) :: Ellipse (F 5) (1 :/: 1)
s6 = E (2, 4) :: Ellipse (F 5) (1 :/: 1)
s7 = E (4, 3) :: Ellipse (F 5) (1 :/: 1)
s8 = E (0, 4) :: Ellipse (F 5) (1 :/: 1)
ss = [Infty, s1, s2, s3, s4, s5, s6, s7, s8]

-- >s0 + s1
-- (2, 1)
-- >it == s2
-- True
-- >generateEs s0
-- [Intfy,(0,1),(4,2),(2,1),(3,4),(3,1),(2,4),(4,3),(0,4)]
-- >it == ss
-- True

-- y^2 = x^3 + 4 x in Q
t1 = E (0, 0)  :: Ellipse Rational (4 :/: 1)
t2 = E (-2, 0) :: Ellipse Rational (4 :/: 1)
t3 = E (2, 0)  :: Ellipse Rational (4 :/: 1)
ts = [Infty, t1, t2, t3]

-- y^2 = x^3 + 9
u1 = E (0, 3)  :: Ellipse Rational (0 :/: 1)
u2 = E (0, -3) :: Ellipse Rational (0 :/: 1)
u3 = E (6, 15) :: Ellipse Rational (0 :/: 1)
