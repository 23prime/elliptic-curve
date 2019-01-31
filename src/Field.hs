{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Field where

import           GHC.TypeLits
import           Math.NumberTheory.GCD
import           Math.NumberTheory.Primes


class (Eq k, Fractional k) => Field k where
  fRecip :: k -> Maybe k
  order  :: k -> Integer

newtype F (p :: Nat) = F Integer deriving (Eq, Ord)


mkF :: KnownNat p => Integer -> F p
mkF n
  | isPrime $ natVal p = p
  | otherwise          = error "Required that p is prime."
  where
    p = F $ n `mod` natVal p

instance KnownNat p => Show (F p) where
  show (F m) = show m

instance KnownNat p => Num (F p) where
  (F m) + (F n) = mkF $ m + n
  (F m) - (F n) = mkF $ m - n
  (F m) * (F n) = mkF $ m * n
  negate (F m)  = mkF $ - m
  abs           = id
  signum (F m)
    | m == 0    = F 0
    | otherwise = F 1
  fromInteger   = mkF

instance KnownNat p => Field (F p) where
  fRecip a@(F m)
    | (mkF m :: F p) == 0 = Nothing
    | isPrime p           = Just $ mkF s
    | otherwise           = Nothing
    where
      (_, s, _) = extendedGCD m p
      p         = natVal a
  order a@(F m) = natVal a

instance KnownNat p => Fractional (F p) where
  recip a@(F m)
    | (mkF m :: F p) == 0 = undefined
    | isPrime p           = mkF s
    | otherwise           = undefined
    where
      (_, s, _) = extendedGCD m p
      p         = natVal a
  F m / F n    = F m * recip (F n)
  fromRational = undefined


fromF :: F p -> Integer
fromF (F m) = m
