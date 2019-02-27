{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeOperators       #-}

module EllipticCurve.TypeFrac where

import           Data.Proxy
import           GHC.TypeLits

-----------------
-- Define Frac --
-----------------
data Frac = Nat :/: Nat

data SFrac (p :: Frac) = SFrac Int Int

class KnownFrac (p :: Frac) where
  fracSing :: SFrac p

instance (KnownNat m, KnownNat n) => KnownFrac (m :/: n) where
  fracSing = SFrac (fromIntegral $ natVal (Proxy :: Proxy m))
                   (fromIntegral $ natVal (Proxy :: Proxy n))

fracVal :: forall f p proxy. (KnownFrac p, Fractional f) => proxy p -> f
fracVal _ = case fracSing :: SFrac p of
              SFrac m n -> fromIntegral m / fromIntegral n
