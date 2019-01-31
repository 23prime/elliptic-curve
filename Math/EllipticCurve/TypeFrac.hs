{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module EllipticCurve.TypeFrac where

import           Data.List
import           Data.Proxy
import           GHC.TypeLits

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
