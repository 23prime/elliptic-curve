module EllipticCurve.Group where

class Group g where
  idElem  :: g
  inverse :: g -> g
  order   :: g -> Order

data Order = O Integer
           | InftyOrder
           deriving (Eq, Ord)

instance Show Order where
  show (O n)      = show n
  show InftyOrder = "Infty"
