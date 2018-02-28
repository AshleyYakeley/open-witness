module Data.Type.Heterogeneous where

import Data.Kind
import Data.Type.Equality

data HetEq (a :: ka) (b :: kb) where
    ReflH :: forall (k :: *) (t :: k). HetEq t t

-- | somewhat awkwardly named
homoHetEq :: forall (k :: *) (a :: k) (b :: k). HetEq a b -> a :~: b
homoHetEq ReflH = Refl

class TestHetEquality (w :: forall k. k -> *) where
    testHetEquality :: forall (ka :: *) (a :: ka) (kb :: *) (b :: kb). w a -> w b -> Maybe (HetEq a b)
