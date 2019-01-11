module Data.Type.Heterogeneous where

import Prelude
import Data.Type.Equality
import Data.Kind

data HetEq (a :: ka) (b :: kb) where
    ReflH :: forall (k :: Type) (t :: k). HetEq t t

-- | somewhat awkwardly named
homoHetEq :: forall (k :: Type) (a :: k) (b :: k). HetEq a b -> a :~: b
homoHetEq ReflH = Refl

class TestHetEquality (w :: forall k. k -> Type) where
    testHetEquality :: forall (ka :: Type) (a :: ka) (kb :: Type) (b :: kb). w a -> w b -> Maybe (HetEq a b)
