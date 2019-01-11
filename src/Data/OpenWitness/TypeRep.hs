module Data.OpenWitness.TypeRep where

import Prelude
import Data.OpenWitness
import Data.Type.Heterogeneous
import Data.Witness
import Data.Kind

data TypeRep :: forall (k :: Type). k -> Type where
    SimpleTypeRep :: forall (k :: Type) (a :: k). IOWitness a -> TypeRep a
    ApplyTypeRep :: forall (k1 :: Type) (k2 :: Type) (p :: k1 -> k2) (a :: k1). TypeRep p -> TypeRep a -> TypeRep (p a)

instance TestHetEquality TypeRep where
    testHetEquality (SimpleTypeRep wa) (SimpleTypeRep wb) = testHetEquality wa wb
    testHetEquality (ApplyTypeRep tfa ta) (ApplyTypeRep tfb tb) = do
        ReflH <- testHetEquality tfa tfb
        ReflH <- testHetEquality ta tb
        return ReflH
    testHetEquality _ _ = Nothing

instance TestEquality TypeRep where
    testEquality wa wb = fmap homoHetEq $ testHetEquality wa wb
