-- | This is an approximate re-implementation of "Data.Dynamic" using open witnesses.
module Data.OpenWitness.Dynamic where

import Data.Kind
import Data.OpenWitness.TypeRep
import Data.OpenWitness.Typeable
import Data.Type.Heterogeneous
import Data.Witness
import Prelude

-- * The @Dynamic@ type
type Dynamic = Any TypeRep

-- * Converting to and from @Dynamic@
toDyn ::
       forall (a :: *). Typeable a
    => a
    -> Dynamic
toDyn = MkAny typeRep

fromDyn :: Typeable a => Dynamic -> a -> a
fromDyn dyn def =
    case fromDynamic dyn of
        Just a -> a
        _ -> def

fromDynamic ::
       forall (a :: *). Typeable a
    => Dynamic
    -> Maybe a
fromDynamic (MkAny uq a) = do
    Refl <- testEquality uq (typeRep :: TypeRep a)
    return a

-- * Applying functions of dynamic type
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (MkAny (ApplyTypeRep (ApplyTypeRep repFn' rx') ry) f) (MkAny rx x) = do
    ReflH <- testHetEquality repFn' (typeRep :: TypeRep (->))
    ReflH <- testHetEquality rx' rx
    return (MkAny ry (f x))
dynApply _ _ = Nothing

dynApp :: Dynamic -> Dynamic -> Dynamic
dynApp a b =
    case (dynApply a b) of
        Just d -> d
        _ -> error "Type error in dynamic application.\nCan't apply function to argument"

dynTypeRep :: Dynamic -> AnyWitness (TypeRep :: * -> *)
dynTypeRep (MkAny r _) = MkAnyWitness r
