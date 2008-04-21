module Data.OpenWitness.Dynamic where
{
	import System.IO.Unsafe;
	import Data.Witness;
	import Data.OpenWitness;
	import Data.OpenWitness.Typeable;

	type Dynamic = Any TypeRep;

	toDyn :: Typeable a => a -> Dynamic;
	toDyn = MkAny rep;

	fromDyn :: Typeable a => Dynamic -> a -> a;
	fromDyn dyn def = case fromDynamic dyn of
	{
		Just a -> a;
		_ -> def;
	};

	fromDynamic :: forall a. Typeable a => Dynamic -> Maybe a;
	fromDynamic (MkAny uq a) = do
	{
		MkSameType <- matchWitness uq (rep :: TypeRep a);
		return a;
	};

	witFn :: IOWitness (() -> ()); -- <- newIOWitness;
	{-# NOINLINE witFn #-};
	witFn = unsafePerformIO newIOWitness;

	instance (Typeable a,Typeable b) => Typeable (a -> b) where
	{
		rep = ApplyTypeRep (ApplyTypeRep1 (SimpleTypeRep2 witFn) rep) rep;
	};

	dynApply :: Dynamic -> Dynamic -> Maybe Dynamic;
	dynApply (MkAny (ApplyTypeRep (ApplyTypeRep1 (SimpleTypeRep2 witFn') rx') ry) f) (MkAny rx x) = do
	{
		MkSameType <- matchWitness witFn' witFn;
		MkSameType <- matchWitness rx' rx;
		return (MkAny ry (f x));
	};
	dynApply _ _ = Nothing;

	--dynApp :: Dynamic -> Dynamic -> Dynamic
}
