-- | This is an approximate re-implementation of "Data.Dynamic" using open witnesses.
module Data.OpenWitness.Dynamic where
{
	import Data.Witness;
	import Data.OpenWitness.OpenRep;
	import Data.OpenWitness.Typeable;

	-- * The @Dynamic@ type
	;

	type Dynamic = Any OpenRep;

	-- * Converting to and from @Dynamic@
	;

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
		MkEqualType <- matchWitness uq (rep :: OpenRep a);
		return a;
	};

	-- * Applying functions of dynamic type
	;

	dynApply :: Dynamic -> Dynamic -> Maybe Dynamic;
	dynApply (MkAny (ApplyOpenRep (ApplyOpenRep1 repFn' rx') ry) f) (MkAny rx x) = do
	{
		MkEqualType <- matchOpenRep2 repFn' (rep2 :: OpenRep2 (->));
		MkEqualType <- matchWitness rx' rx;
		return (MkAny ry (f x));
	};
	dynApply _ _ = Nothing;

	dynApp :: Dynamic -> Dynamic -> Dynamic;
	dynApp a b = case (dynApply a b) of
	{
		Just d -> d;
		_ -> error "Type error in dynamic application.\nCan't apply function to argument";
	};

	dynTypeRep :: Dynamic -> TypeRep;
	dynTypeRep (MkAny r _) = MkAnyWitness r;
}
