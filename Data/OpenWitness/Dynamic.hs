module Data.OpenWitness.Dynamic where
{
	import Data.Witness;
	import Data.OpenWitness.Typeable;

	type Dynamic = Any Rep;

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
		MkEqualType <- matchWitness uq (rep :: Rep a);
		return a;
	};

	dynApply :: Dynamic -> Dynamic -> Maybe Dynamic;
	dynApply (MkAny (ApplyRep (ApplyRep1 repFn' rx') ry) f) (MkAny rx x) = do
	{
		MkEqualType <- matchRep2 repFn' repFn;
		MkEqualType <- matchWitness rx' rx;
		return (MkAny ry (f x));
	};
	dynApply _ _ = Nothing;

	--dynApp :: Dynamic -> Dynamic -> Dynamic
}
