module Data.OpenWitness.Dynamic where
{
	import Data.Witness;
	import Data.OpenWitness.Typeable;

	data Dynamic = forall a. MkDynamic (TypeRep a) a;

	toDyn :: Typeable a => a -> Dynamic;
	toDyn = MkDynamic rep;

	fromDyn :: Typeable a => Dynamic -> a -> a;
	fromDyn dyn def = case fromDynamic dyn of
	{
		Just a -> a;
		_ -> def;
	};

	fromDynamic :: Typeable a => Dynamic -> Maybe a;
	fromDynamic (MkDynamic uq a) = do
	{
		st <- matchWitness uq rep;
		return (mapSameType st a);
	};

	--dynApply :: Dynamic -> Dynamic -> Maybe Dynamic;
	--dynApp :: Dynamic -> Dynamic -> Dynamic

}
