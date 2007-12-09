module Data.OpenWitness.Typeable where
{
	import Data.Witness;
	import Data.OpenWitness;

	data TypeRep1 p where
	{
		SimpleTypeRep1 :: IOWitness (p ()) -> TypeRep1 p;
--		ApplyTypeRep :: Wit (f () ()) -> TypeRep a -> TypeRep1 (f a);
	};

	matchTypeRep1 :: TypeRep1 a -> TypeRep1 b -> Maybe (SameType (a ()) (b ()));
	matchTypeRep1 (SimpleTypeRep1 wa) (SimpleTypeRep1 wb) = matchWitness wa wb;
	{-
	matchTypeRep1 (ApplyTypeRep wfa ta) (ApplyTypeRep wfb tb) = do
	{
		stf <- matchUniqueWitness wfa wfb;
		starg <- matchTypeRep ta tb;
		return (apply1SameType stf starg);
	};
	matchTypeRep _ _ = Nothing;
	-}

	data TypeRep a where
	{
		SimpleTypeRep :: IOWitness a -> TypeRep a;
		ApplyTypeRep :: TypeRep1 p -> TypeRep a -> TypeRep (p a);
	};

	instance Witness TypeRep where
	{
		matchWitness (SimpleTypeRep wa) (SimpleTypeRep wb) = matchWitness wa wb;
		matchWitness (ApplyTypeRep tfa ta) (ApplyTypeRep tfb tb) = do
		{
			stf <- matchTypeRep1 tfa tfb;
			starg <- matchWitness ta tb;
			return (apply1SameType stf starg);
		};
		matchWitness _ _ = Nothing;
	};




	class Typeable a where
	{
		rep :: TypeRep a;
	};

	cast :: (Typeable a,Typeable b) => a -> Maybe b;
	cast a = do
	{
		st <- matchWitness rep rep;
		return (mapSameType st a);
	};

	gcast :: (Typeable a,Typeable b) => c a -> Maybe (c b);
	gcast ca = do
	{
		st <- matchWitness rep rep;
		return (unSameType st ca);
	};
}
