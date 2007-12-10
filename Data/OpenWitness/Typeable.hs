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
			MkSameType <- matchTypeRep1 tfa tfb;
			MkSameType <- matchWitness ta tb;
			return MkSameType;
		};
		matchWitness _ _ = Nothing;
	};




	class Typeable a where
	{
		rep :: TypeRep a;
	};

	cast :: forall a b. (Typeable a,Typeable b) => a -> Maybe b;
	cast a = do
	{
		MkSameType :: SameType a b <- matchWitness rep rep;
		return a;
	};

	gcast :: forall a b c. (Typeable a,Typeable b) => c a -> Maybe (c b);
	gcast ca = do
	{
		MkSameType :: SameType a b <- matchWitness rep rep;
		return ca;
	};
}
