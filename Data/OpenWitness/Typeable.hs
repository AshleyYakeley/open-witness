module Data.OpenWitness.Typeable where
{
	import Data.Witness;
	import Data.OpenWitness;

	data TypeRep2 p where
	{
		SimpleTypeRep2 :: IOWitness (p () ()) -> TypeRep2 p;
--		ApplyTypeRep2 :: TypeRep3 p -> TypeRep a -> TypeRep2 (p a);
	};

	matchTypeRep2 :: TypeRep2 a -> TypeRep2 b -> Maybe (SameType (a () ()) (b () ()));
	matchTypeRep2 (SimpleTypeRep2 wa) (SimpleTypeRep2 wb) = matchWitness wa wb;
{-
	matchTypeRep2 (ApplyTypeRep1 tfa ta) (ApplyTypeRep1 tfb tb) = do
	{
		MkSameType <- matchTypeRep2 tfa tfb;
		MkSameType <- matchWitness ta tb;
		return MkSameType;
	};
	matchTypeRep2 _ _ = Nothing;
-}
	data TypeRep1 p where
	{
		SimpleTypeRep1 :: IOWitness (p ()) -> TypeRep1 p;
		ApplyTypeRep1 :: TypeRep2 p -> TypeRep a -> TypeRep1 (p a);
	};

	matchTypeRep1 :: TypeRep1 a -> TypeRep1 b -> Maybe (SameType (a ()) (b ()));
	matchTypeRep1 (SimpleTypeRep1 wa) (SimpleTypeRep1 wb) = matchWitness wa wb;
	matchTypeRep1 (ApplyTypeRep1 tfa ta) (ApplyTypeRep1 tfb tb) = do
	{
		MkSameType <- matchTypeRep2 tfa tfb;
		MkSameType <- matchWitness ta tb;
		return MkSameType;
	};
	matchTypeRep1 _ _ = Nothing;

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

{-
	mkAppTy :: TypeRep2 (->) -> TypeRep (a -> b) -> TypeRep a -> TypeRep b;
	mkAppTy t2 tf ta = ApplyTypeRep (ApplyTypeRep1 ) tb;
-}
	mkFunTy :: TypeRep2 (->) -> TypeRep a -> TypeRep b -> TypeRep (a -> b);
	mkFunTy t2 ta tb = ApplyTypeRep (ApplyTypeRep1 t2 ta) tb;

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
