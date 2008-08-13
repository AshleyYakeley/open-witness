module Data.OpenWitness.Typeable where
{
	import Data.Witness;
	import Data.OpenWitness;
	import Data.Maybe;

	data TypeRep2 p where
	{
		SimpleTypeRep2 :: IOWitness (p () ()) -> TypeRep2 p;
--		ApplyTypeRep2 :: TypeRep3 p -> TypeRep a -> TypeRep2 (p a);
	};

	matchTypeRep2 :: TypeRep2 a -> TypeRep2 b -> Maybe (EqualType (a () ()) (b () ()));
	matchTypeRep2 (SimpleTypeRep2 wa) (SimpleTypeRep2 wb) = matchWitness wa wb;
{-
	matchTypeRep2 (ApplyTypeRep1 tfa ta) (ApplyTypeRep1 tfb tb) = do
	{
		MkEqualType <- matchTypeRep2 tfa tfb;
		MkEqualType <- matchWitness ta tb;
		return MkEqualType;
	};
	matchTypeRep2 _ _ = Nothing;
-}
	data TypeRep1 p where
	{
		SimpleTypeRep1 :: IOWitness (p ()) -> TypeRep1 p;
		ApplyTypeRep1 :: TypeRep2 p -> TypeRep a -> TypeRep1 (p a);
	};

	matchTypeRep1 :: TypeRep1 a -> TypeRep1 b -> Maybe (EqualType (a ()) (b ()));
	matchTypeRep1 (SimpleTypeRep1 wa) (SimpleTypeRep1 wb) = matchWitness wa wb;
	matchTypeRep1 (ApplyTypeRep1 tfa ta) (ApplyTypeRep1 tfb tb) = do
	{
		MkEqualType <- matchTypeRep2 tfa tfb;
		MkEqualType <- matchWitness ta tb;
		return MkEqualType;
	};
	matchTypeRep1 _ _ = Nothing;

	data TypeRep a where
	{
		SimpleTypeRep :: IOWitness a -> TypeRep a;
		ApplyTypeRep :: TypeRep1 p -> TypeRep a -> TypeRep (p a);
	};

	instance SimpleWitness TypeRep where
	{
		matchWitness (SimpleTypeRep wa) (SimpleTypeRep wb) = matchWitness wa wb;
		matchWitness (ApplyTypeRep tfa ta) (ApplyTypeRep tfb tb) = do
		{
			MkEqualType <- matchTypeRep1 tfa tfb;
			MkEqualType <- matchWitness ta tb;
			return MkEqualType;
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

	instance Eq1 TypeRep where
	{
		equals1 r1 r2 = isJust (matchWitness r1 r2);
	};

	cast :: forall a b. (Typeable a,Typeable b) => a -> Maybe b;
	cast a = do
	{
		MkEqualType :: EqualType a b <- matchWitness rep rep;
		return a;
	};

	gcast :: forall a b c. (Typeable a,Typeable b) => c a -> Maybe (c b);
	gcast ca = do
	{
		MkEqualType :: EqualType a b <- matchWitness rep rep;
		return ca;
	};
}
