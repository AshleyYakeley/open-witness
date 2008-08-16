module Data.OpenWitness.Typeable where
{
	import Data.Witness;
	import Data.OpenWitness;
	import Data.Maybe;

	data Rep2 p where
	{
		SimpleRep2 :: IOWitness (p () ()) -> Rep2 p;
--		ApplyRep2 :: TypeRep3 p -> Rep a -> Rep2 (p a);
	};

	matchRep2 :: Rep2 a -> Rep2 b -> Maybe (EqualType (a () ()) (b () ()));
	matchRep2 (SimpleRep2 wa) (SimpleRep2 wb) = matchWitness wa wb;
{-
	matchRep2 (ApplyRep1 tfa ta) (ApplyRep1 tfb tb) = do
	{
		MkEqualType <- matchRep2 tfa tfb;
		MkEqualType <- matchWitness ta tb;
		return MkEqualType;
	};
	matchRep2 _ _ = Nothing;
-}
	data Rep1 p where
	{
		SimpleRep1 :: IOWitness (p ()) -> Rep1 p;
		ApplyRep1 :: Rep2 p -> Rep a -> Rep1 (p a);
	};

	matchRep1 :: Rep1 a -> Rep1 b -> Maybe (EqualType (a ()) (b ()));
	matchRep1 (SimpleRep1 wa) (SimpleRep1 wb) = matchWitness wa wb;
	matchRep1 (ApplyRep1 tfa ta) (ApplyRep1 tfb tb) = do
	{
		MkEqualType <- matchRep2 tfa tfb;
		MkEqualType <- matchWitness ta tb;
		return MkEqualType;
	};
	matchRep1 _ _ = Nothing;

	data Rep a where
	{
		SimpleRep :: IOWitness a -> Rep a;
		ApplyRep :: Rep1 p -> Rep a -> Rep (p a);
	};

	instance SimpleWitness Rep where
	{
		matchWitness (SimpleRep wa) (SimpleRep wb) = matchWitness wa wb;
		matchWitness (ApplyRep tfa ta) (ApplyRep tfb tb) = do
		{
			MkEqualType <- matchRep1 tfa tfb;
			MkEqualType <- matchWitness ta tb;
			return MkEqualType;
		};
		matchWitness _ _ = Nothing;
	};

	instance Eq1 Rep where
	{
		equals1 r1 r2 = isJust (matchWitness r1 r2);
	};

	repFn :: Rep2 (->);
	repFn = SimpleRep2 witFn where
	{
		witFn :: IOWitness (() -> ()); -- <- newIOWitness;
		witFn = unsafeIOWitnessFromString "Data.OpenWitness.Typeable.witFn";
	};

	class Typeable a where
	{
		rep :: Rep a;
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

	type TypeRep = AnyWitness Rep;

	typeOf :: forall a. (Typeable a) => a -> TypeRep;
	typeOf _ = MkAnyWitness (rep :: Rep a);

{-
	mkAppTy :: Rep (a -> b) -> Rep a -> Rep b;
	mkAppTy tf ta = ApplyRep (ApplyRep1 ) tb;
-}
	mkFunTy :: TypeRep -> TypeRep -> TypeRep;
	mkFunTy (MkAnyWitness ta) (MkAnyWitness tb) = MkAnyWitness (ApplyRep (ApplyRep1 repFn ta) tb);

	funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep;
	funResultTy (MkAnyWitness (ApplyRep (ApplyRep1 repFn' ta') tb')) (MkAnyWitness ta) = do
	{
		MkEqualType <- matchRep2 repFn' repFn;
		MkEqualType <- matchWitness ta' ta;
		return (MkAnyWitness tb');
	};
	funResultTy _ _ = Nothing;

	instance (Typeable a,Typeable b) => Typeable (a -> b) where
	{
		rep = ApplyRep (ApplyRep1 repFn rep) rep;
	};
}
