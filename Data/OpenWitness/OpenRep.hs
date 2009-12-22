module Data.OpenWitness.OpenRep where
{
	import Data.Witness;
	import Data.OpenWitness;
	import Data.Maybe;

	data OpenRep2 p where
	{
		SimpleOpenRep2 :: IOWitness (p () ()) -> OpenRep2 p;
--		ApplyOpenRep2 :: OpenRep3 p -> OpenRep a -> OpenRep2 (p a);
	};

	matchOpenRep2 :: OpenRep2 a -> OpenRep2 b -> Maybe (EqualType (a () ()) (b () ()));
	matchOpenRep2 (SimpleOpenRep2 wa) (SimpleOpenRep2 wb) = matchWitness wa wb;
{-
	matchOpenRep2 (ApplyOpenRep1 tfa ta) (ApplyOpenRep1 tfb tb) = do
	{
		MkEqualType <- matchOpenRep2 tfa tfb;
		MkEqualType <- matchWitness ta tb;
		return MkEqualType;
	};
	matchOpenRep2 _ _ = Nothing;
-}
	data OpenRep1 p where
	{
		SimpleOpenRep1 :: IOWitness (p ()) -> OpenRep1 p;
		ApplyOpenRep1 :: OpenRep2 p -> OpenRep a -> OpenRep1 (p a);
	};

	matchOpenRep1 :: OpenRep1 a -> OpenRep1 b -> Maybe (EqualType (a ()) (b ()));
	matchOpenRep1 (SimpleOpenRep1 wa) (SimpleOpenRep1 wb) = matchWitness wa wb;
	matchOpenRep1 (ApplyOpenRep1 tfa ta) (ApplyOpenRep1 tfb tb) = do
	{
		MkEqualType <- matchOpenRep2 tfa tfb;
		MkEqualType <- matchWitness ta tb;
		return MkEqualType;
	};
	matchOpenRep1 _ _ = Nothing;

	data OpenRep a where
	{
		SimpleOpenRep :: IOWitness a -> OpenRep a;
		ApplyOpenRep :: OpenRep1 p -> OpenRep a -> OpenRep (p a);
	};

	instance SimpleWitness OpenRep where
	{
		matchWitness (SimpleOpenRep wa) (SimpleOpenRep wb) = matchWitness wa wb;
		matchWitness (ApplyOpenRep tfa ta) (ApplyOpenRep tfb tb) = do
		{
			MkEqualType <- matchOpenRep1 tfa tfb;
			MkEqualType <- matchWitness ta tb;
			return MkEqualType;
		};
		matchWitness _ _ = Nothing;
	};

	instance Eq1 OpenRep where
	{
		equals1 r1 r2 = isJust (matchWitness r1 r2);
	};
}