module Data.OpenWitness.OpenRep where
{
--    import Data.Witness;
--    import Data.OpenWitness;
--    import Data.Maybe;
{-
        data T2 (a :: * -> * -> *);

    data OpenRep2 p where
    {
        SimpleOpenRep2 :: IOWitness (T2 p) -> OpenRep2 p;
--        ApplyOpenRep2 :: OpenRep3 p -> OpenRep a -> OpenRep2 (p a);
    };

    matchOpenRep2 :: OpenRep2 a -> OpenRep2 b -> Maybe (EqualType (T2 a) (T2 b));
    matchOpenRep2 (SimpleOpenRep2 wa) (SimpleOpenRep2 wb) = testEquality wa wb;
{-
    matchOpenRep2 (ApplyOpenRep1 tfa ta) (ApplyOpenRep1 tfb tb) = do
    {
        MkEqualType <- matchOpenRep2 tfa tfb;
        MkEqualType <- testEquality ta tb;
        return MkEqualType;
    };
    matchOpenRep2 _ _ = Nothing;
-}

        data T1 (a :: * -> *);

    data OpenRep1 p where
    {
        SimpleOpenRep1 :: IOWitness (T1 p) -> OpenRep1 p;
        ApplyOpenRep1 :: OpenRep2 p -> OpenRep a -> OpenRep1 (p a);
    };

    matchOpenRep1 :: OpenRep1 a -> OpenRep1 b -> Maybe (EqualType (T1 a) (T1 b));
    matchOpenRep1 (SimpleOpenRep1 wa) (SimpleOpenRep1 wb) = testEquality wa wb;
    matchOpenRep1 (ApplyOpenRep1 tfa ta) (ApplyOpenRep1 tfb tb) = do
    {
        MkEqualType <- matchOpenRep2 tfa tfb;
        MkEqualType <- testEquality ta tb;
        return MkEqualType;
    };
    matchOpenRep1 _ _ = Nothing;
-}

{-
    data OpenRep :: k -> * where
    {
        SimpleOpenRep :: IOWitness a -> OpenRep a;
        ApplyOpenRep :: OpenRep p -> OpenRep a -> OpenRep (p a);
    };

    instance TestEquality OpenRep where
    {
        testEquality (SimpleOpenRep wa) (SimpleOpenRep wb) = testEquality wa wb;
        testEquality (ApplyOpenRep tfa ta) (ApplyOpenRep tfb tb) = do
        {
            MkEqualType <- testEquality tfa tfb;
            MkEqualType <- testEquality ta tb;
            return MkEqualType;
        };
        testEquality _ _ = Nothing;
    };

    instance Eq1 OpenRep where
    {
        equals1 r1 r2 = isJust (testEquality r1 r2);
    };
-}
}
