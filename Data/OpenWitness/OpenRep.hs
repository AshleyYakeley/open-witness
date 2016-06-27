module Data.OpenWitness.OpenRep where
{
    import Data.Kind;
    import Data.Maybe;
    import Data.Witness;
    import Data.OpenWitness;
    import Data.Type.Heterogeneous;

    data OpenRep :: forall (k :: *). k -> * where
    {
        SimpleOpenRep :: forall (k :: *) (a :: k). IOWitness a -> OpenRep a;
        ApplyOpenRep :: forall (k1 :: *) (k2 :: *) (p :: k1 -> k2) (a :: k1). OpenRep p -> OpenRep a -> OpenRep (p a);
    };

    instance TestHetEquality OpenRep where
    {
        testHetEquality (SimpleOpenRep wa) (SimpleOpenRep wb) = testHetEquality wa wb;
        testHetEquality (ApplyOpenRep tfa ta) (ApplyOpenRep tfb tb) = do
        {
            ReflH <- testHetEquality tfa tfb;
            ReflH <- testHetEquality ta tb;
            return ReflH;
        };
        testHetEquality _ _ = Nothing;
    };

    instance Eq1 OpenRep where
    {
        equals1 r1 r2 = isJust (testHetEquality r1 r2);
    };
}
