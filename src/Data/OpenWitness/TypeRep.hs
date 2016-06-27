module Data.OpenWitness.TypeRep where
{
    import Data.Kind;
    import Data.Maybe;
    import Data.Witness;
    import Data.OpenWitness;
    import Data.Type.Heterogeneous;

    data TypeRep :: forall (k :: *). k -> * where
    {
        SimpleTypeRep :: forall (k :: *) (a :: k). IOWitness a -> TypeRep a;
        ApplyTypeRep :: forall (k1 :: *) (k2 :: *) (p :: k1 -> k2) (a :: k1). TypeRep p -> TypeRep a -> TypeRep (p a);
    };

    instance TestHetEquality TypeRep where
    {
        testHetEquality (SimpleTypeRep wa) (SimpleTypeRep wb) = testHetEquality wa wb;
        testHetEquality (ApplyTypeRep tfa ta) (ApplyTypeRep tfb tb) = do
        {
            ReflH <- testHetEquality tfa tfb;
            ReflH <- testHetEquality ta tb;
            return ReflH;
        };
        testHetEquality _ _ = Nothing;
    };

    instance TestEquality TypeRep where
    {
        testEquality wa wb = fmap homoHetEq $ testHetEquality wa wb;
    };

    instance Eq1 TypeRep where
    {
        equals1 r1 r2 = isJust (testHetEquality r1 r2);
    };
}