-- | This is an approximate re-implementation of "Data.Typeable" using open witnesses.
module Data.OpenWitness.Typeable where
{
    import Data.Kind;
    import Data.OpenWitness.TypeRep;
    --import Data.OpenWitness;
    import Data.Witness;

    -- | types of kind @*@ with a representation
    ;
    class Typeable (a :: k) where
    {
        typeRep :: TypeRep a;
    };

    instance (Typeable (f :: k1 -> k2),Typeable (a :: k1)) => Typeable (f a) where
    {
        typeRep = ApplyTypeRep typeRep typeRep;
    };
{-
    instance Typeable (->) where
    {
        typeRep = SimpleTypeRep $(iowitness [t|(->)|]);
    };
-}
    cast :: forall (a :: *) (b :: *). (Typeable a,Typeable b) => a -> Maybe b;
    cast a = do
    {
        Refl :: a :~: b <- testEquality typeRep typeRep;
        return a;
    };

    gcast :: forall (k :: *) (a :: k) (b :: k) (c :: k -> *). (Typeable a,Typeable b) => c a -> Maybe (c b);
    gcast ca = do
    {
        Refl :: a :~: b <- testEquality typeRep typeRep;
        return ca;
    };
{-
    -- | given representations of @a@ and @b@, make a representation of @a -> b@
    ;
    mkFunTy :: TypeRep a -> TypeRep b -> TypeRep (a -> b);
    mkFunTy (MkAnyWitness ta) (MkAnyWitness tb) = MkAnyWitness (ApplyTypeRep (ApplyTypeRep1 (rep2 :: TypeRep2 (->)) ta) tb);

    -- | given representations of @a -> b@ and @a@, make a representation of @b@ (otherwise not)
    ;
    funResultTy :: TypeRep (a -> b) -> TypeRep a -> Maybe (TypeRep b);
    funResultTy (MkAnyWitness (ApplyTypeRep (ApplyTypeRep1 repFn' ta') tb')) (MkAnyWitness ta) = do
    {
        MkEqualType <- matchTypeRep2 repFn' (rep2 :: TypeRep2 (->));
        MkEqualType <- testEquality ta' ta;
        return (MkAnyWitness tb');
    };
    funResultTy _ _ = Nothing;
-}
    mkAppTy :: forall (k1 :: *) (k2 :: *) (f :: k1 -> k2) (a :: k1). TypeRep f -> TypeRep a -> TypeRep (f a);
    mkAppTy = ApplyTypeRep;
}
