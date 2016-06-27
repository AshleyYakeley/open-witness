-- | This is an approximate re-implementation of "Data.Typeable" using open witnesses.
module Data.OpenWitness.Typeable where
{
    import Data.Kind;
    import Data.OpenWitness.TypeRep;
    import Data.OpenWitness;
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

    instance Typeable Type where
    {
        typeRep = SimpleTypeRep $(iowitness [t|Type|]);
    };

    type Fun = (->);

    instance Typeable (->) where
    {
        typeRep = SimpleTypeRep $(iowitness [t|Fun|]);
    };

    instance Typeable Constraint where
    {
        typeRep = SimpleTypeRep $(iowitness [t|Constraint|]);
    };

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

    -- | given representations of @a@ and @b@, make a representation of @a -> b@
    ;
    mkFunTy :: TypeRep a -> TypeRep b -> TypeRep (a -> b);
    mkFunTy ta tb = ApplyTypeRep (ApplyTypeRep (typeRep :: TypeRep (->)) ta) tb;
{- GHC panic
    -- | given representations of @a -> b@ and @a@, make a representation of @b@ (otherwise not)
    ;
    funResultTy :: TypeRep (a -> b) -> TypeRep a -> Maybe (TypeRep b);
    funResultTy (ApplyTypeRep (ApplyTypeRep repFn' ta') tb') ta = do
    {
        Refl <- testEquality repFn' (typeRep :: TypeRep (->));
        Refl <- testEquality ta' ta;
        return tb';
    };
    funResultTy _ _ = Nothing;
-}
    mkAppTy :: forall (k1 :: *) (k2 :: *) (f :: k1 -> k2) (a :: k1). TypeRep f -> TypeRep a -> TypeRep (f a);
    mkAppTy = ApplyTypeRep;
}
