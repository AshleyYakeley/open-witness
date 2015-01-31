-- | This is an approximate re-implementation of "Data.Typeable" using open witnesses.
module Data.OpenWitness.Typeable where
{
{-
    import Data.OpenWitness.OpenRep;
    import Data.OpenWitness;
    import Data.Witness;

    -- | types of kind @*@ with a representation
    ;
    class Typeable a where
    {
        rep :: OpenRep a;
    };

    -- | types of kind @* -> *@ with a representation
    ;
    class Typeable1 t where
    {
        rep1 :: OpenRep1 t;
    };

    -- | types of kind @* -> * -> *@ with a representation
    ;
    class Typeable2 t where
    {
        rep2 :: OpenRep2 t;
    };

    instance (Typeable1 f,Typeable a) => Typeable (f a) where
    {
        rep = ApplyOpenRep rep1 rep;
    };

    instance (Typeable2 f,Typeable a) => Typeable1 (f a) where
    {
        rep1 = ApplyOpenRep1 rep2 rep;
    };

    instance Typeable2 (->) where
    {
        rep2 = SimpleOpenRep2 $(iowitness [t|T2 (->)|]);
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

    -- | represents a type of kind @*@
    ;
    type TypeRep = AnyWitness OpenRep;

    typeOf :: forall t. (Typeable t) => t -> TypeRep;
    typeOf _ = MkAnyWitness (rep :: OpenRep t);

    -- | represents a type of kind @* -> *@
    ;
    type TypeRep1 = AnyWitness1 OpenRep1;

    typeOf1 :: forall t a. (Typeable1 t) => t a -> TypeRep1;
    typeOf1 _ = MkAnyWitness1 (rep1 :: OpenRep1 t);

    -- | represents a type of kind @* -> * -> *@
    ;
    type TypeRep2 = AnyWitness2 OpenRep2;

    typeOf2 :: forall t a b. (Typeable2 t) => t a b -> TypeRep2;
    typeOf2 _ = MkAnyWitness2 (rep2 :: OpenRep2 t);

    -- | given representations of @a@ and @b@, make a representation of @a -> b@
    ;
    mkFunTy :: TypeRep -> TypeRep -> TypeRep;
    mkFunTy (MkAnyWitness ta) (MkAnyWitness tb) = MkAnyWitness (ApplyOpenRep (ApplyOpenRep1 (rep2 :: OpenRep2 (->)) ta) tb);

    -- | given representations of @a -> b@ and @a@, make a representation of @b@ (otherwise not)
    ;
    funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep;
    funResultTy (MkAnyWitness (ApplyOpenRep (ApplyOpenRep1 repFn' ta') tb')) (MkAnyWitness ta) = do
    {
        MkEqualType <- matchOpenRep2 repFn' (rep2 :: OpenRep2 (->));
        MkEqualType <- matchWitness ta' ta;
        return (MkAnyWitness tb');
    };
    funResultTy _ _ = Nothing;

    mkAppTy :: TypeRep1 -> TypeRep -> TypeRep;
    mkAppTy (MkAnyWitness1 tf) (MkAnyWitness ta) = MkAnyWitness (ApplyOpenRep tf ta);

    mkAppTy1 :: TypeRep2 -> TypeRep -> TypeRep1;
    mkAppTy1 (MkAnyWitness2 tf) (MkAnyWitness ta) = MkAnyWitness1 (ApplyOpenRep1 tf ta);
-}
}
