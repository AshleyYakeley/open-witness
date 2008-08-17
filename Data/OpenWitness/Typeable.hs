-- | This is an approximate re-implementation of "Data.Typeable" using open witnesses.
module Data.OpenWitness.Typeable
(
	module Data.OpenWitness.Typeable.Rep,
	module Data.OpenWitness.Typeable
) where
{
	import Data.OpenWitness.Typeable.Rep;
	import Data.OpenWitness;
	import Data.Witness;
	import Data.Maybe;

	-- | types of kind @*@ with a representation
	;
	class Typeable a where
	{
		rep :: Rep a;
	};

	-- | types of kind @* -> *@ with a representation
	;
	class Typeable1 t where
	{
		rep1 :: Rep1 t;
	};

	-- | types of kind @* -> * -> *@ with a representation
	;
	class Typeable2 t where
	{
		rep2 :: Rep2 t;
	};

	instance (Typeable1 f,Typeable a) => Typeable (f a) where
	{
		rep = ApplyRep rep1 rep;
	};

	instance (Typeable2 f,Typeable a) => Typeable1 (f a) where
	{
		rep1 = ApplyRep1 rep2 rep;
	};

	instance Typeable2 (->) where
	{
		rep2 = SimpleRep2 witFn where
		{
			witFn :: IOWitness (() -> ()); -- <- newIOWitness;
			witFn = unsafeIOWitnessFromString "Data.OpenWitness.Typeable.witFn";
		};
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
	type TypeRep = AnyWitness Rep;

	typeOf :: forall t. (Typeable t) => t -> TypeRep;
	typeOf _ = MkAnyWitness (rep :: Rep t);
	
	-- | represents a type of kind @* -> *@
	;
	type TypeRep1 = AnyWitness1 Rep1;

	typeOf1 :: forall t a. (Typeable1 t) => t a -> TypeRep1;
	typeOf1 _ = MkAnyWitness1 (rep1 :: Rep1 t);
	
	-- | represents a type of kind @* -> * -> *@
	;
	type TypeRep2 = AnyWitness2 Rep2;

	typeOf2 :: forall t a b. (Typeable2 t) => t a b -> TypeRep2;
	typeOf2 _ = MkAnyWitness2 (rep2 :: Rep2 t);
	
	-- | given representations of @a@ and @b@, make a representation of @a -> b@
	;
	mkFunTy :: TypeRep -> TypeRep -> TypeRep;
	mkFunTy (MkAnyWitness ta) (MkAnyWitness tb) = MkAnyWitness (ApplyRep (ApplyRep1 (rep2 :: Rep2 (->)) ta) tb);

	-- | given representations of @a -> b@ and @a@, make a representation of @b@ (otherwise not)
	;
	funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep;
	funResultTy (MkAnyWitness (ApplyRep (ApplyRep1 repFn' ta') tb')) (MkAnyWitness ta) = do
	{
		MkEqualType <- matchRep2 repFn' (rep2 :: Rep2 (->));
		MkEqualType <- matchWitness ta' ta;
		return (MkAnyWitness tb');
	};
	funResultTy _ _ = Nothing;
	
	mkAppTy :: TypeRep1 -> TypeRep -> TypeRep;
	mkAppTy (MkAnyWitness1 tf) (MkAnyWitness ta) = MkAnyWitness (ApplyRep tf ta);
	
	mkAppTy1 :: TypeRep2 -> TypeRep -> TypeRep1;
	mkAppTy1 (MkAnyWitness2 tf) (MkAnyWitness ta) = MkAnyWitness1 (ApplyRep1 tf ta);
}
