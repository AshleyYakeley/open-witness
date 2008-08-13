module Unsafe where
{
	import Data.Typeable;
	import Data.Maybe;

	newtype Thing a = MkThing {unThing :: a};
	
	instance Typeable (Thing a) where
	{
		typeOf _ = typeOf ();
	};
	
	coerce :: a -> b;
	coerce a = unThing (fromJust (cast (MkThing a)));
}
