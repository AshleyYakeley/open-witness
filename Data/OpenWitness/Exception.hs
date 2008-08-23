module Data.OpenWitness.Exception
(
	Exn, unsafeExnFromString, throw, catch
) where
{
	import Data.OpenWitness;
	import Data.Witness;
	import Data.Maybe;
	import Data.Typeable;
	import Control.Exception (throwDyn,catchDyn);
	import Prelude(IO,String);

	-- | A key to match exceptions. The type variable is the data the exception carries.
	;
	type Exn = IOWitness;

	newtype ExnException = MkExnException (Any Exn);
	
	-- | In the absence of open witness declarations, an unsafe hack to generate 'Exn' exception keys.
	-- This is safe if you use a different string each time (and 'hashString' doesn't collide), and if @e@ is a single type.
	;
	unsafeExnFromString :: String -> Exn e;
	unsafeExnFromString = unsafeIOWitnessFromString;
	
	instance Typeable ExnException where
	{
		typeOf _ = mkTyConApp (mkTyCon "Data.OpenWitness.Exception.ExnException") [];
	};
	
	throw :: Exn e -> e -> a;
	throw exn e = throwDyn (MkExnException (MkAny exn e));
	
	catch :: IO a -> Exn e -> (e -> IO a) -> IO a;
	catch foo exn catcher = catchDyn foo (\ex@(MkExnException cell) -> case matchAny exn cell of
	{
		Just e -> catcher e;
		_ -> throwDyn ex;
	});
}
