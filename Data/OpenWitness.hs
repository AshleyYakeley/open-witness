module Data.OpenWitness
(
	OpenWitness,
	RealWorld,IOWitness,newIOWitness,
	OW,newOpenWitnessOW,runOW,owToIO,
--	unsafeIOWitnessFromInteger,
	unsafeIOWitnessFromString
) where
{
	import Data.Witness;
	import Unsafe.Coerce;
	import System.IO.Unsafe (unsafePerformIO);
	import Control.Concurrent.MVar;
	import Control.Monad.State;
	import Data.HashTable;

	unsafeSameType :: EqualType a b;
	unsafeSameType = unsafeCoerce MkEqualType;

	-- | A witness type that can witness to any type.
	-- But values cannot be constructed; they can only be generated in 'IO' and certain other monads.
	;
	newtype OpenWitness s a = MkOpenWitness Integer deriving Eq;
	
	instance SimpleWitness (OpenWitness s) where
	{
		matchWitness (MkOpenWitness ua) (MkOpenWitness ub) = 
			if ua == ub then Just unsafeSameType else Nothing;
	};

	-- | The @s@ type for running 'OW' in 'IO'.
	;
	data RealWorld;

	-- | An 'OpenWitness' for 'IO'.
	;
	type IOWitness = OpenWitness RealWorld;

	ioWitnessSource :: MVar Integer;
	{-# NOINLINE ioWitnessSource #-};
	ioWitnessSource = unsafePerformIO (newMVar 0);

	-- | Generate a new 'IOWitness' in 'IO'.
	;
	newIOWitness :: forall a. IO (IOWitness a);
	newIOWitness = do
	{
		val <- takeMVar ioWitnessSource;
		putMVar ioWitnessSource (val + 1);
		return (MkOpenWitness val);
	};
	
	type OWState = Integer;
	
	-- | A runnable monad in which 'OpenWitness' values can be generated.
	-- The @s@ parameter plays the same role as it does in 'ST', preventing 'OpenWitness' values from one run being used in another.
	;
	newtype OW s a = MkOW (State OWState a) deriving (Functor,Monad,MonadFix);
	
	-- | Run an 'OW' computation.
	;
	runOW :: forall a. (forall s. OW s a) -> a;
	runOW uw = (\(MkOW st) -> evalState st 0) uw;
	
	-- | Generate a new 'OpenWitness' in 'OW'.
	;
	newOpenWitnessOW :: forall s a. OW s (OpenWitness s a);
	newOpenWitnessOW = MkOW (State (\val -> (MkOpenWitness val,val+1)));
	
	-- | Run an 'OW' computation in 'IO'.
	;
	owToIO :: OW RealWorld a -> IO a;
	owToIO (MkOW st) = modifyMVar ioWitnessSource (\start -> let
	{
		(a,count) = runState st start;
	} in return (count,a));
	
	-- | In the absence of open witness declarations, an unsafe hack to generate 'IOWitness' values.
	-- This is safe if you use a different integer each time, and if @a@ is a single type.
	;
	unsafeIOWitnessFromInteger :: Integer -> IOWitness a;
	unsafeIOWitnessFromInteger = MkOpenWitness;
	
	-- | In the absence of open witness declarations, an unsafe hack to generate 'IOWitness' values.
	-- This is safe if you use a different string each time (and 'hashString' doesn't collide), and if @a@ is a single type.
	;
	unsafeIOWitnessFromString :: String -> IOWitness a;
	unsafeIOWitnessFromString = unsafeIOWitnessFromInteger . fromIntegral . hashString;
}
