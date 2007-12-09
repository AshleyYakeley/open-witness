module Data.OpenWitness
(
	UniqueWitness,
	RealWorld,IOWitness,newUniqueWitness,
	UW,newUniqueWitnessUW,runUW,uwToIO
) where
{
	import Data.Witness;
	import Unsafe.Coerce;
	import System.IO.Unsafe (unsafePerformIO);
	import Control.Concurrent.MVar;
	import Control.Monad.State;

	unsafeSameType :: SameType a b;
	unsafeSameType = MkSameType unsafeCoerce;

	newtype UniqueWitness s a = MkUniqueWitness Integer;
	
	instance Eq (UniqueWitness s a) where
	{
		(MkUniqueWitness ua) == (MkUniqueWitness ub) = ua == ub;
	};
	
	instance Witness (UniqueWitness s) where
	{
		matchWitness (MkUniqueWitness ua) (MkUniqueWitness ub) = 
			if ua == ub then Just unsafeSameType else Nothing;
	};

	data RealWorld;

	type IOWitness = UniqueWitness RealWorld;

	uwSource :: MVar Integer;
	{-# NOINLINE uwSource #-};
	uwSource = unsafePerformIO (newMVar 0);

	newUniqueWitness :: forall a. IO (IOWitness a);
	newUniqueWitness = do
	{
		val <- takeMVar uwSource;
		putMVar uwSource (val + 1);
		return (MkUniqueWitness val);
	};
	
	type UWState = Integer;
	
	newtype UW s a = MkUW (State UWState a) deriving (Functor,Monad,MonadFix);
	
	runUW :: forall a. (forall s. UW s a) -> a;
	runUW uw = (\(MkUW st) -> evalState st 0) uw;
	
	newUniqueWitnessUW :: forall s a. UW s (UniqueWitness s a);
	newUniqueWitnessUW = MkUW (State (\val -> (MkUniqueWitness val,val+1)));
	
	uwToIO :: UW RealWorld a -> IO a;
	uwToIO (MkUW st) = modifyMVar uwSource (\start -> let
	{
		(a,count) = runState st start;
	} in return (count,a));
}
