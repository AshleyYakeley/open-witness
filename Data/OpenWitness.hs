module Data.OpenWitness
(
	OpenWitness,
	RealWorld,IOWitness,newIOWitness,
	OW,newOpenWitnessOW,runOW,owToIO,
	unsafeIOWitnessFromInteger,unsafeIOWitnessFromString
) where
{
	import Data.Witness;
	import Unsafe.Coerce;
	import System.IO.Unsafe (unsafePerformIO);
	import Control.Concurrent.MVar;
	import Control.Monad.State;
	import Data.HashTable;

	unsafeSameType :: SameType a b;
	unsafeSameType = unsafeCoerce MkSameType;

	newtype OpenWitness s a = MkOpenWitness Integer deriving Eq;
	
	instance Witness (OpenWitness s) where
	{
		matchWitness (MkOpenWitness ua) (MkOpenWitness ub) = 
			if ua == ub then Just unsafeSameType else Nothing;
	};

	data RealWorld;

	type IOWitness = OpenWitness RealWorld;

	ioWitnessSource :: MVar Integer;
	{-# NOINLINE ioWitnessSource #-};
	ioWitnessSource = unsafePerformIO (newMVar 0);

	newIOWitness :: forall a. IO (IOWitness a);
	newIOWitness = do
	{
		val <- takeMVar ioWitnessSource;
		putMVar ioWitnessSource (val + 1);
		return (MkOpenWitness val);
	};
	
	type OWState = Integer;
	
	newtype OW s a = MkOW (State OWState a) deriving (Functor,Monad,MonadFix);
	
	runOW :: forall a. (forall s. OW s a) -> a;
	runOW uw = (\(MkOW st) -> evalState st 0) uw;
	
	newOpenWitnessOW :: forall s a. OW s (OpenWitness s a);
	newOpenWitnessOW = MkOW (State (\val -> (MkOpenWitness val,val+1)));
	
	owToIO :: OW RealWorld a -> IO a;
	owToIO (MkOW st) = modifyMVar ioWitnessSource (\start -> let
	{
		(a,count) = runState st start;
	} in return (count,a));
	
	unsafeIOWitnessFromInteger :: Integer -> IOWitness a;
	unsafeIOWitnessFromInteger = MkOpenWitness;
	
	-- | a hack in the absence of open witness declarations
	unsafeIOWitnessFromString :: String -> IOWitness a;
	unsafeIOWitnessFromString = unsafeIOWitnessFromInteger . fromIntegral . hashString;
}
