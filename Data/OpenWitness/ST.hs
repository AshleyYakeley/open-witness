module Data.OpenWitness.ST
(
	ST,runST,fixST,
	newSTRef,readSTRef,writeSTRef,modifySTRef,
	stToOW,RealWorld,stToIO
) where
{
	import Data.OpenWitness;
	import Data.Witness.WitnessDict;
	import Control.Monad.State;
	
	type ST s a = StateT (WitnessDict (OpenWitness s)) (OW s) a;

	stToOW :: ST s a -> OW s a;
	stToOW st = evalStateT st emptyWitnessDict;

	runST :: (forall s . ST s a) -> a;
	runST st = runOW (stToOW st);
	
	fixST :: (a -> ST s a) -> ST s a;
	fixST = mfix;

	type STRef = OpenWitness;
	
	newSTRef :: a -> ST s (STRef s a);
	newSTRef a = do
	{
		wit <- lift newOpenWitnessOW;
		dict <- get;
		put (witnessDictAdd wit a dict);
		return wit;
	};
	
	readSTRef :: STRef s a -> ST s a;
	readSTRef key = do
	{
		dict <- get;
		case witnessDictLookup key dict of
		{
			Just a -> return a;
			_ -> fail "ref not found";
		};
	};
	
	writeSTRef :: forall s a. STRef s a -> a -> ST s ();
	writeSTRef key newa = modify (\dict -> case witnessDictReplace key newa dict of
	{
		Just dict' -> dict';
		_ -> error "ref not found";
	});
	
	modifySTRef :: forall s a. STRef s a -> (a -> a) -> ST s ();
	modifySTRef key amap = modify (\dict -> case witnessDictModify key amap dict of
	{
		Just dict' -> dict';
		_ -> error "ref not found";
	});
	
	stToIO :: ST RealWorld a -> IO a;
	stToIO = owToIO . stToOW;
}
