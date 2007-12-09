{-# OPTIONS -fglasgow-exts -Wall -Werror #-}
module Data.OpenWitness.ST
(
	ST,runST,fixST,
	newSTRef,readSTRef,writeSTRef,modifySTRef,
	stToOW,RealWorld,stToIO
) where
{
	import Data.OpenWitness;
	import Data.OpenWitness.OpenDict;
	import Control.Monad.State;
	
	newtype ST s a = MkST (StateT (OpenDict s) (OW s) a) deriving (Functor,Monad,MonadFix);

	stToOW :: ST s a -> OW s a;
	stToOW (MkST st) = evalStateT st emptyOpenDict;

	runST :: (forall s . ST s a) -> a;
	runST st = runOW (stToOW st);
	
	fixST :: (a -> ST s a) -> ST s a;
	fixST = mfix;

	newtype STRef s a = MkSTRef (OpenKey s a) deriving Eq;
	
	newSTRef :: a -> ST s (STRef s a);
	newSTRef a = MkST (do
	{
		wit <- lift newOpenWitnessOW;
		dict <- get;
		let {(dict',key) = openKeyNew wit a dict;};
		put dict';
		return (MkSTRef key);
	});
	
	readSTRef :: STRef s a -> ST s a;
	readSTRef (MkSTRef key) = MkST (do
	{
		dict <- get;
		case openKeyLookup key dict of
		{
			Just a -> return a;
			_ -> fail "ref not found";
		};
	});
	
	writeSTRef :: forall s a. STRef s a -> a -> ST s ();
	writeSTRef (MkSTRef key) newa = MkST (modify (\dict -> case openKeyReplace key newa dict of
	{
		Just dict' -> dict';
		_ -> error "ref not found";
	}));
	
	modifySTRef :: forall s a. STRef s a -> (a -> a) -> ST s ();
	modifySTRef (MkSTRef key) amap = MkST (modify (\dict -> case openKeyModify key amap dict of
	{
		Just dict' -> dict';
		_ -> error "ref not found";
	}));
	
	stToIO :: ST RealWorld a -> IO a;
	stToIO = owToIO . stToOW;
}
