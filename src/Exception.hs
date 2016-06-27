module Exception where
{
	import Data.OpenWitness;
	import Control.Monad.Error;
	
	data DynException = forall a. MkDynException (IOWitness a) a;
	
	throw :: IOWitness a -> a -> IO b
}
