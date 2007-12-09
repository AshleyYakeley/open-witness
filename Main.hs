module Main where
{
	import Data.OpenWitness.ST;
	import Unsafe;
	
	stuff :: ST s [Int];
	stuff = do
	{
		ra <- newSTRef 3;
		a1 <- readSTRef ra;
		rb <- newSTRef 4;
		b1 <- readSTRef rb;
		writeSTRef ra 5;
		a2 <- readSTRef ra;
		writeSTRef rb 7;
		b2 <- readSTRef rb;
		return [a1,b1,a2,b2];
	};
	
	main :: IO ();
	main = do
	{
		putStrLn (show (runST stuff));
		putStrLn (show (coerce 'A' :: Int));
	};
}
