module Main where
{
	import Data.OpenWitness.ST;
	import Data.OpenWitness.Exception;
	import Unsafe;
	import Prelude hiding (catch);
	
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
	
	intExn :: Exn Int;
	intExn = unsafeExnFromString "Main.intExn";
	stringExn :: Exn String;
	stringExn = unsafeExnFromString "Main.stringExn";
	
	showCatch :: IO a -> IO ();
	showCatch f = catch (catch (do
	{
		f;
		putStrLn "not caught";
	}) intExn (\i -> putStrLn ("caught intExn " ++ (show i))))
	stringExn (\s -> putStrLn ("caught stringExn " ++ (show s)));
	
	main :: IO ();
	main = do
	{
		putStrLn (show (runST stuff));
		putStrLn (show (coerce 'A' :: Int));
		showCatch (return "hello");
		showCatch (throw intExn 3);
		showCatch (throw stringExn "text");
		showCatch (throw intExn 67);
		showCatch (throw stringExn "str");
	};
}
