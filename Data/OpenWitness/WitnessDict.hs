module Data.OpenWitness.WitnessDict
(
	OpenDict,emptyOpenDict,
	OpenKey,openKeyLookup,openKeyReplace,openKeyModify,openKeyNew
) where
{
	import Data.Witness;
	import Data.OpenWitness;
	import Control.Monad.State;
	
	data Cell s = forall a. MkCell (OpenWitness s a) a;
	
	newtype OpenDict s = MkOpenDict [Cell s];
	
	emptyOpenDict :: OpenDict s;
	emptyOpenDict = MkOpenDict[];
	
	newtype OpenKey s a = MkOpenKey (OpenWitness s a) deriving Eq;
	
	findM :: (a -> Maybe b) -> [a] -> Maybe b;
	findM match (a:as) = case match a of
	{
		mb@(Just _) -> mb;
		_ -> findM match as;
	};
	findM _ _ = Nothing;
	
	openKeyLookup :: OpenKey s a -> OpenDict s -> Maybe a;
	openKeyLookup (MkOpenKey wit) (MkOpenDict cells) = findM (\(MkCell cwit ca) -> do
	{
		smt <- matchWitness cwit wit;
		return (mapSameType smt ca);
	}) cells;

	openKeyReplace :: OpenKey s a -> a -> OpenDict s -> Maybe (OpenDict s);
	openKeyReplace (MkOpenKey wit) newa (MkOpenDict cc) = fmap MkOpenDict (mapCells cc) where
	{
		mapCells (cell@(MkCell cwit _):cells) = case matchWitness wit cwit of
		{
			Just smt -> Just ((MkCell cwit (mapSameType smt newa)):cells);
			_ -> fmap (cell :) (mapCells cells);
		};
		mapCells _ = Nothing;
	};

	openKeyModify :: OpenKey s a -> (a -> a) -> OpenDict s -> Maybe (OpenDict s);
	openKeyModify (MkOpenKey wit) amap (MkOpenDict cc) = fmap MkOpenDict (mapCells cc) where
	{
		mapCells (cell@(MkCell cwit olda):cells) = case matchWitness wit cwit of
		{
			Just smt -> Just ((MkCell cwit (mapSameType smt (amap (mapSameType (reverseSameType smt) olda)))):cells);
			_ -> fmap (cell :) (mapCells cells);
		};
		mapCells _ = Nothing;
	};
	
	openKeyNew :: OpenWitness s a -> a -> OpenDict s -> (OpenDict s,OpenKey s a);
	openKeyNew wit a (MkOpenDict cells) = (MkOpenDict ((MkCell wit a):cells),MkOpenKey wit);
}
