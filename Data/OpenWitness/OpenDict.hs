module Data.OpenWitness.OpenDict
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
		MkSameType <- matchWitness cwit wit;
		return ca;
	}) cells;
	
	replaceHelper :: (forall a. OpenWitness s a -> Maybe (a -> a)) -> Cell s -> Maybe (Cell s);
	replaceHelper wmaa (MkCell wit a) = case wmaa wit of
	{
		Just aa -> Just $ MkCell wit (aa a);
		_ -> Nothing;
	};

	openKeyModify :: OpenKey s a -> (a -> a) -> OpenDict s -> Maybe (OpenDict s);
	openKeyModify (MkOpenKey wit) amap (MkOpenDict cc) = fmap MkOpenDict (mapCells cc) where
	{
		mapCells (cell:cells) = case replaceHelper (\cwit -> do
		{
			MkSameType <- matchWitness wit cwit;
			return amap;
		}) cell of
		{
			Just newcell -> Just (newcell:cells);
			_ -> fmap (cell :) (mapCells cells);
		};
		mapCells _ = Nothing;
	};

	openKeyReplace :: OpenKey s a -> a -> OpenDict s -> Maybe (OpenDict s);
	openKeyReplace key newa = openKeyModify key (const newa);
	
	openKeyNew :: OpenWitness s a -> a -> OpenDict s -> (OpenDict s,OpenKey s a);
	openKeyNew wit a (MkOpenDict cells) = (MkOpenDict ((MkCell wit a):cells),MkOpenKey wit);
}
