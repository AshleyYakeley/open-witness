module Data.OpenWitness.OpenDict
(
	OpenDict,emptyOpenDict,
	openKeyLookup,openKeyReplace,openKeyModify,openKeyAdd
) where
{
	import Data.Witness;
	import Control.Monad.State;
	
	newtype OpenDict w = MkOpenDict [Any w];
	
	emptyOpenDict :: OpenDict w;
	emptyOpenDict = MkOpenDict[];
	
	findM :: (a -> Maybe b) -> [a] -> Maybe b;
	findM match (a:as) = case match a of
	{
		mb@(Just _) -> mb;
		_ -> findM match as;
	};
	findM _ _ = Nothing;
	
	openKeyLookup :: (Witness w) => w a -> OpenDict w -> Maybe a;
	openKeyLookup wit (MkOpenDict cells) = findM (\(MkAny cwit ca) -> do
	{
		MkSameType <- matchWitness cwit wit;
		return ca;
	}) cells;
	
	replaceHelper :: (forall a. w a -> Maybe (a -> a)) -> Any w -> Maybe (Any w);
	replaceHelper wmaa (MkAny wit a) = case wmaa wit of
	{
		Just aa -> Just $ MkAny wit (aa a);
		_ -> Nothing;
	};

	openKeyModify :: (Witness w) => w a -> (a -> a) -> OpenDict w -> Maybe (OpenDict w);
	openKeyModify wit amap (MkOpenDict cc) = fmap MkOpenDict (mapCells cc) where
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

	openKeyReplace :: (Witness w) => w a -> a -> OpenDict w -> Maybe (OpenDict w);
	openKeyReplace wit newa = openKeyModify wit (const newa);
	
	openKeyAdd :: w a -> a -> OpenDict w -> OpenDict w;
	openKeyAdd wit a (MkOpenDict cells) = MkOpenDict ((MkAny wit a):cells);
}
