module Object where
{
	import Data.Witness;
	import Data.OpenWitness;

	data Object s = forall a. MkObject (OpenWitness s a) a;

	matchObject :: OpenWitness s b -> Object s -> Maybe b;
	matchObject uqb (MkObject uqa a) = do
	{
		MkEqualType <- matchWitness uqa uqb;
		return a;
	};

	makeConversions :: OpenWitness s b -> (b -> Object s,Object s -> Maybe b);
	makeConversions wit = (MkObject wit,matchObject wit);

	getConversions :: OW s (b -> Object s,Object s -> Maybe b);
	getConversions = do
	{
		wit <- newOpenWitnessOW;
		return (makeConversions wit);
	};
}
