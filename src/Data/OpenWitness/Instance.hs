module Data.OpenWitness.Instance where
{
    import Data.Kind;
    import Data.Constraint;
    import Data.Type.Heterogeneous;
    import Data.OpenWitness.TypeRep;

    data Instance = forall (t :: Constraint). t => MkInstance (TypeRep t);

    findInstance :: [Instance] -> TypeRep t -> Maybe (Dict t);
    findInstance [] _ = Nothing;
    findInstance (MkInstance ti:ii) t = case testHetEquality ti t of
    {
        Just ReflH -> Just Dict;
        Nothing -> findInstance ii t;
    };
}
