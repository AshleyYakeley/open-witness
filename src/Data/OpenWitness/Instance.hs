module Data.OpenWitness.Instance where

import Data.Constraint
import Data.OpenWitness.TypeRep
import Data.Type.Heterogeneous
import Prelude

data Instance =
    forall (t :: Constraint). t => MkInstance (TypeRep t)

findInstance :: [Instance] -> TypeRep t -> Maybe (Dict t)
findInstance [] _ = Nothing
findInstance (MkInstance ti:ii) t =
    case testHetEquality ti t of
        Just ReflH -> Just Dict
        Nothing -> findInstance ii t
