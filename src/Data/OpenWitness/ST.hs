-- | This is an approximate re-implementation of "Control.Monad.ST" and "Data.STRef" using open witnesses.
module Data.OpenWitness.ST
    (
    -- * The @ST@ Monad
      ST
    , runST
    , fixST
    -- * Converting @ST@ to @OW@ and @IO@
    , stToOW
    , RealWorld
    , stToIO
    -- * STRefs
    , STRef
    , newSTRef
    , readSTRef
    , writeSTRef
    , modifySTRef
    ) where

import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.OpenWitness
import Data.Witness.WitnessDict
import Prelude

type ST s = StateT (WitnessDict (OpenWitness s)) (OW s)

stToOW :: ST s a -> OW s a
stToOW st = evalStateT st emptyWitnessDict

runST :: (forall s. ST s a) -> a
runST st = runOW (stToOW st)

fixST :: (a -> ST s a) -> ST s a
fixST = mfix

type STRef s = OpenWitness s

newSTRef :: a -> ST s (STRef s a)
newSTRef a = do
    wit <- lift newOpenWitnessOW
    dict <- get
    put (witnessDictAdd wit a dict)
    return wit

readSTRef :: STRef s a -> ST s a
readSTRef key = do
    dict <- get
    case witnessDictLookup key dict of
        Just a -> return a
        _ -> error "ref not found"

writeSTRef :: forall s a. STRef s a -> a -> ST s ()
writeSTRef key newa = modify (witnessDictReplace key newa)

modifySTRef :: forall s a. STRef s a -> (a -> a) -> ST s ()
modifySTRef key amap = modify (witnessDictModify key amap)

stToIO :: ST RealWorld a -> IO a
stToIO = owToIO . stToOW
