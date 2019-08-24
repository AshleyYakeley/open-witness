module Data.OpenWitness.Witnessed
    ( Witnessed
    , unWitnessed
    , newWitnessed
    , WitnessedIORef
    , newWitnessedIORef
    ) where

import Data.IORef
import Data.OpenWitness
import Data.Type.Equality
import Prelude

data Witnessed f a =
    MkWitnessed (IOWitness a)
                (f a)

unWitnessed :: Witnessed f a -> f a
unWitnessed (MkWitnessed _ fa) = fa

instance TestEquality (Witnessed f) where
    testEquality (MkWitnessed wa _) (MkWitnessed wb _) = testEquality wa wb

newWitnessed :: f a -> IO (Witnessed f a)
newWitnessed fa = do
    wit <- newIOWitness
    return $ MkWitnessed wit fa

type WitnessedIORef = Witnessed IORef

newWitnessedIORef :: a -> IO (WitnessedIORef a)
newWitnessedIORef a = do
    ref <- newIORef a
    newWitnessed ref
