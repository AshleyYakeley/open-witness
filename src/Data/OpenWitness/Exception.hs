{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -fno-warn-unused-matches #-}
module Data.OpenWitness.Exception
(
    Exn, declexn, throw, catch
) where
{
    import Data.Kind;
    import Data.OpenWitness;
    import Data.Witness;
    import Data.Maybe;
    import Data.Typeable;
    import Language.Haskell.TH;
    import qualified Control.Exception as CE (Exception,throw,catch);
    import Prelude(IO,Show(..));

    -- | A key to match exceptions. The type variable is the data the exception carries.
    ;
    newtype Exn (e :: *) = MkExn (IOWitness e);

    instance TestEquality Exn where
    {
        testEquality (MkExn a) (MkExn b) = testEquality a b;
    };

    newtype ExnException = MkExnException (Any Exn) deriving Typeable;

    -- | Template Haskell function to declare 'Exn' exception keys.
    ;
    declexn :: TypeQ -> Q Exp;
    declexn te = [| MkExn $(iowitness te) |];

    instance Show ExnException where
    {
        show _ = "ExnException";
    };

    instance CE.Exception ExnException;

    throw :: Exn e -> e -> a;
    throw exn e = CE.throw (MkExnException (MkAny exn e));

    catch :: IO a -> Exn e -> (e -> IO a) -> IO a;
    catch foo exn catcher = CE.catch foo (\ex@(MkExnException cell) -> case matchAny exn cell of
    {
        Just e -> catcher e;
        _ -> CE.throw ex;
    });
}