{-# OPTIONS -fno-warn-name-shadowing #-}
module Data.OpenWitness
(
    OpenWitness,
    RealWorld,IOWitness,newIOWitness,
    OW,newOpenWitnessOW,runOW,owToIO,
--    unsafeIOWitnessFromInteger,
--    unsafeIOWitnessFromString,
    iowitness
) where
{
    import Prelude;
    import Data.Kind;
    import Data.List;
    import System.Random;
    import Language.Haskell.TH;
    import Unsafe.Coerce;
    import System.IO.Unsafe (unsafePerformIO);
    import Control.Concurrent.MVar;
    import Control.Monad.Fix;
    import Control.Monad.Trans.State;
    import Data.Functor.Identity;
    import Data.Traversable;
    import Data.Hashable;
    import Data.Witness;
    import Data.Type.Heterogeneous;


    unsafeSameType :: HetEq a b;
    unsafeSameType = unsafeCoerce ReflH;

    -- | A witness type that can witness to any type.
    -- But values cannot be constructed; they can only be generated in 'IO' and certain other monads.
    ;
    newtype OpenWitness :: * -> forall (k :: *). k -> * where
    {
        MkOpenWitness :: Integer -> OpenWitness s a;
    };

    instance Eq (OpenWitness s a) where
    {
        (MkOpenWitness p) == (MkOpenWitness q) = p == q;
    };

    instance TestHetEquality (OpenWitness s) where
    {
        testHetEquality (MkOpenWitness ua) (MkOpenWitness ub) =
            if ua == ub then Just unsafeSameType else Nothing;
    };

    instance TestEquality (OpenWitness s) where
    {
        testEquality wa wb = fmap homoHetEq $ testHetEquality wa wb;
    };

    -- | The @s@ type for running 'OW' in 'IO'.
    ;
    data RealWorld;

    -- | An 'OpenWitness' for 'IO'.
    ;
    type IOWitness = OpenWitness RealWorld;

    ioWitnessSource :: MVar Integer;
    {-# NOINLINE ioWitnessSource #-};
    ioWitnessSource = unsafePerformIO (newMVar 0);

    -- | Generate a new 'IOWitness' in 'IO'.
    ;
    newIOWitness :: forall a. IO (IOWitness a);
    newIOWitness = do
    {
        val <- takeMVar ioWitnessSource;
        putMVar ioWitnessSource (val + 1);
        return (MkOpenWitness val);
    };

    type OWState = Integer;

    -- | A runnable monad in which 'OpenWitness' values can be generated.
    -- The @s@ parameter plays the same role as it does in 'ST', preventing 'OpenWitness' values from one run being used in another.
    ;
    newtype OW s a = MkOW (State OWState a) deriving (Functor,Applicative,Monad,MonadFix);

    -- | Run an 'OW' computation.
    ;
    runOW :: forall a. (forall s. OW s a) -> a;
    runOW uw = (\(MkOW st) -> evalState st 0) uw;

    -- | Generate a new 'OpenWitness' in 'OW'.
    ;
    newOpenWitnessOW :: forall s a. OW s (OpenWitness s a);
    newOpenWitnessOW = MkOW (StateT (\val -> Identity (MkOpenWitness val,val+1)));

    -- | Run an 'OW' computation in 'IO'.
    ;
    owToIO :: OW RealWorld a -> IO a;
    owToIO (MkOW st) = modifyMVar ioWitnessSource (\start -> let
    {
        (a,count) = runState st start;
    } in return (count,a));

    -- | An unsafe hack to generate 'IOWitness' values.
    -- This is safe if you use a different integer each time, and if @a@ is a single type.
    ;
    unsafeIOWitnessFromInteger :: Integer -> IOWitness a;
    unsafeIOWitnessFromInteger = MkOpenWitness;

    -- | An unsafe hack to generate 'IOWitness' values.
    -- This is safe if you use a different string each time (and 'hashString' doesn't collide), and if @a@ is a single type.
    ;
    unsafeIOWitnessFromString :: String -> IOWitness a;
    unsafeIOWitnessFromString = unsafeIOWitnessFromInteger . fromIntegral . hash;

    -- | Template Haskell function to declare 'IOWitness' values.
    ;
    iowitness :: TypeQ -> Q Exp;
    iowitness qt = do
    {
        t <- qt;
        _ <- forM (freevarsType t) (\v -> reportError ("Type variable "++(show v)++" free in iowitness type"));
        l <- location;
        rnd :: Integer <- runIO randomIO;
        key <- return ((showLoc l) ++ "/" ++ (show rnd));
        keyExpr <- return (LitE (StringL key));
        untypedWitExpr <- [| unsafeIOWitnessFromString $(return keyExpr) |];
        [| $(return untypedWitExpr) :: IOWitness $(return t) |];
    } where
    {
        showLoc :: Loc -> String;
        showLoc l = (loc_filename l) ++ "=" ++ (loc_package l) ++ ":" ++ (loc_module l) ++ (show (loc_start l)) ++ (show (loc_end l));

        unionList :: (Eq a) => [[a]] -> [a];
        unionList [] = [];
        unionList (l:ls) = union l (unionList ls);

        bindingvarTVB :: TyVarBndr -> Name;
        bindingvarTVB (PlainTV n) = n;
        bindingvarTVB (KindedTV n _) = n;

        freevarsType :: Language.Haskell.TH.Type -> [Name];
        freevarsType (ForallT tvbs ps t) =
         (union (freevarsType t) (unionList (fmap freevarsType ps))) \\ (fmap bindingvarTVB tvbs);
        freevarsType (VarT name) = [name];
        freevarsType (AppT t1 t2) = union (freevarsType t1) (freevarsType t2);
        freevarsType (SigT t _) = freevarsType t;
        freevarsType _ = [];
    };
}
