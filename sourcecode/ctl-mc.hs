{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A simple CTL model checker (with the memoization trick).

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.List
import           Data.Maybe (Maybe (..), fromJust, fromMaybe)
import           Control.Monad
import           Control.Monad.State (State (..), get, put, evalState)

type Gen a = (a -> a)

fix :: Gen a -> a
fix f = f (fix f)

type Memo a b = State (M.Map a b)

query :: Ord a => a -> Memo a b (Maybe b)
query k = fmap (M.lookup k) get

store :: Ord a => a -> b -> Memo a b ()
store k v = fmap (M.insert k v) get >>= put

data CTL a = TT | FF
    | Atomic a
    | Not (CTL a)
    | And (CTL a) (CTL a)
    | Or (CTL a) (CTL a)
    | Imp (CTL a) (CTL a) -- p ==> q = Not p `Or` q
    | AX (CTL a)           -- AX p: p holds for all successor states.
    | EX (CTL a)           -- EX p: holds for some successor states.
    | AU (CTL a) (CTL a)   -- p `AU` q: p holds for all path until q holds.
    | EU (CTL a) (CTL a)    -- p `EU` q: p holds for all path until q holds.
    | AF (CTL a) -- AF p = AU TT p: p will hold for all path in the future.
    | EF (CTL a) -- EF p = EU TT p: p will hold for some path in the future.
    | AG (CTL a) -- AG p = Not (EF (Not p)): p always holds for all path.
    | EG (CTL a) -- EG p = Not (AF (Not p)): p always holds for some path.
    deriving (Show, Eq, Ord)

-- | successor function on the state machine.
type Succ st = st -> [st]

-- | Interpretation of atomic formulas.
type Interp p st = p -> st -> Bool

andM, orM :: Monad m => m Bool -> m Bool -> m Bool
andM = liftM2 (&&)
orM  = liftM2 (||)

notM :: Monad m => m Bool -> m Bool
notM = fmap not

anyM, allM :: (Monad m, Traversable t) => (s -> m Bool) -> t s -> m Bool
anyM fn = fmap or . mapM fn
allM fn = fmap and . mapM fn

memoize :: (Ord a, Ord b) => c -> Gen (a -> b -> Memo (a, b) c c)
memoize visited self s t = do
    cached <- query (s, t)
    case cached of
      Just v  -> return v
      Nothing -> store (s, t) visited >> self s t >>= \v -> store (s, t) v >> return v

data Model s a = Model { nodes :: S.Set s
                       , successor :: s -> [s]
                       , interpreter :: a -> s -> Bool }

mc :: forall a s. (Ord a, Ord s) => Model s a -> s -> CTL a -> Bool
mc (Model sts succ interp) node constraint = evalState task M.empty where
    task = fix (checkCTL . memoize False) constraint node

    checkCTL :: Monad m => Gen (CTL a -> s -> m Bool)
    checkCTL self formula st = check formula where
        check TT         = return True
        check FF         = return False
        check (Atomic p) = return $ interp p st
        check (Not p)    = notM $ self p st
        check (And p q)  = self p st `andM` self q st
        check (Or p q)   = self p st `orM` self q st
        check (Imp p q)  = check $ Not p `Or` q
        check (AX p)     = allM (self p) (succ st)
        check (EX p)     = anyM (self p) (succ st)
        check (AU p q)   = self q st `orM` (self p st `andM` allM (self formula) (succ st))
        check (EU p q)   = self q st `orM` (self p st `andM` anyM (self formula) (succ st))
        check (AF p)     = check $ AU TT p
        check (EF p)     = check $ EU TT p
        check (AG p)     = check $ Not (EF (Not p))
        check (EG p)     = check $ Not (AF (Not p))

-- Expected output:
--
-- [ [True,False,False,True]
-- , [False,False,False,False]
-- , [True,False,True,True]
-- , [False,True,False,False]]
--
example1 :: IO ()
example1 = do
    let res = [[ch s pred | s <- S.toList sts] | pred <- preds]
    print res
    where
        sts = S.fromList ["S0", "S1", "S2", "S3"]

        succFn k = fromMaybe [] $ lookup k
            [ ("S0", ["S1", "S3"])
            , ("S1", ["S1", "S2"])
            , ("S2", ["S3"])
            , ("S3", ["S0"])]

        labelFn pred st = elem pred $ fromMaybe [] $ lookup st
            [ ("S0", "pq")
            , ("S1", "r")
            , ("S2", "pt")
            , ("S3", "qr")]

        ch = mc $ Model sts succFn labelFn

        preds = [EG (Atomic 'q'), EG (Not (Atomic 'r')), EG (Or (Atomic 'p') (Atomic 'q')), EG (Atomic 'r')]

-- Expected output:
--
-- [ [False,False,False,False]
-- , [True,False,True,True]
-- , [False,True,False,False]
-- , [True,False,False,True]
-- , [True,True,False,True]
-- , [True,True,False,True]]
example2:: IO ()
example2 = do
    let res = [[ch s pred | s <- S.toList sts] | pred <- preds]
    print res
    where
        sts = S.fromList ["Red", "Green", "Yellow", "Orange"]

        labelFn = (==)

        succFn k = fromMaybe [] $ lookup k
            [ ("Red", ["Green"])
            , ("Green", ["Yellow"])
            , ("Yellow", ["Red"])]

        ch = mc $ Model sts succFn labelFn

        preds = [ EF (Atomic "Blue")
                , EU TT (Atomic "Red")
                , EU (Atomic "Green") (Atomic "Orange")
                , Not (AU (Not (Atomic "Yellow")) (Atomic "Red"))
                , Not (EU (Not (Atomic "Yellow")) (Atomic "Red"))
                , Not (EU (Not (Atomic "Yellow")) (Atomic "Red"))
                ]

-- Expected output:
--
-- [ [False,False,False,False]
-- , [True,True,True,True]
-- , [True,True,False,False]
-- , [True,False,False,True]
-- , [False,False,False,True]
-- , [False,False,False,True]]
example3:: IO ()
example3 = do
    let res = [[ch s pred | s <- S.toList sts] | pred <- preds]
    print res
    where
        sts = S.fromList ["Red", "Green", "Yellow", "Orange"]

        labelFn = (==)

        succFn k = fromMaybe [] $ lookup k
            [ ("Red", ["Green"])
            , ("Green", ["Yellow", "Orange"])
            , ("Orange", ["Red"])
            , ("Yellow", ["Red"])]

        ch = mc $ Model sts succFn labelFn

        preds = [ EF (Atomic "Blue")
                , EU TT (Atomic "Red")
                , EU (Atomic "Green") (Atomic "Orange")
                , Not (AU (Not (Atomic "Yellow")) (Atomic "Red"))
                , Not (EU (Not (Atomic "Yellow")) (Atomic "Red"))
                , Not (EU (Not (Atomic "Yellow")) (Atomic "Red"))
                ]

main = do
    example1
    example2
    example3

