module Philosopher (simulate) where

import System.Random
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

type Semaphore = TVar Bool

newSem :: Bool -> IO Semaphore
newSem val = newTVarIO val

p :: Semaphore -> STM ()
p sem = do
    val <- readTVar sem
    if val
        then writeTVar sem False
        else retry

v :: Semaphore -> STM ()
v sem = writeTVar sem True

type Buffer a = TVar [a]

newBuffer :: IO (Buffer a)
newBuffer = newTVarIO []

put :: Buffer a -> a -> STM ()
put buffer item = do
    xs <- readTVar buffer
    writeTVar buffer (xs ++ [item])

get :: Buffer a -> STM a
get buffer = do
    xs <- readTVar buffer
    case xs of
        [] -> retry
        (item:rest) -> do
            writeTVar buffer rest
            return item

simulate n = do
    forks <- replicateM n (newSem True)
    outputBuffer <- newBuffer
    mapM_ (\i -> forkIO (philosopher i outputBuffer (forks !!  i) (forks !! ((i+1) `mod` n)))) [0..n-1]
    output outputBuffer

output buffer = do
    (atomically $ get buffer) >>= putStrLn
    output buffer

philosopher :: Int -> Buffer String -> Semaphore -> Semaphore -> IO ()
philosopher n out left right = do
    -- thinking.
    atomically $ put out ("Philosopher " ++ show n ++ " is thinking.")
    randomDelay

    atomically $ do
        p left
        p right

    -- eating.
    atomically $ put out ("Philosopher " ++ show n ++ " is eating.")
    randomDelay

    atomically $ do
        v left
        v right

    -- continue.
    philosopher n out left right

    where randomDelay = randomRIO (100000,500000) >>= threadDelay
