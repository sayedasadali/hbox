-- Concurrent FIFO Queue
-- =====================
-- 
-- <div class="hidden">
-- 
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, OverlappingInstances, FlexibleInstances #-}
module FIFO where
import qualified Data.Map as Map 
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Test.QuickCheck 
import Control.Monad (forM, forM_)
import Data.List (transpose, intercalate)
-- 
import qualified Data.Set as Set
import Control.Applicative ((<$>))
import qualified Text.PrettyPrint as PP
-- 
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Exit
import System.IO.Unsafe
import Data.IORef
-- 
quickCheckN :: Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n}
-- 
-- </div>
-- 
-- 
-- In this question, we'll develop a concurrently-accessible
-- first-in, first-out (FIFO) queue with a fixed capacity, 
-- called a finite channel.[^channelnote] Your finite channel 
-- will behave as follows: If a read occurs when the queue 
-- is empty, the reader should block until an item becomes 
-- available. Similarly, if a write occurs when the queue is full 
-- (i.e., the number of items in the queue is the capacity 
-- specified when the queue was created), the writer should 
-- block until an item is removed from the queue.
-- 
-- [^channelnote]: This question is based on homeworks 10 and 11 from
-- Benjamin Pierce's 
-- [Advanced Programming](http://www.cis.upenn.edu/~bcpierce/courses/552-2008/index.html)
-- class at UPenn.
-- 
-- Before defining any operations on your finite channel, you need to
-- change the representation of finite channels from the following
-- obviously incorrect one:
-- 
data FiniteChan a = Chan Int (TVar ([a], Int))
-- 
-- Next, define an operation for creating a finite channel of a
-- particular capacity:
-- 
newFiniteChan :: Int -> IO (FiniteChan a)
newFiniteChan capacity = do
                           tv <- atomically $ newTVar ([], 0)
                           return $ Chan capacity tv
-- 
-- Next, define the operation for reading from the queue:
-- 
readFiniteChan :: FiniteChan a -> IO a
readFiniteChan (Chan _ tv) = 
  atomically $ do
    (xs, n) <- readTVar tv
    case n of
      0         -> retry
      otherwise -> do writeTVar tv (tail xs, n - 1)
                      return $ head xs
-- 
-- Remember that reads should block in the case where the channel is
-- empty.
-- 
-- Finally, define the operation for writing to the queue:
-- 
writeFiniteChan :: FiniteChan a -> a -> IO ()
writeFiniteChan (Chan cap tv) x =
  atomically $ do
    (xs, n) <- readTVar tv
    if n == cap
      then retry
      else do writeTVar tv (xs ++ [x], n + 1)
-- 
-- Remember that writes should block in the case where the channel is at
-- capacity.
-- 
-- Below are some tests that exercise your channel abstraction. You should
-- run the `testFiniteChannel' function at the very end to ensure that all
-- of the tests pass.
-- 
-- First, we define a debugging output function for tracing the execution
-- of the tests. Uncomment out the second version if you need more information
-- to diagnose errors.
-- 
dbg :: Monad m => t -> m ()
dbg _ = return ()
 --dbg s = do id <- myThreadId;  putStrLn $ "[" ++ (show id) ++ "] " ++ s
-- 
-- Various test parameters and utilities follow.
-- 
rounds, delayUnit :: Int
rounds = 1000
delayUnit = 1000*200 -- 0.2 seconds

assert :: Bool -> [Char] -> IO ()
assert b s = 
  if b then return () else do
  putStrLn $ "assertion failed: " ++ s
  exitWith (ExitFailure 1)

beginTest :: [Char] -> IO ()
beginTest s = putStrLn $ "Starting test procedure: " ++ s

endTest :: IO ()
endTest = do
  putStrLn "Test passed"
  putStrLn ""

newFC :: Int -> IO (FiniteChan a)
newFC x = do
  dbg $ "newFiniteChan " ++ (show x) ++ " called"
  c <- newFiniteChan x
  dbg $ "newFiniteChan " ++ (show x) ++ " returned"
  return c

readFC :: Show b => FiniteChan b -> IO b
readFC c = do
  dbg $ "readFiniteChan called"
  x <- readFiniteChan c
  dbg $ "readFiniteChan returned, result=" ++ (show x)
  return x

writeFC :: Show a => FiniteChan a -> a -> IO ()
writeFC c x = do
  dbg $ "writeFiniteChan " ++ (show x) ++ " called"
  writeFiniteChan c x
  dbg $ "writeFiniteChan " ++ (show x) ++ " returned"
-- 
-- The first test fills and empties the queue twice, checking that FIFO
-- order is respected.
-- 
test1 :: IO [()]
test1 = forM [1..5] $ \i -> test1a i

test1a :: Int -> IO ()
test1a x = do
  beginTest $ "test1:"++(show x)
  fc <- newFC x
  forM [1..x] $ \i-> writeFC fc i
  forM [1..x] $ \i-> do
    j <- readFC fc
    assert (i==j) "FIFO order not respected"

  forM [x+1..2*x] $ \i-> writeFC fc i
  forM [x+1..2*x] $ \i-> do
    j <- readFC fc
    assert (i==j) "FIFO order not respected"
  endTest
-- 
-- The second test is a simple two-thread producer/consumer setup, again
-- testing for FIFO semantics.
-- 
test2 :: IO [()] 
test2 = forM [1..5] $ \i -> test2a i

test2a :: Int -> IO ()
test2a size = do
  beginTest $ "test2:"++(show size)
  fc <- newFC size
  forkIO (producer fc)
  consumer fc
  forkIO (consumer fc)
  producer fc
  endTest
 where
  producer fc = do
    forM [1..rounds] $ \i-> do
      writeFC fc i
    return ()
  consumer fc = do
    forM [1..rounds] $ \i-> do
      j <- readFC fc
      assert(i==j) "FIFO order not respected"
    return ()
-- 
-- The third test checks that, if the consumer is slow, the queue will
-- always be full when it's read, and also that the producer is not
-- allowed to insert more items into the queue than its capacity should
-- allow.
-- 
test3 :: IO [()]
test3 = forM [1..5] $ \i -> test3a i

test3a :: Int -> IO ()
test3a size = do
  beginTest $ "test3:"++(show size)
  fc <- newFC size
  forkIO (producer fc)
  consumer fc
  endTest
 where
  counter = unsafePerformIO (newMVar 0)
  producer fc = do
    forM [1..rounds] $ \i-> do
      writeFC fc i
      modifyMVar_ counter $ \c-> do
        -- putStrLn $ (show c) ++ "<" ++ (show size)
        assert (c<size) "Queue size not within limit"
        return (c+1)
    return ()
  consumer fc = do
    forM [1..10] $ \i-> do
      threadDelay delayUnit
      modifyMVar_ counter $ \c-> do
         -- putStrLn $ (show c) ++ "<=" ++ (show size)
         assert (c==size) "Queue should always be full with a slow reader"
         return (c-1)
      j <- readFC fc
      assert(i==j) ""
    return ()
-- 
-- The fourth test is like the third, except its checks that, with a slow
-- producer, the queue is always empty when it's written to.
-- 
test4 :: IO [()]
test4 = forM [1..5] $ \i -> test4a i

test4a :: Int -> IO ()
test4a size = do
  beginTest $ "test4:"++(show size)
  fc <- newFC size
  forkIO (consumer fc)
  producer fc
  endTest
 where
  counter = unsafePerformIO (newMVar 0)
  producer fc = do
    forM [1..5] $ \i-> do
      threadDelay delayUnit
      modifyMVar_ counter $ \c-> do
        -- putStrLn $ (show c) ++ "<" ++ (show size)
        assert (c==0) "Queue should always be empty with a slow writer"
        return (c+1)
      writeFC fc i
    return ()
  consumer fc = do
    forM [1..rounds] $ \i-> do
      j <- readFC fc
      assert(i==j) ""
      modifyMVar_ counter $ \c-> do
         -- putStrLn $ (show c) ++ "<=" ++ (show size)
         assert (c<=size) "Queue size not within limit"
         return (c-1)
    return ()
-- 
-- The fifth test checks the behavior of multiple producers and
-- consumers.
-- 
test5 :: IO [()]
test5 = forM [1..5] $ \i -> test5a i

test5a :: Int -> IO ()
test5a size = do
  beginTest $ "test5:"++(show size)
  fc1 <- newFC size
  fc2 <- newFC 1
  forM [1..nums] $ \_ -> forkIO (producer fc1)
  forM [1..nums] $ \_ -> forkIO (consumer fc1 fc2)
  s <- newIORef 0
  forM [1..(nums*rounds)] $ \_ -> do
    i <- readFC fc2
    modifyIORef s (+i)
  result <- readIORef s
  assert (result== nums * (sum [1..rounds])) "total sent <> total received"
  endTest
 where
  nums = 10 
  producer fc = do
    forM [1..rounds] $ \i -> writeFC fc i
    return ()
  consumer fc1 fc2 = do
    forM [1..rounds] $ \_ -> do
      i <- readFC fc1
      writeFC fc2 i
    return ()
-- 
-- The sixth test is like the third, this time with multiple producer
-- threads.
-- 
test6 :: IO [()]
test6 = forM [1..5] $ \i -> test6a i

test6a :: Int -> IO ()
test6a size = do
  beginTest $ "test6:"++(show size)
  fc <- newFC size
  forM [1..5] $ \_ -> forkIO (producer fc)
  consumer fc
  endTest
 where
  counter = unsafePerformIO (newMVar 0)
  producer fc = do
    forM [1..rounds] $ \i-> do
      writeFC fc i
      modifyMVar_ counter $ \c-> do
        -- putStrLn $ (show c) ++ "<" ++ (show size)
        assert (c<size) "queue size not within limit"
        return (c+1)
    return ()
  consumer fc = do
    forM [1..10] $ \_-> do
      threadDelay delayUnit
      modifyMVar_ counter $ \c-> do
         -- putStrLn $ (show c) ++ "<=" ++ (show size)
         assert (c==size) "queue should always be full with slow reader"
         return (c-1)
      readFC fc
    return ()
-- 
-- The final test is like the fourth, but with multiple consumer threads.
-- 
test7 :: IO [()]
test7 = forM [1..5] $ \i -> test7a i

test7a :: Int -> IO ()
test7a size = do
  beginTest $ "test7:"++(show size)
  fc <- newFC size
  forM [1..5] $ \_-> forkIO (consumer fc)
  producer fc
  endTest
 where
  counter = unsafePerformIO (newMVar 0)
  producer fc = do
    forM [1..10] $ \i-> do
      threadDelay delayUnit
      modifyMVar_ counter $ \c-> do
        -- putStrLn $ (show c) ++ "<" ++ (show size)
        assert (c==0) "queue should always be empty with slow writer"
        return (c+1)
      writeFC fc i
    return ()
  consumer fc = do
    forM [1..rounds] $ \_-> do
      _ <- readFC fc
      modifyMVar_ counter $ \c-> do
         -- putStrLn $ (show c) ++ "<=" ++ (show size)
         assert (c<=size) "queue size not within limit"
         return (c-1)
    return ()

testFiniteChannel :: IO [()]
testFiniteChannel = do
  test1
  test2
  test3
  test4
  test5  
  test6
  test7
-- 
-- 
