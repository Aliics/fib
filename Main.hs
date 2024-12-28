module Main where

import Control.DeepSeq (NFData, deepseq)
import Data.Array qualified as A
import Data.Time (diffUTCTime, getCurrentTime)

main :: IO ()
main = do
  -- Naive fib implementation, which very quickly becomes super slow.
  compute "fibNaive 1" (fibNaive 1)
  compute "fibNaive 10" (fibNaive 10)
  compute "fibNaive 32" (fibNaive 32)

  -- Same implementation but memoized using a list.
  -- These are linked lists, and (!!) is O(n).
  compute "fibMemoList 32" (fibMemoList 32)
  compute "fibMemoList 100" (fibMemoList 100)
  compute "fibMemoList 1_000" (fibMemoList 1_000)
  compute "fibMemoList 10_000" (fibMemoList 10_000)
  compute "fibMemoList 32_000" (fibMemoList 32_000)

  -- Now we memoize with an array.
  -- Indexing is now super fast. We do create an intermediate list to fill.
  -- And our bounds must also be known.
  compute "fibMemoArr 32" (fibMemoArr 32)
  compute "fibMemoArr 100" (fibMemoArr 100)
  compute "fibMemoArr 1_000" (fibMemoArr 1_000)
  compute "fibMemoArr 10_000" (fibMemoArr 10_000)
  compute "fibMemoArr 32_000" (fibMemoArr 32_000)
  compute "fibMemoArr 100_000" (fibMemoArr 100_000)
  compute "fibMemoArr 320_000" (fibMemoArr 320_000)

-- Compute somethingand show how long it took.
compute :: (NFData a) => String -> a -> IO ()
compute label x = do
  s <- getCurrentTime
  n <- x `deepseq` getCurrentTime
  putStrLn $ mconcat ["\"", label, "\" ", "time taken: ", show $ diffUTCTime n s]

-- Generic fib application.
fib :: (Int -> Integer) -> Int -> Integer
fib _ 0 = 1
fib _ 1 = 1
fib f n = f (n - 1) + f (n - 2)

fibNaive :: Int -> Integer
fibNaive = fib fibNaive

fibMemoList :: Int -> Integer
fibMemoList = (map (fib fibMemoList) [0 ..] !!)

fibMemoArr :: Int -> Integer
fibMemoArr n =
  fibArr n
 where
  max = n

  fibArr = (fmap (fib fibArr) (A.listArray (0, max) [0 ..]) A.!)
