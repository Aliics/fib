# fib in Haskell

I wanted to showcase how easy it is to optimize in Haskell if you know where your algorithms are slow.

On my machine, I can easily calculate to the 100,000th fib number in 0.5seconds. Classically, this is done
with memoization because classic `f (n - 1) + f (n - 2)` is a very redundant algorithm.

Results from running `main`:
```
"fibNaive 1" time taken: 0.000003352s
"fibNaive 10" time taken: 0.000068725s
"fibNaive 32" time taken: 1.409242054s
"fibMemoList 32" time taken: 0.000025074s
"fibMemoList 100" time taken: 0.000060484s
"fibMemoList 1_000" time taken: 0.00365452s
"fibMemoList 10_000" time taken: 0.381131997s
"fibMemoList 32_000" time taken: 5.572740374s
"fibMemoArr 32" time taken: 0.000045398s
"fibMemoArr 100" time taken: 0.000054896s
"fibMemoArr 1_000" time taken: 0.000504893s
"fibMemoArr 10_000" time taken: 0.006968905s
"fibMemoArr 32_000" time taken: 0.167217407s
"fibMemoArr 100_000" time taken: 0.413188751s
"fibMemoArr 320_000" time taken: 2.918897316s
```

# TL;DR

Memoizing is really easy to implement using infinite lists, and an array can also be easily tacked on top
of that for faster indexing.

Moving from
```hs
(map f [0 ..] !!)
```

to an implement with an array

```hs
-- Qualified A used for clarity.
(fmap f (A.listArray (0, max) [0 ..]) A.!)
```

in normally pretty simple.
