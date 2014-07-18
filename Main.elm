module Main where

import Lazy (lazy)
import Lazy.Stream (Stream)
import Lazy.Stream as S

nats : Stream Int
nats = S.iterate (\n -> n + 1) 0

-- Just like you'd write it in Haskell

{- Without memoized lazy values this is equivalent to the naive
   recursive version:

```Haskell
fib 0 = 0
fib 1 = 1
fib n = (fib n) + (fib (n - 1))
```
-}
fibs : Stream Int
fibs =
  S.cons 0 <| \() ->
  S.cons 1 <| \() ->
  S.zipWith (+) fibs (S.tail fibs)

{- Equivalent to this smarter version of fib:
```Haskell
fib = fib_help 0 1

fib_help m n 0 = m
fib_help m n k = fib_help n (m + n) (k - 1)
```
-}
fibs' : Stream Int
fibs' = S.unfold (\(m, n) -> (m, (n, m + n))) (0, 1)

-- Both fast!
-- main = asText . S.take 100 <| fibs'
main = asText . S.take 25 <| fibs
