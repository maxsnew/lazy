module Main where

import Lazy (lazy)
import Lazy.Stream (Stream)
import Lazy.Stream as S

nats : Stream Int
nats = S.iterate (\n -> n + 1) 0

-- Just like you'd write it in Haskell
{- This is equivalent to a memoized version of the following function:

```haskell
fib = fib_help 0 1

fib_help m n 0 = m
fib_help m n k = fib_help n (m + n) (k - 1)
```
-}
fibs : Stream Int
fibs =
  S.cons 0 <| \() ->
  S.cons 1 <| \() ->
  S.zipWith (+) fibs (S.tail fibs)

-- fast!
main = asText << S.take 1000 <| fibs
