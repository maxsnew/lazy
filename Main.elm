module Main where

import Lazy (lazy)
import Lazy.Stream (Stream)
import Lazy.Stream as S

nats : Stream Int
nats = S.iterate (\n -> n + 1) 0

-- Just like you'd write it in Haskell
-- There are ways to write it that are faster, but with memoizing thunks you don't need to
fibs : Stream Int
fibs =
  S.cons 0 <| \() ->
  S.cons 1 <| \() ->
  S.zipWith (+) fibs (S.tail fibs)

-- Much faster, but is it because it's avoiding recomputation or just making fewer function calls?
fibs' : Stream Int
fibs' = S.unfold (\(m, n) -> (m, (n, m + n))) (0, 1)

-- Prohibitively slow for large number of values taken.
main = asText . S.take 100 <| fibs'

-- This is way faster
-- main = asText . S.take 25 <| fibs'
