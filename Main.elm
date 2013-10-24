module Main where

import LList as LList
import LList (LList, (:~:), (:~:~), (+~+))
import Lazy as Lazy

nats : LList Int
nats = 0 :~: (\() -> LList.map ((+) 1) nats)

-- Just like you'd write it in Haskell
-- There are ways to write it that are faster, but with memoizing thunks you don't need to
fibs : LList Int
fibs = 0 :~: Lazy.pure (1 :~: (\() -> LList.zipWith (+) fibs (LList.tail fibs)))

-- Prohibitively slow for large number of values taken.
main = asText . LList.take 25 <| fibs
