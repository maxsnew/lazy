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

-- Much faster, but is it because it's avoiding recomputation or just making fewer function calls?
fibs_iter' : Int -> Int -> LList Int
fibs_iter' n m = n :~: (\() -> (fibs_iter' m (n + m)))

fibs' : LList Int
fibs' = fibs_iter' 0 1

-- Prohibitively slow for large number of values taken.
main = asText . LList.take 25 <| fibs

-- This is way faster
-- main = asText . LList.take 25 <| fibs'
