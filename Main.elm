module Main where

import LList as LList
import LList (LList, (:~:), (:~:~), (+~+))
import Lazy as Lazy

nats : LList Int
nats = 0 :~: (\() -> LList.map ((+) 1) nats)

fibs : LList Int
fibs = 0 :~: Lazy.pure (1 :~: (\() -> LList.zipWith (+) fibs (LList.tail fibs)))

-- Prohibitively slow for large number of values taken.
main = asText . LList.take 25 <| fibs
