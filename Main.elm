module Main where

import Stream as Stream
import Stream (Stream, (:~:), (:~:~), (+~+))
import open Lazy

nats : Stream Int
nats = 0 :~: (\() -> Stream.map ((+) 1) nats)

fibs : Stream Int
fibs = 0 :~: lazyPure (1 :~: (\() -> Stream.zipWith (+) fibs (Stream.tail fibs)))

-- Prohibitively slow for large number of values taken.
main = asText . Stream.toList . Stream.take 15 <| fibs
