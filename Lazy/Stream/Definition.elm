module Lazy.Stream.Definition (head, tail, force
                              , cons, cons'
                              )
       where

import open Lazy
import Lazy
       
data Stream a = S (Lazy (a, Stream a))

head : Stream a -> a
head = fst . force

tail : Stream a -> Stream a
tail = snd . force

force : Stream a -> (a, Stream a)
force (S t) = Lazy.force t

cons : a -> (() -> Stream a) -> Stream a
cons x txs = let mtxs = lazy txs in
  S . lazy <| \() ->
  (x, Lazy.force mtxs)

cons' : (() -> (a, Stream a)) -> Stream a
cons' = S . lazy
