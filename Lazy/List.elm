module Lazy.List ( force
                 , nil, cons, cons'
                 )
       where

import open Lazy
import Lazy as Lazy

data List a = L { bod : Lazy (Bod a) }

data Bod a = Nil
           | Cons a (List a)

force : List a -> Bod a
force (L r) = Lazy.force r.bod

nil : List a
nil = L { bod = lazy (\() -> Nil) }

cons : a -> (() -> List a) -> List a
cons x txs = L { bod = Lazy.map (Cons x) (lazy txs) }

cons' : (() -> (a, List a)) -> List a
cons' t = L { bod = Lazy.map (uncurry Cons) (lazy t) }
