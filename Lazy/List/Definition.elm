module Lazy.List.Definition ( force
                            , nil, cons, cons'
                            )
       where

import Lazy (..)
import Lazy as Lazy

data List a = L (Lazy (Bod a))

-- Equivalent to Maybe (a, List a)
data Bod a = Nil
           | Cons a (List a)

force : List a -> Bod a
force (L bod) = Lazy.force bod

nil : List a
nil = L (lazy (\() -> Nil))

cons : a -> (() -> List a) -> List a
cons x txs = L (Lazy.map (Cons x) (lazy txs))

cons' : (() -> (a, List a)) -> List a
cons' t = L (Lazy.map (uncurry Cons) (lazy t))

list : (() -> Bod a) -> List a
list = L . lazy
