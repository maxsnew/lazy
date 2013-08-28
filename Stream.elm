module Stream where

import open Lazy

data Stream a = End
              | More a (Lazy (Stream a))

-- Stream cons operator
(:~:) : a -> Lazy (Stream a) -> Stream a
(:~:) = More

(:~:~) : a -> Lazy (Stream a) -> Lazy (Stream a)
(:~:~) x txs () = x :~: txs

-- Stream is a functor
map : (a -> b) -> Stream a -> Stream b
map f xs = case xs of
  End -> End
  More x txs ->
    (f x) :~: (lazyMap (map f) txs)

-- Streams form a monoid with identity End
(+~+) : Stream a -> Stream a -> Stream a
(+~+) xs ys = case xs of
  End -> ys
  More x txs -> x :~: lazyMap (flip (+~+) ys) txs

-- Stream is Foldable (in a lazy way)
foldr : (a -> b -> b) -> b -> Stream a -> Lazy b
foldr f init xs' = let go xs = case xs of
                         End        -> lazyPure init
                         More x txs -> lazyMap (f x) (lazyBind txs go)
                   in go xs'
