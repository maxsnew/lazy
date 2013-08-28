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

-- toList might not terminate!
toList : Stream a -> [a]
toList xs = case xs of
  End        -> []
  More x txs -> x :: (toList . runLazy <| txs)

fromList : [a] -> Stream a
fromList xs' = case xs' of
  [] -> End
  x :: xs -> x :~: (lazyPure . fromList <| xs) 

-- Lazy Zip
zip : Stream a -> Stream b -> Stream (a, b)
zip xs ys = case xs of
  End -> End
  More x txs -> case ys of
    End -> End
    More y tys -> (x, y) :~: ((zip `lazyMap` txs) `lazyAp` tys)

zipWith : (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f xs ys = map (uncurry f) <| zip xs ys

-- More utils
head : Stream a -> a
head xs = case xs of
  More x _ -> x

tail : Stream a -> Stream a
tail xs = case xs of
  More _ txs -> runLazy txs

take : Int -> Stream a -> Stream a
take n xs = case xs of
  End -> End
  More x txs -> if n == 0
                then End
                else x :~: lazyMap (take (n - 1)) txs

takeWhile : (a -> Bool) -> Stream a -> Stream a
takeWhile p xs = case xs of
  End -> End
  More x txs -> let rest = takeWhile p `lazyMap` txs in
                if p x
                then x :~: rest
                else runLazy rest

drop : Int -> Stream a -> Stream a
drop n xs = case xs of
  End -> End
  More x txs -> if n == 0
                then xs
                else runLazy <| drop (n - 1) `lazyMap` txs

dropWhile : (a -> Bool) -> Stream a -> Stream a
dropWhile p = takeWhile (not . p)

-- Things you can't do with lists
repeat : a -> Stream a
repeat x = x :~: (\() -> repeat x)

cycle : [a] -> Stream a
cycle xs = fromList xs +~+ cycle xs

iterate : (a -> a) -> a -> Stream a
iterate f x = let y = f x 
              in y :~: (\() -> iterate f y)
