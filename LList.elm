module LList where

import Lazy as Lazy
import Lazy (Lazy, force)

-- Lazy Lists
data LList a = End
             | More a (Lazy (LList a))

-- LList cons operator
(:~:) : a -> Lazy (LList a) -> LList a
(:~:) = More

-- Lazy cons
(:~:~) : a -> Lazy (LList a) -> Lazy (LList a)
(:~:~) x txs () = x :~: txs

-- LList is a functor
map : (a -> b) -> LList a -> LList b
map f xs = case xs of
  End -> End
  More x txs ->
    (f x) :~: (Lazy.map (map f) txs)

-- LLists form a monoid with identity End
(+~+) : LList a -> LList a -> LList a
(+~+) xs ys = case xs of
  End -> ys
  More x txs -> x :~: Lazy.map (flip (+~+) ys) txs

-- LList is Foldable (in an eager way)
foldr : (a -> b -> b) -> b -> LList a -> b
foldr f init xs' = let go xs = case xs of
                         End -> init
                         More x txs ->
                           f x . go . force <| txs
                   in go xs'

-- LList is Foldable (in a lazy way)                         
foldr' : (a -> b -> b) -> b -> LList a -> Lazy b
foldr' f init xs' = let go xs = case xs of
                          End        -> Lazy.pure init
                          More x txs -> Lazy.map (f x) (Lazy.bind txs go)
                    in go xs'

foldl : (b -> a -> b) -> b -> LList a -> b
foldl f init xs' = let go xs acc = case xs of
                         End        -> init
                         More x txs -> go (force txs) (f acc x)
                   in go xs' init

-- toList might not terminate!
toList : LList a -> [a]
toList xs = case xs of
  End        -> []
  More x txs -> x :: (toList . force <| txs)

fromList : [a] -> LList a
fromList xs' = case xs' of
  [] -> End
  x :: xs -> x :~: (Lazy.pure . fromList <| xs) 

-- Lazy Zip
zip : LList a -> LList b -> LList (a, b)
zip xs ys = case xs of
  End -> End
  More x txs -> case ys of
    End -> End
    More y tys -> (x, y) :~: ((zip `Lazy.map` txs) `Lazy.ap` tys)

zipWith : (a -> b -> c) -> LList a -> LList b -> LList c
zipWith f xs ys = map (uncurry f) <| zip xs ys

-- More utils
head : LList a -> a
head xs = case xs of
  More x _ -> x

tail : LList a -> LList a
tail xs = case xs of
  More _ txs -> force txs

take : Int -> LList a -> [a]
take n xs = case xs of
  End -> []
  More x txs -> if n == 0
                then []
                else x :: take (n - 1) (force txs)

takeWhile : (a -> Bool) -> LList a -> LList a
takeWhile p xs = case xs of
  End -> End
  More x txs -> let rest = takeWhile p `Lazy.map` txs in
                if p x
                then x :~: rest
                else force rest

drop : Int -> LList a -> LList a
drop n xs = case xs of
  End -> End
  More x txs -> if n == 0
                then xs
                else force <| drop (n - 1) `Lazy.map` txs

dropWhile : (a -> Bool) -> LList a -> LList a
dropWhile p = takeWhile (not . p)

-- Things you can't do with lists
repeat : a -> LList a
repeat x = x :~: (\() -> repeat x)

cycle : [a] -> LList a
cycle xs = fromList xs +~+ cycle xs

iterate : (a -> a) -> a -> LList a
iterate f x = let y = f x 
              in y :~: (\() -> iterate f y)
