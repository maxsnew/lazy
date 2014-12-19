module Lazy.Stream ( Stream
                   , head, tail, force
                   , cons, cons', iterate, unfold, repeat, cycle
                   , map, apply, map2, scanl
                   , take, drop, splitAt
                   , sampleOn
                   , filter, takeWhile, dropWhile, splitWith
                   ) where

{-| This library is for creating and manipulating infinite streams.

The cells of the Stream are memoized using the `Lazy` type, meaning
tails of the stream are memoized, and thus certain uses can take a lot
of space. If you only need the values of your stream once, you may be
better off with a Stream implementation that is not memoized (i.e.,
uses plain thunks `() -> Stream a` as tails of the Stream).

# Create
@docs cons, cons', iterate, unfold, repeat, cycle

# Observe
@docs head, tail, force, sampleOn, take, drop, splitAt

# Converge
@docs foldr, filter, takeWhile, dropWhile, splitWith

# Transform
@docs map, apply, map2, scanl

-}

import Lazy (..)
import Lazy
import Signal ((<~), foldp, Signal)
import List (..)

type Stream a = S (Lazy (a, Stream a))

-----------
-----------

{-| Get the first element of a stream, called the `head`.

```haskell
head ones == 1
```
-}
head : Stream a -> a
head = fst << force

{-| Drop the `head` of a stream, leaving you with the `tail`.

```haskell
-- 1, 1, 1, 1, ...
stillAllOnes = tail
```
-}
tail : Stream a -> Stream a
tail = snd << force

{-| Get both the head and tail at once -}
force : Stream a -> (a, Stream a)
force (S t) = Lazy.force t

{-| Create a stream:

```haskell
-- 1, 1, 1, 1, ...
ones = cons 1 (\() -> ones)
```
 -}
cons : a -> (() -> Stream a) -> Stream a
cons x txs = let mtxs = lazy txs in
  S << lazy <| \() ->
  (x, Lazy.force mtxs)

{-| Create a stream that is slightly more lazy. Notice that the
head of the stream is defined within the thunk, so its evaluation
is delayed. This is nice when each element of the stream is costly to compute.

```haskell
-- 1, 1, 1, 1, ...
ones = cons' (\_ -> (1,ones))
```
-}
cons' : (() -> (a, Stream a)) -> Stream a
cons' = S << lazy

{-| Iteratively apply a function to a value:

```haskell
-- x, f x, f (f x), f (f (f x)), ...
iterate f x

-- 2, 4, 8, 16, ...
powersOf2 = iterate (\n -> n^2) 2
```
-}
iterate : (a -> a) -> a -> Stream a
iterate f x = cons' <| \() ->
  (x, iterate f (f x))

{-| Build a stream from a seed value:
```haskell
--- 0, 1, 1, 2, 3, 5, 8, ...
fibs = unfold (\(m,n) -> (m, (n, m + n))) (0, 1)
```
-}
unfold : (b -> (a, b)) -> b -> Stream a
unfold f s = let loop s =
                   cons' <| \() ->
                   let (hd, s') = f s
                   in (hd, loop s')
             in loop s

{-| Repeat a value infinitely:

```haskell
-- 1, 1, 1, 1, ...
ones = repeat 1
```
 -}
repeat : a -> Stream a
repeat x = let go = cons x <| \() -> go
           in go

{-| Infinitely cycle through a list, where the head and tail of
the list are given separately to ensure that no one tries to cycle
on an empty list:

```haskell
-- "Alice", "Bob", "Alice", "Bob", ...
cycle "Alice" ["Bob"]
```
-}
cycle : a -> List a -> Stream a
cycle x xs = let cycle' ys = case ys of
                   [] -> go
                   (y :: ys) -> cons y <| \() ->
                     cycle' ys
                 go = cons' <| \() ->
                   (x, cycle' xs)
             in go

{-| Apply a function to every element of a Stream.

```haskell
-- 2, 2, 2, 2, ...
twos = map (\n -> n + 1) ones
```
-}
map : (a -> b) -> Stream a -> Stream b
map f xs = cons' <| \() ->
  (f (head xs), map f (tail xs))

{-| Pairwise apply a stream of functions to a stream of arguments.
When paired with `map`, this can be used to emulate `map2` over *n* streams.

```haskell
map3 : (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
map3 f xs ys zs = f `S.map` xs `S.apply` ys `S.apply` zs
```
-}
apply : Stream (a -> b) -> Stream a -> Stream b
apply fs xs = map2 (<|) fs xs

{-| Combine two streams, applying the given function pairwise:

```haskell
-- 3, 5, 9, 17, ...
map2 (+) ones powersOf2
```
-}
map2 : (a -> b -> c) -> Stream a -> Stream b -> Stream c
map2 f xs ys = cons' <| \() ->
  (f (head xs) (head ys),
   map2 f (tail xs) (tail ys))

{-| Scan over a stream from the left, building an infinite stream of reductions.

```haskell
-- 0, 1, 2, 3, ...
runningTotal = scanl (+) 0 ones
```
-}
scanl : (a -> b -> b) -> b -> Stream a -> Stream b
scanl f init xs = cons' <| \() ->
  (init,
   scanl f (f (head xs) init) (tail xs))

{-| Take the first *n* elements of a stream

```haskell
take 5 powersOf2 == [2,4,8,16,32]
```
-}
take : Int -> Stream a -> List a
take n xs = fst <| splitAt n xs

{-| Drop the first *n* elements of a stream

```haskell
-- 64, 128, 256, 512, ...
drop 5 powersOf2
```
-}
drop : Int -> Stream a -> Stream a
drop n xs = snd <| splitAt n xs

{-| Combination of `take` and `drop`

```haskell
splitAt n xs == (take n xs, drop n xs)
```
-}
splitAt : Int -> Stream a -> (List a, Stream a)
splitAt n xs = case n of
  0 -> ([], xs)
  n -> let (heads, end) = splitAt (n - 1) (tail xs)
       in (head xs :: heads, end)

{-| Turn a stream into a signal of the elements of the stream,
    advancing through the stream whenever an event in the given signal is
    fired.
-}
sampleOn : Signal b -> Stream a -> Signal a
sampleOn sig str = let tails = foldp (\_ -> tail) str sig in
                   head <~ tails

{-| Filter the elements of a Stream according to a predicate.

```haskell
-- 1, 3, 5, 7, ...
filter isOdd naturals
```
    This will infinite loop if no more elements in the stream satisfy
    the predicate!

```haskell
-- Bad! Infinite loop!
filter isOdd twos
```

-}
filter : (a -> Bool) -> Stream a -> Stream a
filter p xs = cons' <| (\() ->
  let (hd, tl) = force xs in
  case p hd of
    True ->  (hd, filter p tl)
    False -> force <| filter p tl)

{-| Take values from the stream for as long as the predicate holds.

```haskell
takeWhile (\n -> n < 10) powersOf2 == [2,4,8]
```

    This will infinite loop if all elements of the stream satisfy the
    predicate!

```haskell
-- Bad! Infinite loop!
takeWhile isEven powersOf2
```

-}
takeWhile : (a -> Bool) -> Stream a -> List a
takeWhile p xs = fst <| splitWith p xs

{-| Drop values from the stream as long as the predicate holds.

```haskell
-- 16, 32, 64, 128, ...
dropWhile (\n -> n < 10) powersOf2
```

    This can infinite loop if all elements of the stream satisfy the
    predicate!

```haskell
-- Bad! Infinite loop!
dropWhile isEven powersOf2
```

-}
dropWhile : (a -> Bool) -> Stream a -> Stream a
dropWhile p xs = snd <| splitWith p xs

{-| Combination of `takeWhile` and `dropWhile`. Split a stream when
    the predicate no longer holds.

```haskell
splitWith pred xs == (takeWhile pred xs, dropWhile pred xs)

splitWith (\n -> n < 10) powersOf2 == ([2,4,8], ...)
```
This will infinite loop if all elements satisfy the predicate!
-}
splitWith : (a -> Bool) -> Stream a -> (List a, Stream a)
splitWith p xs = let (hd, tl) = force xs in
  case p hd of
    True  -> let (taken, dropped) = splitWith p tl
             in (hd :: taken, dropped)
    False -> ([], xs)

{-| Lazily fold over a Stream.

    Forcing the value of this function only terminates if the provided
    folding function eventually ignores its second argument.

-}
foldr : (a -> Lazy b -> Lazy b) -> Stream a -> Lazy b
foldr f xs = let loop xs = lazy <| \() ->
                   let (hd, tl) = force xs in
                   Lazy.force <| f hd (loop tl)
             in loop xs
