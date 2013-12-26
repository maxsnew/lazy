module Lazy.List ( force, head, tail
                 , nil, cons, cons', list
                 , iterate, unfold, repeat, cycle
                 , map, foldr, append
                 )
       where

import Lazy.List.Definition as Def
import Lazy.List.Definition (List, Nil, Cons, Bod)

import Lazy
import open Lazy

force : List a -> Bod a
force = Def.force

nil : List a
nil = Def.nil

cons : a -> (() -> List a) -> List a
cons = Def.cons

cons' : (() -> (a, List a)) -> List a
cons' = Def.cons'

list : (() -> Bod a) -> List a
list = Def.list

head : List a -> Maybe a
head xs = case force xs of
  Nil      -> Nothing
  Cons x _ -> Just x

tail : List a -> Maybe (List a)
tail xs = case force xs of
  Nil       -> Nothing
  Cons _ xs -> Just xs

iterate : (a -> Maybe a) -> a -> List a
iterate f x = let loop x = list <| \() ->
                    case f x of
                      Nothing -> Nil
                      Just y  -> Cons x <| loop y
              in loop x

unfold : (b -> Maybe (a, b)) -> b -> List a
unfold f s = let loop s = list <| \() ->
                   case f s of
                     Nothing     -> Nil
                     Just (x, y) -> Cons x (loop y)
             in loop s

repeat : a -> List a
repeat = iterate Just

cycle : a -> [a] -> List a
cycle x xs = let cycle' xs = case xs of
                   [] -> begin
                   (x :: xs) -> cons x <| \() ->
                     cycle' xs
                 begin = cons x <| \() ->
                   cycle' xs
             in begin

map : (a -> b) -> List a -> List b
map f xs = let loop xs = list <| \() ->
                 case force xs of
                   Nil -> Nil
                   Cons x xs ->
                     Cons (f x) (loop xs)
           in loop xs

foldr : (a -> Lazy b -> Lazy b) -> b -> List a -> Lazy b
foldr f def xs = let loop xs = lazy <| \() ->
                       case force xs of
                         Nil -> def
                         Cons x xs ->
                           Lazy.force . f x . loop <| xs
                 in loop xs

append : List a -> List a -> List a
append xs ys = let lcons : a -> Lazy (List a) -> Lazy (List a)
                   lcons x tys = lazy <| \() ->
                     cons x (\() -> Lazy.force tys)
               in Lazy.force <| foldr lcons ys xs
