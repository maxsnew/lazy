module Lazy.List where

import Lazy.List.Definition as Def
import Lazy.List.Definition (Bod(..))

import Lazy
import Lazy (..)

force : Def.List a -> Bod a
force = Def.force

nil : Def.List a
nil = Def.nil

cons : a -> (() -> Def.List a) -> Def.List a
cons = Def.cons

cons' : (() -> (a, Def.List a)) -> Def.List a
cons' = Def.cons'

list : (() -> Bod a) -> Def.List a
list = Def.list

head : Def.List a -> Maybe a
head xs = case force xs of
  Nil      -> Nothing
  Cons x _ -> Just x

tail : Def.List a -> Maybe (Def.List a)
tail xs = case force xs of
  Nil       -> Nothing
  Cons _ xs -> Just xs

iterate : (a -> Maybe a) -> a -> Def.List a
iterate f x = let loop x = list <| \() ->
                    case f x of
                      Nothing -> Nil
                      Just y  -> Cons x <| loop y
              in loop x

unfold : (b -> Maybe (a, b)) -> b -> Def.List a
unfold f s = let loop s = list <| \() ->
                   case f s of
                     Nothing     -> Nil
                     Just (x, y) -> Cons x (loop y)
             in loop s

repeat : a -> Def.List a
repeat = iterate Just

cycle : a -> List a -> Def.List a
cycle x xs = let cycle' xs = case xs of
                   [] -> begin
                   (x :: xs) -> cons x <| \() ->
                     cycle' xs
                 begin = cons x <| \() ->
                   cycle' xs
             in begin

map : (a -> b) -> Def.List a -> Def.List b
map f xs = let loop xs = list <| \() ->
                 case force xs of
                   Nil -> Nil
                   Cons x xs ->
                     Cons (f x) (loop xs)
           in loop xs

foldr : (a -> Lazy b -> Lazy b) -> b -> Def.List a -> Lazy b
foldr f def xs = let loop xs = lazy <| \() ->
                       case force xs of
                         Nil -> def
                         Cons x xs ->
                           Lazy.force << f x << loop <| xs
                 in loop xs

append : Def.List a -> Def.List a -> Def.List a
append xs ys = let lcons : a -> Lazy (Def.List a) -> Lazy (Def.List a)
                   lcons x tys = lazy <| \() ->
                     cons x (\() -> Lazy.force tys)
               in Lazy.force <| foldr lcons ys xs
