module Lazy where

type Lazy a = () -> a

-- Lazy values can be evaluated
force : Lazy a -> a
force tx = tx ()

-- Lazy is a functor
map : (a -> b) -> Lazy a -> Lazy b
map f = (.) f

-- Lazy is an applicative functor
pure : a -> Lazy a
pure x () = x
ap   : Lazy (a -> b) -> Lazy a -> Lazy b
ap tf tx = force tf . tx

-- Lazy is a monad
join : Lazy (Lazy a) -> Lazy a
join t = force t
bind : Lazy a -> (a -> Lazy b) -> Lazy b
bind tx mtf = (flip mtf) `ap` tx






