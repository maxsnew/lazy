module Lazy where

type Lazy a = () -> a

-- Lazy values can be evaluated
runLazy : Lazy a -> a
runLazy tx = tx ()

-- Lazy is a functor
lazyMap : (a -> b) -> Lazy a -> Lazy b
lazyMap f = (.) f

-- Lazy is an applicative functor
lazyPure : a -> Lazy a
lazyPure x () = x
lazyAp   : Lazy (a -> b) -> Lazy a -> Lazy b
lazyAp tf tx = runLazy tf . tx

-- Lazy is a monad
lazyJoin : Lazy (Lazy a) -> Lazy a
lazyJoin t = runLazy t
lazyBind : Lazy a -> (a -> Lazy b) -> Lazy b
lazyBind tx mtf = (flip mtf) `lazyAp` tx






