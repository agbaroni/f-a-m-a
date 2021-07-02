module Main where

import Control.Applicative

data State a = State a deriving Show

instance Functor State where
  -- fmap :: (a -> b) -> State a -> State b
  fmap f (State x) = State (f x)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

test1 :: State Int -> State Int
test1 s = fmap fact s

func1 :: Int -> Int -> Int
func1 m n = 2 * (m + n)

class Functor2 f where
  fmap2 :: (a -> b -> c) -> f a -> f b -> f c

instance Functor2 State where
  fmap2 f (State x) (State y) = State (f x y)

test2 :: State Int -> State Int -> State Int
test2 s t = fmap2 func1 s t

class FunctorN1 f where
  fmapN1 :: f (a -> b) -> f a -> f b

instance FunctorN1 State where
  -- 1) fmapN (State f) (State x) = State (f x)
  -- 2)
  fmapN1 (State f) x = fmap f x

test3 :: State Int -> State Int -> State Int
test3 s t = fmapN1 (fmapN1 (State func1) s) t

class FunctorN2 f where
  embedN2 :: a -> f a
  fmapN2 :: f (a -> b) -> f a -> f b

instance FunctorN2 State where
  embedN2 = State
  fmapN2 (State f) x = fmap f x

test4 :: State Int -> State Int -> State Int
test4 s t = fmapN2 (fmapN2 (embedN2 func1) s) t

wrap :: State a -> IO a
wrap (State x) = return x

-- FunctorN3
instance Applicative State where
  -- pure :: a -> State a
  pure = State
  -- <*> :: State (a -> b) -> State a -> State b
  (State f) <*> x = fmap f x

test5 :: State Int -> State Int -> State Int
test5 s t = (pure func1) <*> s <*> t

main :: IO ()
main = (wrap (test1 (State 7))) >>= \n -> print n >> (wrap (test2 (State 10) (State 12))) >>= \n -> print n >> (wrap (test3 (State 5) (State 6))) >>= \n -> print n >> (wrap (test5 (State 15) (State 36))) >>= \n -> print n >> return ()
