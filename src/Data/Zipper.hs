module Data.Zipper
  ( Zipper(..)
  , goNext
  , goPrev
  , fromList
  , toList
  )
  where

import Control.Comonad (Comonad(..))

data Zipper a = Zipper [a] a [a]

instance Functor Zipper where
  fmap f (Zipper ls c rs) = Zipper (fmap f ls) (f c) (fmap f rs)

instance Comonad Zipper where
  extract (Zipper _ c _) = c
  duplicate z = Zipper (tail $ iterate goPrev z) z (tail $ iterate goNext z)
  extend f z = Zipper (fmap f $ tail $ iterate goPrev z) (f z) (fmap f $ tail $ iterate goNext z)

goNext :: Zipper a -> Zipper a
goNext (Zipper ls c (r:rs)) = Zipper (c:ls) r rs

goPrev :: Zipper a -> Zipper a
goPrev (Zipper (l:ls) c rs) = Zipper ls l (c:rs)

fromList :: a -> [a] -> Zipper a
fromList x xs = Zipper (repeat x) x (xs ++ repeat x)

toList :: Zipper a -> [a]
toList (Zipper _ _ xs) = xs
