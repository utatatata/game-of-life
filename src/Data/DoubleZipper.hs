module Data.DoubleZipper
  ( DoubleZipper(..)
  , fromList
  , toList
  )
  where

import Control.Comonad (Comonad(..))
import Data.Zipper (Zipper(..))
import qualified Data.Zipper as Z

newtype DoubleZipper a = DoubleZipper (Zipper (Zipper a))

instance Functor DoubleZipper where
  fmap f (DoubleZipper zz) = DoubleZipper (fmap (fmap f) zz)

instance Comonad DoubleZipper where
  extract (DoubleZipper zz) = extract (extract zz)
  duplicate (DoubleZipper zz) = fmap DoubleZipper . DoubleZipper . roll $ roll zz where
    roll zz = Zipper (tail $ iterate (fmap Z.goPrev) zz) zz (tail $ iterate (fmap Z.goNext) zz)

fromList :: a -> [[a]] -> DoubleZipper a
fromList x xss = DoubleZipper $ Z.fromList (Z.fromList x []) (map (Z.fromList x) xss)

toList :: DoubleZipper a -> [[a]]
toList (DoubleZipper (Zipper _ _ rows)) = fmap Z.toList rows
