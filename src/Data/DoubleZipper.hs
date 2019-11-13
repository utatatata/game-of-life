module Data.DoubleZipper where

import Control.Comonad (Comonad(..))
import Data.Zipper (Zipper(..), goNext, goPrev)

newtype DoubleZipper a = DoubleZipper (Zipper (Zipper a))

instance Functor DoubleZipper where
  fmap f (DoubleZipper zz) = DoubleZipper (fmap (fmap f) zz)

instance Comonad DoubleZipper where
  extract (DoubleZipper zz) = extract (extract zz)
  duplicate (DoubleZipper zz) = fmap DoubleZipper . DoubleZipper . roll $ roll zz where
    roll zz = Zipper (tail $ iterate (fmap goPrev) zz) zz (tail $ iterate (fmap goNext) zz)