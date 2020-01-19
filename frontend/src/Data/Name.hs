{-|
Module      : Data.Name
Description : A name and its lenses
Copyright   : (c) Mats Rauhala, 2019
License     : TBD
Maintainer  : mats.rauhala@iki.fi
Stability   : experimental
Portability : POSIX

A name and its lens
-}
module Data.Name where

import Control.Lens (Lens', lens, view, set)

-- | Newtype over name
newtype Name = Name Text deriving (Eq, Ord, Show)

class HasName a where
  getName :: a -> Name
  getName = view name

  setName :: a -> Name -> a
  setName x n = set name n x

  name :: Lens' a Name
  name = lens getName setName
