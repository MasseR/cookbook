{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Data.Recipe
Description : A single recipe
Copyright   : (c) Mats Rauhala, 2019
License     : TBD
Maintainer  : mats.rauhala@iki.fi
Stability   : experimental
Portability : POSIX

A recipe is a list of ingredients and instructions
-}
module Data.Recipe
  ( Recipe(..)
  , ingredients
  )
  where

import           Data.Doc (Doc)

import Control.Lens (Lens', lens)

-- Try to model the data

data Recipe a
  = Recipe { _ingredients  :: a
           , _instructions :: Doc
           }
  deriving (Eq)

ingredients :: Lens' (Recipe a) a
ingredients = lens _ingredients (\r i -> r{_ingredients = i})

instance (Semigroup a) => Semigroup (Recipe a) where
  Recipe a b <> Recipe a' b' = Recipe (a <> a') (b <> b')

instance Monoid a => Monoid (Recipe a) where
  mempty = Recipe mempty mempty
