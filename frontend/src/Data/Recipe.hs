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
  )
  where

import           Data.Doc (Doc)

-- Try to model the data

data Recipe m a
  = Recipe { ingredients  :: m a
           , instructions :: Doc
           }

instance (Semigroup (m a)) => Semigroup (Recipe m a) where
  Recipe a b <> Recipe a' b' = Recipe (a <> a') (b <> b')
