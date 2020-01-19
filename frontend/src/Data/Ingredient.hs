{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Data.Ingredient
Description : An amendable ingredient list
Copyright   : (c) Mats Rauhala, 2019
License     : TBD
Maintainer  : mats.rauhala@iki.fi
Stability   : experimental
Portability : POSIX

An amendable ingredient list. The monoid operations will handle the business rules
-}
module Data.Ingredient
  ( Ingredient(..)
  , Ingredients
  , singleton
  )
  where

import qualified Data.Map.Strict as M
import Data.Semigroup (Last(..))

-- XXX: Think of a way to represent the units
data Unit

-- | Name of the ingredient
newtype Name = Name Text deriving (Eq, Ord, Show)

-- | Amount
newtype Amount = Amount Double deriving (Eq, Ord, Show, Num)


-- | The specific ingredient
data Ingredient
  = Ingredient { name   :: Name
               , amount :: Amount
               , unit   :: Unit }

-- | A collection of ingredients
--
-- If there are multiple ingredients of the same type, the last one will win
newtype Ingredients = Ingredients (Map Name (Last Ingredient))

-- | Create a list with a single ingredient
singleton :: Name -> Amount -> Unit -> Ingredients
singleton n a u = Ingredients (M.singleton n (Last (Ingredient n a u)))

instance Semigroup Ingredients where
  Ingredients a <> Ingredients b = Ingredients (M.unionWith (<>) a b)

instance Monoid Ingredients where
  mempty = Ingredients M.empty
