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
  , Amount(..)
  , Unit(..)
  , singleton
  , ingredient
  )
  where

import           Control.Lens    (lens)
import qualified Data.Map.Strict as M
import           Data.Name       (HasName (name), Name)
import           Data.Semigroup  (Last (..))

-- XXX: Think of a way to represent the units
newtype Unit = Unit Text
  deriving (Eq, Show)

-- | Amount
newtype Amount = Amount Double deriving (Eq, Ord, Show, Num)


-- | The specific ingredient
data Ingredient
  = Ingredient { _ingredientName   :: Name
               , _ingredientAmount :: Amount
               , _ingredientUnit   :: Unit }
  deriving (Eq, Show)

instance HasName Ingredient where
  name = lens _ingredientName (\x n -> x{_ingredientName=n})

-- | A collection of ingredients
--
-- If there are multiple ingredients of the same type, the last one will win
newtype Ingredients = Ingredients (Map Name (Last Ingredient))
  deriving (Eq, Show)

-- | Create a list with a single ingredient
singleton :: Name -> Amount -> Unit -> Ingredients
singleton n a u = Ingredients (M.singleton n (Last (Ingredient n a u)))

-- | Create a list with a single ingredient
--
-- Synonymous with singleton
ingredient :: Name -> Amount -> Unit -> Ingredients
ingredient = singleton

instance Semigroup Ingredients where
  Ingredients a <> Ingredients b = Ingredients (M.unionWith (<>) a b)

instance Monoid Ingredients where
  mempty = Ingredients M.empty
