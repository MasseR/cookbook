{-|
Module      : Data.Cookbook
Description : A cookbook is a collection of recipes
Copyright   : (c) Mats Rauhala, 2019
License     : TBD
Maintainer  : mats.rauhala@iki.fi
Stability   : experimental
Portability : POSIX

A cookbook is a collection of recipes. A recipe can contain multiple variations.
-}
module Data.Cookbook where

import           Data.Ingredient
import           Data.Recipe

import           Control.Lens    (Lens', lens)
import           Data.Name

-- | A diary is a collection of recipes
--
-- Each time you implement a recipe you should keep a diary of your amendments and notes
data Diary = Diary { _date   :: !Day
                   , _recipe :: Recipe Ingredients
                   }
  deriving (Eq)

recipe :: Lens' Diary (Recipe Ingredients)
recipe = lens _recipe (\d r -> d{_recipe=r})

-- | A food is a single food
--
-- A food is something like 'karjalanpaisti' which contains one or more recipes.
data Food
  = Food { _foodName    :: !Name -- ^ Name of the food
         , _foodRecipes :: [Diary]
         }
  deriving (Eq)

instance HasName Food where
  name = lens _foodName (\x n -> x{_foodName=n})

-- | A cookbook is a collection of different foods
newtype Cookbook
  = Cookbook ( Map Name Food )
