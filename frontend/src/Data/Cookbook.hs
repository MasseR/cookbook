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

-- | A diary is a collection of recipes
--
-- Each time you implement a recipe you should keep a diary of your amendments and notes
data Diary = Diary { date   :: !Day
                   , recipe :: Recipe Ingredients
                   }
  deriving (Eq)

-- | A food is a single food
--
-- A food is something like 'karjalanpaisti' which contains one or more recipes.
data Food
  = Food { name    :: Text -- ^ Name of the food
         , recipes :: [Diary]
         }
  deriving (Eq)

-- | A cookbook is a collection of different foods
newtype Cookbook
  = Cookbook ( Map Text Food )
