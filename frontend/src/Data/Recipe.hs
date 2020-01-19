{-# LANGUAGE FlexibleInstances #-}
module Data.Recipe where

-- Try to model the data

data Recipe m a
  = Recipe { ingredients  :: m a
           , instructions :: Text
           }

data Unit

data Ingredient
  = Ingredient { name :: Text
               , amount :: Double
               , unit :: Unit }

instance (Semigroup (m a)) => Semigroup (Recipe m a) where
  Recipe a b <> Recipe a' b' = Recipe (a <> a') (b <> b')
