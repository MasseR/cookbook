module Data.Recipe where

data Recipe a
  = Recipe { ingredients :: [a]
           , instructions :: Text
           }
