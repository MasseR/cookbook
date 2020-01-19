{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : Data.Doc
Description : Pretty printer
Copyright   : (c) Mats Rauhala, 2019
License     : TBD
Maintainer  : mats.rauhala@iki.fi
Stability   : experimental
Portability : POSIX

Pretty printer based on the prettier printer paper

Simplified, the unoptimized version
-}
module Data.Doc
  ( Doc
  , nil
  , text
  , line
  , nest
  , layout
  , PrettyShow(..)
  )
  where

import           Data.Functor.Foldable

import qualified Data.Text             as T

import           Data.String

-- | The higher order document dsl
data DocF f
  = Nil
  | Content !Text f
  | Line !Int f
  deriving (Functor, Eq)

type Doc = Fix DocF

instance Eq Doc where
  Fix a == Fix b = a == b

instance IsString Doc where
  fromString x = text (T.pack x)

instance Semigroup Doc where
  Fix (Content a x) <> y = Fix (Content a (x <> y))
  Fix (Line i x) <> y = Fix (Line i (x <> y))
  Fix Nil <> y = y

instance Monoid Doc where
  mempty = Fix Nil

nest :: Int -> Doc -> Doc
nest n = cata $ \case
  Line i x -> Fix (Line (i+n) x)
  c -> Fix c

layout :: Doc -> Text
layout = cata $ \case
  Nil -> ""
  Content c x -> c <> x
  Line i x -> "\n" <> T.replicate i " " <> x

nil :: Doc
nil = Fix Nil

text :: Text -> Doc
text x = Fix (Content x nil)

line :: Doc
line = Fix (Line 0 nil)

class PrettyShow a where
  pretty :: a -> Doc
  default pretty :: Show a => a -> Doc
  pretty = text . T.pack . show
