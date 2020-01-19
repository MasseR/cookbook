{-|
Module      : Data.Functor.Foldable
Description : Simplified recursion schemes
Copyright   : (c) Mats Rauhala, 2019
License     : TBD
Maintainer  : mats.rauhala@iki.fi
Stability   : experimental
Portability : POSIX

Simplified version of recursion schemes
-}
module Data.Functor.Foldable where

newtype Fix f = Fix { getFix :: f (Fix f) }

-- | Cata or fold
cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = go where go = f . fmap go . getFix
{-# INLINE cata #-}
