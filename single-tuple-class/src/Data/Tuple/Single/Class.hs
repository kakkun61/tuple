{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Tuple.Single.Class
  ( Single (..)
  , pattern Single
  ) where

class Single t where
  wrap :: a -> t a
  unwrap :: t a -> a

pattern Single :: Single t => a -> t a
pattern Single a <- (unwrap -> a) where
  Single a = wrap a
