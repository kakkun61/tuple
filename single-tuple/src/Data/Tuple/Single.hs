{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Tuple.Single
  ( Single (..)
  , pattern Single
  ) where

import           Data.Functor.Identity (Identity (Identity, runIdentity))
import           Data.Tuple.OneTuple   (OneTuple (OneTuple), only)
import           Data.Tuple.Only       (Only (Only, fromOnly))

class Single t where
  wrap :: a -> t a
  unwrap :: t a -> a

pattern Single :: Single t => a -> t a
pattern Single a <- (unwrap -> a) where
  Single a = wrap a

instance Single Identity where
  wrap = Identity
  unwrap = runIdentity

{-# COMPLETE Single :: Identity #-}

instance Single Only where
  wrap = Only
  unwrap = fromOnly

{-# COMPLETE Single :: Only #-}

instance Single OneTuple where
  wrap = OneTuple
  unwrap = only

{-# COMPLETE Single :: OneTuple #-}
