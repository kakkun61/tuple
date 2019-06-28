{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tuple.Single.Identity () where

import           Data.Functor.Identity   (Identity (Identity, runIdentity))
import           Data.Tuple.Single.Class (Single (unwrap, wrap))

instance Single Identity where
  wrap = Identity
  unwrap = runIdentity
