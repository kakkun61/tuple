{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tuple.Single.OneTuple () where

import           Data.Tuple.OneTuple     (OneTuple (OneTuple), only)
import           Data.Tuple.Single.Class (Single (unwrap, wrap))

instance Single OneTuple where
  wrap = OneTuple
  unwrap = only
