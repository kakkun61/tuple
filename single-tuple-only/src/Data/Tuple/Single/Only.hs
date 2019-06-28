{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tuple.Single.Only () where

import           Data.Tuple.Only         (Only (Only, fromOnly))
import           Data.Tuple.Single.Class (Single (unwrap, wrap))

instance Single Only where
  wrap = Only
  unwrap = fromOnly
