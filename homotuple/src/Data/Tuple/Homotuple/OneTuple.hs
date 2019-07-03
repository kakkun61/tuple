{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tuple.Homotuple.OneTuple () where

import           Data.Tuple.Homotuple (Homotuple, errorLengthMismatch)
import           Data.Tuple.OneTuple  (OneTuple (OneTuple))
import           GHC.Exts             (IsList (Item, fromList, toList))

type instance Homotuple 1 a = OneTuple a

instance IsList (OneTuple a) where
  type Item (OneTuple a) = a
  fromList [a] = OneTuple a
  fromList _   = errorLengthMismatch
  toList (OneTuple a) = [a]
