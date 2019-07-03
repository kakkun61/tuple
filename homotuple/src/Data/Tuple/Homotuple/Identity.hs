{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tuple.Homotuple.Identity () where

import           Data.Functor.Identity (Identity (Identity))
import           Data.Tuple.Homotuple  (Homotuple, errorLengthMismatch)
import           GHC.Exts              (IsList (Item, fromList, toList))

type instance Homotuple 1 a = Identity a

instance IsList (Identity a) where
  type Item (Identity a) = a
  fromList [a] = Identity a
  fromList _   = errorLengthMismatch
  toList (Identity a) = [a]
