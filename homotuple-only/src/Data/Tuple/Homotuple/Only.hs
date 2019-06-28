{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tuple.Homotuple.Only () where

import           Data.Tuple.Homotuple (Homotuple, errorLengthMismatch)
import           Data.Tuple.Only      (Only (Only))
import           GHC.Exts             (IsList (Item, fromList, toList))

type instance Homotuple 1 a = Only a

instance IsList (Only a) where
  type Item (Only a) = a
  fromList [a] = Only a
  fromList _   = errorLengthMismatch
  toList (Only a) = [a]
