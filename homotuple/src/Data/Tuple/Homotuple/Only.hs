{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE Trustworthy  #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Copyright   :  Kazuki Okamoto
-- License     :  see LICENSE
-- Maintainer  :  kazuki.okamoto@kakkun61.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Single homotuples.

module Data.Tuple.Homotuple.Only () where

import Data.Tuple.Homotuple (Homotuple, errorLengthMismatch)
import Data.Tuple.Only      (Only (Only))
import GHC.Exts             (IsList (Item, fromList, toList))

type instance Homotuple 1 a = Only a

instance IsList (Only a) where
  type Item (Only a) = a
  fromList [a] = Only a
  fromList _   = errorLengthMismatch
  toList (Only a) = [a]
