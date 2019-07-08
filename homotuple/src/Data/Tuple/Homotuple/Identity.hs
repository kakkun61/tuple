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

module Data.Tuple.Homotuple.Identity () where

import Data.Functor.Identity (Identity (Identity))
import Data.Tuple.Homotuple  (Homotuple, errorLengthMismatch)
import GHC.Exts              (IsList (Item, fromList, toList))

type instance Homotuple 1 a = Identity a

instance IsList (Identity a) where
  type Item (Identity a) = a
  fromList [a] = Identity a
  fromList _   = errorLengthMismatch
  toList (Identity a) = [a]
