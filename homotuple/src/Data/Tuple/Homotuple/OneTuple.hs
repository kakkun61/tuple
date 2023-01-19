{-# LANGUAGE CPP          #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

#if MIN_VERSION_OneTuple(0,3,0) && MIN_VERSION_base(4,15,0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif

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

module Data.Tuple.Homotuple.OneTuple () where

#if MIN_VERSION_OneTuple(0,3,0)
#if !MIN_VERSION_base(4,15,0)
import Data.Tuple.Homotuple (Homotuple, errorLengthMismatch)
import Data.Tuple.Solo      (Solo (Solo))
import GHC.Exts             (IsList (Item, fromList, toList))

type instance Homotuple 1 a = Solo a

instance IsList (Solo a) where
  type Item (Solo a) = a
  fromList [a] = Solo a
  fromList _   = errorLengthMismatch
  toList (Solo a) = [a]
#endif
#else
import Data.Tuple.Homotuple (Homotuple, errorLengthMismatch)
import Data.Tuple.OneTuple  (OneTuple (OneTuple))
import GHC.Exts             (IsList (Item, fromList, toList))

type instance Homotuple 1 a = OneTuple a

instance IsList (OneTuple a) where
  type Item (OneTuple a) = a
  fromList [a] = OneTuple a
  fromList _   = errorLengthMismatch
  toList (OneTuple a) = [a]
#endif
