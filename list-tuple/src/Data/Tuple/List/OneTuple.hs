{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Copyright   :  Kazuki Okamoto
-- License     :  see LICENSE
-- Maintainer  :  kazuki.okamoto@kakkun61.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- List-like operations for 'Data.Tuple.OneTuple.OneTuple'.

module Data.Tuple.List.OneTuple () where

import           Prelude             ()

import           Data.Tuple.List     (Cons, HasCons, HasHead, HasInit, HasLast,
                                      HasLength, HasTail, HasUncons, Head, Init,
                                      Last, Length, Tail)
import           Data.Tuple.OneTuple (OneTuple)

-- 1

type instance Cons a () = OneTuple a
type instance Head (OneTuple a) = a
type instance Tail (OneTuple a) = ()
type instance Init (OneTuple a) = ()
type instance Last (OneTuple a) = a
type instance Length (OneTuple a) = 1

instance HasHead (OneTuple a)

instance HasTail (OneTuple a)

instance HasInit (OneTuple a)

instance HasLast (OneTuple a)

instance HasCons a ()

instance HasUncons (OneTuple a)

instance HasLength (OneTuple a)

-- 2

type instance Cons a (OneTuple b) = (a, b)
type instance Tail (a, b) = OneTuple b
type instance Init (a, b) = OneTuple a

instance HasTail (a, b)

instance HasInit (a, b)

instance HasCons a (OneTuple b)

instance HasUncons (a, b)
