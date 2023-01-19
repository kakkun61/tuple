{-# LANGUAGE CPP                   #-}
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

#if MIN_VERSION_OneTuple(0,3,0)
#if !MIN_VERSION_base(4,15,0)
import Prelude ()

import Data.Tuple.List (Cons, HasAt, HasCons, HasHead, HasInit, HasLast, HasLength, HasReverse, HasTail, HasUncons,
                        Head, Init, Last, Length, Reverse, Tail, type (!!))
import Data.Tuple.Solo (Solo)

-- 1

type instance Cons a () = Solo a
type instance Head (Solo a) = a
type instance Tail (Solo a) = ()
type instance Init (Solo a) = ()
type instance Last (Solo a) = a
type instance Length (Solo a) = 1

instance HasHead (Solo a)

instance HasTail (Solo a)

instance HasInit (Solo a)

instance HasLast (Solo a)

instance HasCons a ()

instance HasUncons (Solo a)

instance HasLength (Solo a)

type instance Reverse (Solo a) = Solo a

instance HasReverse (Solo a)

type instance (Solo a) !! 0 = a

instance HasAt (Solo a) 0

-- 2

type instance Cons a (Solo b) = (a, b)
type instance Tail (a, b) = Solo b
type instance Init (a, b) = Solo a

instance HasTail (a, b)

instance HasInit (a, b)

instance HasCons a (Solo b)

instance HasUncons (a, b)
#endif
#else
import Prelude ()

import Data.Tuple.List     (Cons, HasAt, HasCons, HasHead, HasInit, HasLast, HasLength, HasReverse, HasTail, HasUncons,
                            Head, Init, Last, Length, Reverse, Tail, type (!!))
import Data.Tuple.OneTuple (OneTuple)

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

type instance Reverse (OneTuple a) = OneTuple a

instance HasReverse (OneTuple a)

type instance (OneTuple a) !! 0 = a

instance HasAt (OneTuple a) 0

-- 2

type instance Cons a (OneTuple b) = (a, b)
type instance Tail (a, b) = OneTuple b
type instance Init (a, b) = OneTuple a

instance HasTail (a, b)

instance HasInit (a, b)

instance HasCons a (OneTuple b)

instance HasUncons (a, b)
#endif
