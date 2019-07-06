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
-- List-like operations for 'Data.Tuple.Only.Only'.

module Data.Tuple.List.Only () where

import           Prelude         ()

import           Data.Tuple.List (Cons, HasCons, HasHead, HasInit, HasLast,
                                  HasLength, HasTail, HasUncons, Head, Init,
                                  Last, Length, Tail)
import           Data.Tuple.Only (Only)

-- 1

type instance Cons a () = Only a
type instance Head (Only a) = a
type instance Tail (Only a) = ()
type instance Init (Only a) = ()
type instance Last (Only a) = a
type instance Length (Only a) = 1

instance HasHead (Only a)

instance HasTail (Only a)

instance HasInit (Only a)

instance HasLast (Only a)

instance HasCons a ()

instance HasUncons (Only a)

instance HasLength (Only a)

-- 2

type instance Cons a (Only b) = (a, b)
type instance Tail (a, b) = Only b
type instance Init (a, b) = Only a

instance HasTail (a, b)

instance HasInit (a, b)

instance HasCons a (Only b)

instance HasUncons (a, b)
