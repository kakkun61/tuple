{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tuple.List.Identity () where

import           Prelude               ()

import           Data.Functor.Identity (Identity)
import           Data.Tuple.List       (Cons, HasCons, HasHead, HasInit,
                                        HasLast, HasLength, HasTail, HasUncons,
                                        Head, Init, Last, Length, Tail)

-- 1

type instance Cons a () = Identity a
type instance Head (Identity a) = a
type instance Tail (Identity a) = ()
type instance Init (Identity a) = ()
type instance Last (Identity a) = a
type instance Length (Identity a) = 1

instance HasHead (Identity a)

instance HasTail (Identity a)

instance HasInit (Identity a)

instance HasLast (Identity a)

instance HasCons a ()

instance HasUncons (Identity a)

instance HasLength (Identity a)

-- 2

type instance Cons a (Identity b) = (a, b)
type instance Tail (a, b) = Identity b
type instance Init (a, b) = Identity a

instance HasTail (a, b)

instance HasInit (a, b)

instance HasCons a (Identity b)

instance HasUncons (a, b)
