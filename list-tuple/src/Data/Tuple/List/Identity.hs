{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tuple.List.Identity () where

import           Prelude               ()

import           Data.Functor.Identity (Identity (Identity))
import           Data.Tuple.List (Cons, Head, Init, Last, Length, Tail, HeadTailUnique (cons'), TupleUnique (head, last, length, null, uncons'),
                                  head1, last1, length1, null1)

type instance Cons a () = Identity a
type instance Head (Identity a) = a
type instance Last (Identity a) = a
type instance Tail (Identity a) = ()
type instance Init (Identity a) = ()
type instance Length (Identity a) = 1

instance HeadTailUnique a () where
  cons' a _ = Identity a

instance TupleUnique (Identity a) where
  head = head1
  last = last1
  uncons' (Identity a) = (a, ())
  null = null1
  length = length1
