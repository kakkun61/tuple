{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tuple.List.OneTuple () where

import           Prelude             ()

import           Data.Tuple.List (Cons, Head, Init, Last, Length, Tail, HeadTailUnique (cons'), TupleUnique (head, last, length, null, uncons'),
                                  head1, last1, length1, null1)
import           Data.Tuple.OneTuple (OneTuple (OneTuple))

type instance Cons a () = OneTuple a
type instance Head (OneTuple a) = a
type instance Last (OneTuple a) = a
type instance Tail (OneTuple a) = ()
type instance Init (OneTuple a) = ()
type instance Length (OneTuple a) = 1

instance HeadTailUnique a () where
  cons' a _ = OneTuple a

instance TupleUnique (OneTuple a) where
  head = head1
  last = last1
  uncons' (OneTuple a) = (a, ())
  null = null1
  length = length1
