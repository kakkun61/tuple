{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tuple.List.Only () where

import           Prelude         ()

import           Data.Tuple.List (Cons, Head, Init, Last, Length, Tail, HeadTailUnique (cons'), TupleUnique (head, last, length, null, uncons'),
                                  head1, last1, length1, null1)
import           Data.Tuple.Only (Only (Only))

type instance Cons a () = Only a
type instance Head (Only a) = a
type instance Last (Only a) = a
type instance Tail (Only a) = ()
type instance Init (Only a) = ()
type instance Length (Only a) = 1

instance HeadTailUnique a () where
  cons' a _ = Only a

instance TupleUnique (Only a) where
  head = head1
  last = last1
  uncons' (Only a) = (a, ())
  null = null1
  length = length1

type instance Tail (a, b) = Only b
