{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tuple.List.OneTuple () where

import           Prelude             ()

import           Data.Tuple.List     (Construct (Cons, cons'), Destruct (Head, Init, Last, Length, Tail, head, last, length, null, uncons'),
                                      head1, last1, length1, null1)
import           Data.Tuple.OneTuple (OneTuple (OneTuple))

instance Construct a () where
  type Cons a () = OneTuple a
  cons' a _ = OneTuple a

instance Destruct (OneTuple a) where
  type Head (OneTuple a) = a
  head = head1
  type Last (OneTuple a) = a
  last = last1
  type Tail (OneTuple a) = ()
  type Init (OneTuple a) = ()
  uncons' (OneTuple a) = (a, ())
  null = null1
  type Length (OneTuple a) = 1
  length = length1
