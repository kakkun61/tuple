{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tuple.List.Identity () where

import           Prelude               ()

import           Data.Functor.Identity (Identity (Identity))
import           Data.Tuple.List       (Construct (Cons, cons'), Destruct (Head, Init, Last, Length, Tail, head, last, length, null, uncons'),
                                        head1, last1, length1, null1)

instance Construct a () where
  type Cons a () = Identity a
  cons' a _ = Identity a

instance Destruct (Identity a) where
  type Head (Identity a) = a
  head = head1
  type Last (Identity a) = a
  last = last1
  type Tail (Identity a) = ()
  type Init (Identity a) = ()
  uncons' (Identity a) = (a, ())
  null = null1
  type Length (Identity a) = 1
  length = length1
