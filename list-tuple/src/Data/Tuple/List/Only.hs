{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tuple.List.Only () where

import           Prelude         ()

import           Data.Tuple.List (Construct (Cons, cons'), Destruct (Head, Init, Last, Length, Tail, head, last, length, null, uncons'),
                                  head1, last1, length1, null1)
import           Data.Tuple.Only (Only (Only))

instance Construct a () where
  type Cons a () = Only a
  cons' a _ = Only a

instance Destruct (Only a) where
  type Head (Only a) = a
  head = head1
  type Last (Only a) = a
  last = last1
  type Tail (Only a) = ()
  type Init (Only a) = ()
  uncons' (Only a) = (a, ())
  null = null1
  type Length (Only a) = 1
  length = length1
