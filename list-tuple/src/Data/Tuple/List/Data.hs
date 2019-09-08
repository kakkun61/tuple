{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE Safe                   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Copyright   :  Kazuki Okamoto
-- License     :  see LICENSE
-- Maintainer  :  kazuki.okamoto@kakkun61.com
-- Stability   :  experimental
-- Portability :  GHC

module Data.Tuple.List.Data
  ( Cons
  , Head
  , Tail
  , Init
  , Last
  , Length
  , HasHead' (head')
  , HasTail' (tail')
  , HasInit' (init')
  , HasLast' (last')
  , HasCons' (cons')
  , HasUncons' (uncons')
  , HasHead (head)
  , HasTail (tail)
  , HasInit (init)
  , HasLast (last)
  , HasCons (cons)
  , HasUncons (uncons)
  , HasLength (length)
  , pattern Null
  , pattern Cons'
  , pattern Cons
  , Reverse
  , HasReverse' (reverse')
  , HasReverse (reverse)
  , type (!!)
  , HasAt' ((!!!), at')
  , HasAt ((!!), at)
  ) where

import Prelude (Integral, fromInteger, ($))

import Data.Functor.Identity (Identity)
import Data.Kind             (Type)
import Data.Proxy            (Proxy (Proxy))
import Data.Tuple.OneTuple   (OneTuple)
import Data.Tuple.Only       (Only)
import GHC.TypeLits          (KnownNat, Nat, natVal)

-- Basic functions

type family Cons a u :: Type
type family Head t :: Type
type family Tail t :: Type
type family Init t :: Type
type family Last t :: Type
type family Length t :: Nat

class HasHead' t a where
  head' :: t -> a

class HasTail' t u where
  tail' :: t -> u

class HasInit' t s where
  init' :: t -> s

class HasLast' t z where
  last' :: t -> z

class HasCons' t a u where
  cons' :: a -> u -> t

class HasUncons' t a u where
  uncons' :: t -> (a, u)

class HasHead t where
  head :: t -> Head t
  default head :: HasHead' t (Head t) => t -> Head t
  head = head'

class HasTail t where
  tail :: t -> Tail t
  default tail :: HasTail' t (Tail t) => t -> Tail t
  tail = tail'

class HasInit t where
  init :: t -> Init t
  default init :: HasInit' t (Init t) => t -> Init t
  init = init'

class HasLast t where
  last :: t -> Last t
  default last :: HasLast' t (Last t) => t -> Last t
  last = last'

class HasCons a u where
  cons :: a -> u -> Cons a u
  default cons :: HasCons' (Cons a u) a u => a -> u -> Cons a u
  cons = cons'

class HasUncons t  where
  uncons :: t -> (Head t, Tail t)
  default uncons :: HasUncons' t (Head t) (Tail t) => t -> (Head t, Tail t)
  uncons = uncons'

class HasLength t where
  length :: Integral n => t -> n
  default length :: (Integral n, KnownNat (Length t)) => t -> n
  length _ = fromInteger $ natVal (Proxy :: Proxy (Length t))

pattern Null :: Length t ~ 0 => t
pattern Null <- _

pattern Cons' :: (HasCons' t a u, HasUncons' t a u) => a -> u -> t
pattern Cons' a u <- (uncons' -> (a, u)) where
  Cons' a u = cons' a u

pattern Cons :: (HasCons a u, HasUncons t, t ~ Cons a u, a ~ Head t, u ~ Tail t) => a -> u -> t
pattern Cons a u <- (uncons -> (a, u)) where
  Cons a u = cons a u

-- List transformations

type family Reverse t = (r :: Type) | r -> t

class HasReverse' t r where
  reverse' :: t -> r

class HasReverse t where
  reverse :: t -> Reverse t
  default reverse :: HasReverse' t (Reverse t) => t -> Reverse t
  reverse = reverse'

-- Indexing tuples

type family t !! (n :: Nat) :: Type

class HasAt' t (n :: Nat) e where
  (!!!) :: t -> proxy n -> e
  t !!! _ = at' @t @n @e t
  at' :: t -> e
  at' t = t !!! (Proxy :: Proxy n)

class HasAt t (n :: Nat) where
  (!!) :: t -> proxy n -> t !! n
  default (!!) :: HasAt' t n (t !! n) => t -> proxy n -> t !! n
  (!!) = (!!!)
  at :: t -> t !! n
  at t = t !! (Proxy :: Proxy n)

-- Complete pragmas

{-# COMPLETE Null :: () #-}

{-# COMPLETE Null :: Proxy #-}

{-# COMPLETE Cons' :: Identity #-}
{-# COMPLETE Cons' :: OneTuple #-}
{-# COMPLETE Cons' :: Only #-}
{-# COMPLETE Cons :: Identity #-}
{-# COMPLETE Cons :: OneTuple #-}
{-# COMPLETE Cons :: Only #-}

{-# COMPLETE Cons :: (,) #-}
{-# COMPLETE Cons' :: (,) #-}

{-# COMPLETE Cons' :: (,,) #-}
{-# COMPLETE Cons :: (,,) #-}
