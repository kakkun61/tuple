{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Copyright   :  Kazuki Okamoto
-- License     :  see LICENSE
-- Maintainer  :  kazuki.okamoto@kakkun61.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- List-like operations for tuples.
--
-- This is a bit tricky of classes because Haskell does not have 1-tuples.
-- If you use 'Data.Tuple.Only.Only', 'Data.Tuple.OneTuple.OneTuple' or 'Data.Functor.Identity.Identity' as 1-tuples,
-- import @Data.Tuple.List.Only@, @Data.Tuple.List.OneTuple@ or @Data.Tuple.List.Identity@ respectively
-- and classes without a prime (dash) symbol, for examle 'HasHead'', are useful,
-- you can also use classes with a prime (dash) symbol.
-- If you use 'Data.Tuple.Single.Single' class for polymorphic 1-tuples, you should use classes with a prime (dash) symbol.

module Data.Tuple.List
  ( -- * Type families
    Cons
  , Head
  , Last
  , Tail
  , Init
  , Length
    -- * Type classes
    -- This clases are for all n-tuples including abstract 1-tuples, 2-tuples.
  , HasHead' (..)
  , HasLast' (..)
  , HasTail' (..)
  , HasInit' (..)
  , HasCons' (..)
  , HasUncons' (..)
    -- * More concrete type classes
    -- This classes are for n-tuples (n ≧ 2) and for concrete 1-tuples, 2-tupes.
  , HasHead (..)
  , HasLast (..)
  , HasTail (..)
  , HasInit (..)
  , HasCons (..)
  , HasUncons (..)
  , HasLength (..)
    -- * Patterns
  , pattern Null
  , pattern Cons'
  , pattern Cons
  ) where

import           Prelude               (Int, Integral, fromInteger, ($))

import           Data.Functor.Identity (Identity)
import           Data.Kind             (Type)
import           Data.Proxy            (Proxy (Proxy))
import           Data.Tuple.OneTuple   (OneTuple)
import           Data.Tuple.Only       (Only)
import           Data.Tuple.Single     (Single (unwrap, wrap))
import           GHC.TypeLits          (ErrorMessage (Text), KnownNat, Nat,
                                        TypeError, natVal)

type family Cons a u :: Type
type family Head t :: Type
type family Last t :: Type
type family Tail t :: Type
type family Init t :: Type
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

pattern Null :: HasLength t => t
pattern Null <- (length -> 0 :: Int)

pattern Cons' :: (HasCons' t a u, HasUncons' t a u) => a -> u -> t
pattern Cons' a u <- (uncons' -> (a, u)) where
  Cons' a u = cons' a u

pattern Cons :: (HasCons a u, HasUncons t, t ~ Cons a u, a ~ Head t, u ~ Tail t) => a -> u -> t
pattern Cons a u <- (uncons -> (a, u)) where
  Cons a u = cons a u

-- 0

type instance Head () = TypeError (Text "empty tuple")
type instance Last () = TypeError (Text "empty tuple")
type instance Tail () = TypeError (Text "empty tuple")
type instance Init () = TypeError (Text "empty tuple")
type instance Length () = 0

instance HasLength ()

{-# COMPLETE Null :: () #-}

-- 1

instance {-# OVERLAPPABLE #-} (Single c, t ~ c a) => HasHead' t a where
  head' = unwrap

instance Single c => HasTail' (c a) () where
  tail' _ = ()

instance Single c => HasInit' (c a) () where
  init' _ = ()

instance {-# OVERLAPPABLE #-} Single c => HasLast' (c a) a where
  last' = unwrap

instance Single c => HasCons' (c a) a () where
  cons' a () = wrap a

instance Single c => HasUncons' (c a) a () where
  uncons' t = (unwrap t, ())

{-# COMPLETE Cons' :: Identity #-}
{-# COMPLETE Cons' :: OneTuple #-}
{-# COMPLETE Cons' :: Only #-}
{-# COMPLETE Cons :: Identity #-}
{-# COMPLETE Cons :: OneTuple #-}
{-# COMPLETE Cons :: Only #-}

-- 2

type instance Head (a, b) = a
type instance Last (a, b) = b
type instance Length (a, b) = 2

instance HasHead' (a, b) a where
  head' (a, _) = a

instance Single c => HasTail' (a, b) (c b) where
  tail' (_, b) = wrap b

instance Single c => HasInit' (a, b) (c a) where
  init' (a, _) = wrap a

instance HasLast' (a, b) b where
  last' (_, b) = b

instance Single c => HasCons' (a, b) a (c b) where
  cons' a u = (a, unwrap u)

instance Single c => HasUncons' (a, b) a (c b) where
  uncons' (a, b) = (a, wrap b)

instance HasHead (a, b)

instance HasLast (a, b)

instance HasLength (a, b)

{-# COMPLETE Cons' :: (,) #-}
{-# COMPLETE Cons :: (,) #-}

-- 3

type instance Cons a (b, c) = (a, b, c)
type instance Head (a, b, c) = a
type instance Last (a, b, c) = c
type instance Tail (a, b, c) = (b, c)
type instance Init (a, b, c) = (a, b)
type instance Length (a, b, c) = 3

instance HasHead' (a, b, c) a where
  head' (a, _, _) = a

instance HasTail' (a, b, c) (b, c) where
  tail' (_, b, c) = (b, c)

instance HasInit' (a, b, c) (a, b) where
  init' (a, b, _) = (a, b)

instance HasLast' (a, b, c) c where
  last' (_, _, c) = c

instance HasCons' (a, b, c) a (b, c) where
  cons' a (b, c) = (a, b, c)

instance HasUncons' (a, b, c) a (b, c) where
  uncons' (a, b, c) = (a, (b, c))

instance HasHead (a, b, c)

instance HasTail (a, b, c)

instance HasInit (a, b, c)

instance HasLast (a, b, c)

instance HasCons a (b, c)

instance HasUncons (a, b, c)

instance HasLength (a, b, c)

{-# COMPLETE Cons' :: (,,) #-}
{-# COMPLETE Cons :: (,,) #-}

-- 4

type instance Cons a (b, c, d) = (a, b, c, d)
type instance Head (a, b, c, d) = a
type instance Last (a, b, c, d) = d
type instance Tail (a, b, c, d) = (b, c, d)
type instance Init (a, b, c, d) = (a, b, c)
type instance Length (a, b, c, d) = 4

instance HasHead' (a, b, c, d) a where
  head' (a, _, _, _) = a

instance HasTail' (a, b, c, d) (b, c, d) where
  tail' (_, b, c, d) = (b, c, d)

instance HasInit' (a, b, c, d) (a, b, c) where
  init' (a, b, c, _) = (a, b, c)

instance HasLast' (a, b, c, d) d where
  last' (_, _, _, d) = d

instance HasCons' (a, b, c, d) a (b, c, d) where
  cons' a (b, c, d) = (a, b, c, d)

instance HasUncons' (a, b, c, d) a (b, c, d) where
  uncons' (a, b, c, d) = (a, (b, c, d))

instance HasHead (a, b, c, d)

instance HasTail (a, b, c, d)

instance HasInit (a, b, c, d)

instance HasLast (a, b, c, d)

instance HasCons a (b, c, d)

instance HasUncons (a, b, c, d)

instance HasLength (a, b, c, d)

{-# COMPLETE Cons' :: (,,,) #-}
{-# COMPLETE Cons :: (,,,) #-}
