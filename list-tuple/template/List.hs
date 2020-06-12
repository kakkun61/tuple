{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE Safe                   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

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
  ( -- * Basic functions
    -- ** Type families
    Cons
  , Head
  , Last
  , Tail
  , Init
  , Length
    -- ** Type classes
    -- This clases are for all n-tuples including abstract 1-tuples, 2-tuples.
  , HasHead' (..)
  , HasLast' (..)
  , HasTail' (..)
  , HasInit' (..)
  , HasCons' (..)
  , HasUncons' (..)
    -- ** More concrete type classes
    -- This classes are for n-tuples (n ≦ 2) and for concrete 1-tuples, 2-tupes.
  , HasHead (..)
  , HasLast (..)
  , HasTail (..)
  , HasInit (..)
  , HasCons (..)
  , HasUncons (..)
  , HasLength (..)
    -- ** Patterns
  , pattern Null
  , pattern Cons'
  , pattern Cons
    -- * List transfomations
  , Reverse
  , HasReverse (..)
  , HasReverse' (..)
    -- * Indexing tuples
  , type (!!)
  , HasAt' (..)
  , HasAt (..)
  ) where

import Prelude (Integral, error, fromInteger, id, ($))

import Data.Functor.Identity (Identity)
import Data.Kind             (Type)
import Data.Proxy            (Proxy (Proxy))
import Data.Tuple.OneTuple   (OneTuple)
import Data.Tuple.Only       (Only)
import Data.Tuple.Single     (Single (unwrap, wrap))
import GHC.TypeLits          (ErrorMessage (Text), KnownNat, Nat, TypeError, natVal)

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

-- 0

--- Unit

type instance Head () = TypeError (Text "empty tuple")
type instance Tail () = TypeError (Text "empty tuple")
type instance Init () = TypeError (Text "empty tuple")
type instance Last () = TypeError (Text "empty tuple")
type instance Length () = 0

instance HasLength ()

{-# COMPLETE Null :: () #-}

type instance Reverse () = ()

instance HasReverse' () () where
  reverse' = id

instance HasReverse ()

--- Proxy

type instance Head (Proxy a) = TypeError (Text "empty tuple")
type instance Tail (Proxy a) = TypeError (Text "empty tuple")
type instance Init (Proxy a) = TypeError (Text "empty tuple")
type instance Last (Proxy a) = TypeError (Text "empty tuple")
type instance Length (Proxy a) = 0

instance TypeError (Text "empty tuple") => HasTail' (Proxy a) b where
  tail' = error "never reach here"

instance TypeError (Text "empty tuple") => HasInit' (Proxy a) b where
  init' = error "never reach here"

instance HasLength (Proxy a)

{-# COMPLETE Null :: Proxy #-}

type instance Reverse (Proxy a) = (Proxy a)

instance {-# OVERLAPPING #-} HasReverse' (Proxy a) (Proxy a) where
  reverse' = id

instance HasReverse (Proxy a)

-- 1

instance {-# OVERLAPPABLE #-} (Single c, t ~ c a) => HasHead' t a where
  head' = unwrap

instance {-# OVERLAPPABLE #-} (Single c, b ~ ()) => HasTail' (c a) b where
  tail' _ = ()

instance {-# OVERLAPPABLE #-} (Single c, b ~ ()) => HasInit' (c a) b where
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

instance {-# OVERLAPPABLE #-} Single c => HasLength (c a) where
  length _ = 1

instance {-# OVERLAPPABLE #-} (Single c0, Single c1, c0 ~ c1, a ~ b) => HasReverse' (c0 a) (c1 b) where
  reverse' = id

instance {-# OVERLAPPABLE #-} (Single c, a ~ b) => HasAt' (c a) 0 b where
  t !!! _ = unwrap t

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

type instance Reverse (a, b) = (b, a)

instance HasReverse' (a, b) (b, a) where
  reverse' (a, b) = (b, a)

instance HasReverse (a, b)

type instance (a, b) !! 0 = a

instance HasAt' (a, b) 0 a where
  (a, _) !!! _ = a

instance HasAt (a, b) 0

type instance (a, b) !! 1 = b

instance HasAt' (a, b) 1 b where
  (_, b) !!! _ = b

instance HasAt (a, b) 1

---- embed 3

---- embed 4

---- embed 5

---- embed 6

---- embed 7

---- embed 8

---- embed 9

---- embed 10

---- embed 11

---- embed 12

---- embed 13

---- embed 14

---- embed 15
