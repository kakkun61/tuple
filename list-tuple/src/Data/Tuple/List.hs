{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wno-redundant-constraints#-}

module Data.Tuple.List
  ( Cons
  , Head
  , Last
  , Tail
  , Init
  , Length
  , HeadTailUnique (..)
  , TupleUnique (..)
  , TupleTailUnique (..)
  , TupleInitUnique (..)
  , head1
  , last1
  , null1
  , length1
    -- Patterns
  , pattern Null
  , pattern Cons
  , pattern Cons'
  ) where

import           Prelude           (Bool (False, True), Eq ((==)), Int,
                                    Integral, error, fromInteger, ($))

import           Data.Kind         (Type)
import           Data.Proxy        (Proxy (Proxy))
import           Data.Tuple.Single (Single (unwrap, wrap))
import           GHC.Stack         (HasCallStack)
import           GHC.TypeLits      (ErrorMessage (Text),
                                    KnownNat, Nat, TypeError, natVal)

type family Cons a u :: Type
type family Head t :: Type
type family Last t :: Type
type family Tail t :: Type
type family Init t :: Type
type family Length t :: Nat

class TupleUnique t where
  head :: t -> Head t

  last :: t -> Last t

  tail' :: t -> Tail t
  default tail' :: TupleTailUnique t (Tail t) => t -> Tail t
  tail' = tail

  init' :: t -> Init t
  default init' :: TupleInitUnique t (Init t) => t -> Init t
  init' = init

  uncons' :: t -> (Head t, Tail t)
  uncons' t = (head t, tail' t)

  null :: t -> Bool
  null t = length t == (0 :: Int)

  length :: Integral n => t -> n
  default length :: (Integral n, KnownNat (Length t)) => t -> n
  length _ = fromInteger $ natVal (Proxy :: Proxy (Length t))

class HeadTailUnique a u where
  cons' :: a -> u -> Cons a u
  default cons' :: (TupleTailUnique (Cons a u) u, a ~ Head (Cons a u)) => a -> u -> Cons a u
  cons' = cons

class TupleTailUnique t u where
  cons :: Head t -> u -> t
  default cons :: (HeadTailUnique (Head t) u, t ~ Cons (Head t) u) => Head t -> u -> t
  cons = cons'

  uncons :: a ~ Head t => t -> (a, u)
  default uncons :: (TupleUnique t, a ~ Head t, u ~ Tail t) => t -> (a, u)
  uncons t = (head t, tail' t)

  tail :: t -> u
  default tail :: (TupleUnique t, u ~ Tail t) => t -> u
  tail = tail'

class TupleInitUnique t s where
  init :: t -> s
  default init :: (TupleUnique t, s ~ Init t) => t -> s
  init = init'

-- 0

type instance Head () = TypeError (Text "() does not have more than 0 elements")
type instance Last () = TypeError (Text "() does not have more than 0 elements")
type instance Tail () = TypeError (Text "() does not have more than 0 elements")
type instance Init () = TypeError (Text "() does not have more than 0 elements")
type instance Length () = 0

instance TupleUnique () where
  head = neverReachHere
  last = neverReachHere
  tail' = neverReachHere
  init' = neverReachHere
  null _ = True
  length _ = 0

-- 1

instance (Single t, a ~ Head (t a)) => TupleTailUnique (t a) () where
  cons a _ = wrap a
  uncons t = (unwrap t, ())
  tail _ = ()

instance (Single t, a ~ Head (t a)) => TupleInitUnique (t a) () where
  init _ = ()

head1 :: Single t => t a -> a
head1 = unwrap

last1 :: Single t => t a -> a
last1 = unwrap

null1 :: t a -> Bool
null1 _ = False

length1 :: Integral n => t a -> n
length1 _ = 1

-- 2

type instance Head (a, b) = a
type instance Last (a, b) = b
-- type instance Tail (a, b) = TypeError (Text "singleton tuple is not determined uniquely")
-- type instance Init (a, b) = TypeError (Text "singleton tuple is not determined uniquely")
type instance Length (a, b) = 2

instance TupleUnique (a, b) where
  head (a, _) = a
  last (_, b) = b
  tail' = neverReachHere -- TODO No!
  init' = neverReachHere
  uncons' = neverReachHere
  null _ = False
  length _ = 2

instance Single t => TupleTailUnique (a, b) (t b) where
  cons a tb = (a, unwrap tb)
  uncons (a, b) = (a, wrap b)
  tail (_, b) = wrap b

instance Single t => TupleInitUnique (a, b) (t a) where
  init (a, _) = wrap a

-- 3

type instance Cons a (b, c) = (a, b, c)
type instance Head (a, b, c) = a
type instance Last (a, b, c) = c
type instance Tail (a, b, c) = (b, c)
type instance Init (a, b, c) = (a, b)
type instance Length (a, b, c) = 3

instance HeadTailUnique a (b, c) where
  cons' a (b, c) = (a, b, c)

instance TupleUnique (a, b, c) where
  head (a, _, _) = a
  last (_, _, c) = c
  tail' (_, b, c) = (b, c)
  init' (a, b, _) = (a, b)
  uncons' (a, b, c) = (a, (b, c))
  null _ = False
  length _ = 3
  -- type Reverse (a, b, c) = (c, b, a)
  -- reverse (a, b, c) = (c, b, a)

instance TupleTailUnique (a, b, c) (b, c)

instance TupleInitUnique (a, b, c) (a, b)

-- patterns

pattern Null :: TupleUnique t => t
pattern Null <- (null -> True)

pattern Cons :: (TupleTailUnique t u, a ~ Head t) => a -> u -> t
pattern Cons a t <- (uncons -> (a, t)) where
  Cons a t = cons a t

pattern Cons' :: (HeadTailUnique a u, TupleUnique t, t ~ Cons a u, a ~ Head t, u ~ Tail t) => a -> u -> t
pattern Cons' a t <- (uncons' -> (a, t)) where
  Cons' a t = cons' a t

-- etc.

neverReachHere :: HasCallStack => a
neverReachHere = error "never reach here"
