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

module Data.Tuple.List
  ( Construct (..)
  , Destruct (..)
  , HasCons (..)
  , HasTail (..)
  , HasInit (..)
  , head1
  , last1
  , null1
  , length1
  , HasAt (..)
  , at1
    -- Patterns
  , pattern Null
  , pattern Cons
  , pattern Cons'
  ) where


import           Prelude                 (Bool (False, True), Eq ((==)), Int,
                                          Integral, error, fromInteger, ($))

import           Data.Kind               (Type)
import           Data.Proxy              (Proxy (Proxy))
import           Data.Tuple.Single.Class (Single (unwrap, wrap))
import           GHC.Stack               (HasCallStack)
import           GHC.TypeLits            (ErrorMessage ((:<>:), ShowType, Text),
                                          KnownNat, Nat, TypeError, natVal)

class Construct a u where
  type Cons a u :: Type
  cons' :: a -> u -> Cons a u
  default cons' :: HasCons (Cons a u) a u => a -> u -> Cons a u
  cons' = cons

class HasCons t a u where
  cons :: a -> u -> t
  default cons :: (Construct a u, t ~ Cons a u) => a -> u -> t
  cons = cons'

  uncons :: t -> (a, u)
  default uncons :: (Destruct t, a ~ Head t, u ~ Tail t) => t -> (a, u)
  uncons t = (head t, tail' t)

class Destruct t where
  type Head t :: Type
  head :: t -> Head t

  type Last t :: Type
  last :: t -> Last t

  type Tail t :: Type
  tail' :: t -> Tail t
  default tail' :: HasTail t (Tail t) => t -> Tail t
  tail' = tail

  type Init t :: Type
  init' :: t -> Init t
  default init' :: HasInit t (Init t) => t -> Init t
  init' = init

  uncons' :: t -> (Head t, Tail t)
  uncons' t = (head t, tail' t)

  null :: t -> Bool
  null t = length t == (0 :: Int)

  type Length t :: Nat

  length :: Integral n => t -> n
  default length :: (Integral n, KnownNat (Length t)) => t -> n
  length _ = fromInteger $ natVal (Proxy :: Proxy (Length t))

class HasTail t u where
  tail :: t -> u
  default tail :: (Destruct t, u ~ Tail t) => t -> u
  tail = tail'

class HasInit t s where
  init :: t -> s
  default init :: (Destruct t, s ~ Init t) => t -> s
  init = init'

class HasAt t (n :: Nat) where
  type Item t n :: Type
  at :: t -> proxy n -> Item t n

-- 0

instance Destruct () where
  type Head () = TypeError (ShowType () :<>: Text " does not have more than 0 elements")
  head = neverReachHere
  type Last () = TypeError (ShowType () :<>: Text " does not have more than 0 elements")
  last = neverReachHere
  type Tail () = TypeError (ShowType () :<>: Text " does not have more than 0 elements")
  tail' = neverReachHere
  type Init () = TypeError (ShowType () :<>: Text " does not have more than 0 elements")
  init' = neverReachHere
  null _ = True
  type Length () = 0
  length _ = 0

-- 1

instance Single t => HasCons (t a) a () where
  cons a _ = wrap a
  uncons t = (unwrap t, ())

instance HasTail (t a) () where
  tail _ = ()

instance HasInit (t a) () where
  init _ = ()

head1 :: Single t => t a -> a
head1 = unwrap

last1 :: Single t => t a -> a
last1 = unwrap

null1 :: t a -> Bool
null1 _ = False

length1 :: Integral n => t a -> n
length1 _ = 1

at1 :: Single t => t a -> proxy (n :: Nat) -> a
at1 t _ = unwrap t

-- 2

instance Destruct (a, b) where
  type Head (a, b) = a
  head (a, _) = a
  type Last (a, b) = b
  last (_, b) = b
  type Tail (a, b) = TypeError (Text "singleton tuple is not determined uniquely")
  tail' = neverReachHere
  type Init (a, b) = TypeError (Text "singleton tuple is not determined uniquely")
  init' = neverReachHere
  null _ = False
  type Length (a, b) = 2
  length _ = 2

instance Single t => HasCons (a, b) a (t b) where
  cons a tb = (a, unwrap tb)
  uncons (a, b) = (a, wrap b)

instance Single t => HasTail (a, b) (t b) where
  tail (_, b) = wrap b

instance Single t => HasInit (a, b) (t a) where
  init (a, _) = wrap a

instance HasAt (a, b) 0 where
  type Item (a, b) 0 = a
  at (a, _) _ = a

instance HasAt (a, b) 1 where
  type Item (a, b) 1 = b
  at (_, b) _ = b

-- 3

instance Construct a (b, c) where
  type Cons a (b, c) = (a, b, c)
  cons' a (b, c) = (a, b, c)

instance Destruct (a, b, c) where
  type Head (a, b, c) = a
  head (a, _, _) = a
  type Last (a, b, c) = c
  last (_, _, c) = c
  type Tail (a, b, c) = (b, c)
  tail' (_, b, c) = (b, c)
  type Init (a, b, c) = (a, b)
  init' (a, b, _) = (a, b)
  uncons' (a, b, c) = (a, (b, c))
  null _ = False
  type Length (a, b, c) = 3
  length _ = 3
  -- type Reverse (a, b, c) = (c, b, a)
  -- reverse (a, b, c) = (c, b, a)

instance HasCons (a, b, c) a (b, c)

instance HasTail (a, b, c) (b, c)

instance HasInit (a, b, c) (a, b)

-- patterns

pattern Null :: Destruct t => t
pattern Null <- (null -> True)

pattern Cons :: HasCons t a u => a -> u -> t
pattern Cons a t <- (uncons -> (a, t)) where
  Cons a t = cons a t

pattern Cons' :: (Construct a u, Destruct t, t ~ Cons a u, a ~ Head t, u ~ Tail t) => a -> u -> t
pattern Cons' a t <- (uncons' -> (a, t)) where
  Cons' a t = cons' a t

-- etc.

neverReachHere :: HasCallStack => a
neverReachHere = error "never reach here"
