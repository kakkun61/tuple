{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE Trustworthy            #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Copyright   :  Kazuki Okamoto
-- License     :  see LICENSE
-- Maintainer  :  kazuki.okamoto@kakkun61.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Homotuples, whoes items are the same type or which are lists with type-level length.

module Data.Tuple.Homotuple
  ( Homotuple
    -- * List-like
  , replicate
    -- * Functor-like
  , (<$>)
    -- * Applicative-like
  , (<*>)
  , pure
    -- * Monad-like
  , (>>=)
    -- * Semigoupe-like
  , (<>)
    -- * Monoid-like
  , empty
    -- * Utility constraints
  , IsHomolisttuple
  , IsHomotupleItem
    -- * For implementers
  , errorLengthMismatch
  ) where

import Prelude (Num (fromInteger), error, ($), (.))

import qualified Control.Applicative as A
import qualified Control.Monad       as M
import           Data.Kind           (Type)
import qualified Data.List           as L
import           Data.Proxy          (Proxy (Proxy))
import qualified Data.Semigroup      as S
import           Data.Tuple.Single   (Single (wrap))
import           GHC.Exts            (IsList (Item, fromList, toList))
import           GHC.Stack           (HasCallStack)
import           GHC.TypeLits        (type (*), type (+), KnownNat, Nat, natVal)

type family Homotuple (n :: Nat) (a :: Type) = (t :: Type) | t -> n

type instance Homotuple 0 a = ()

type instance Homotuple 2 a = (a, a)

type instance Homotuple 3 a = (a, a, a)

type instance Homotuple 4 a = (a, a, a, a)

type instance Homotuple 5 a = (a, a, a, a, a)

type instance Homotuple 6 a = (a, a, a, a, a, a)

type instance Homotuple 7 a = (a, a, a, a, a, a, a)

type instance Homotuple 8 a = (a, a, a, a, a, a, a, a)

type instance Homotuple 9 a = (a, a, a, a, a, a, a, a, a)

type instance Homotuple 10 a = (a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 11 a = (a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 12 a = (a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 13 a = (a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 14 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 15 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 16 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 17 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 18 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 19 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 20 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 21 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 22 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 23 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 24 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 25 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 26 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 27 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 28 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 29 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 30 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 31 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 32 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 33 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 34 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 35 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 36 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 37 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 38 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 39 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 40 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 41 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 42 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 43 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 44 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 45 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 46 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 47 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 48 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 49 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 50 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 51 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 52 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 53 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 54 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 55 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 56 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 57 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 58 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 59 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 60 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 61 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

type instance Homotuple 62 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

instance IsList () where
  type Item () = ()
  fromList [] = ()
  fromList _  = errorLengthMismatch
  toList () = []

instance IsList (a, a) where
  type Item (a, a) = a
  fromList [a, b] = (a, b)
  fromList _      = errorLengthMismatch
  toList (a, b) = [a, b]

instance IsList (a, a, a) where
  type Item (a, a, a) = a
  fromList [a, b, c] = (a, b, c)
  fromList _         = errorLengthMismatch
  toList (a, b, c) = [a, b, c]

instance IsList (a, a, a, a) where
  type Item (a, a, a, a) = a
  fromList [a, b, c, d] = (a, b, c, d)
  fromList _            = errorLengthMismatch
  toList (a, b, c, d) = [a, b, c, d]

instance IsList (a, a, a, a, a) where
  type Item (a, a, a, a, a) = a
  fromList [a, b, c, d, e] = (a, b, c, d, e)
  fromList _               = errorLengthMismatch
  toList (a, b, c, d, e) = [a, b, c, d, e]

instance IsList (a, a, a, a, a, a) where
  type Item (a, a, a, a, a, a) = a
  fromList [a, b, c, d, e, f] = (a, b, c, d, e, f)
  fromList _                  = errorLengthMismatch
  toList (a, b, c, d, e, f) = [a, b, c, d, e, f]

instance IsList (a, a, a, a, a, a, a) where
  type Item (a, a, a, a, a, a, a) = a
  fromList [a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
  fromList _                     = errorLengthMismatch
  toList (a, b, c, d, e, f, g) = [a, b, c, d, e, f, g]

instance IsList (a, a, a, a, a, a, a, a) where
  type Item (a, a, a, a, a, a, a, a) = a
  fromList [a, b, c, d, e, f, g, h] = (a, b, c, d, e, f, g, h)
  fromList _                        = errorLengthMismatch
  toList (a, b, c, d, e, f, g, h) = [a, b, c, d, e, f, g, h]

instance IsList (a, a, a, a, a, a, a, a, a) where
  type Item (a, a, a, a, a, a, a, a, a) = a
  fromList [a, b, c, d, e, f, g, h, i] = (a, b, c, d, e, f, g, h, i)
  fromList _                           = errorLengthMismatch
  toList (a, b, c, d, e, f, g, h, i) = [a, b, c, d, e, f, g, h, i]

instance IsList (a, a, a, a, a, a, a, a, a, a) where
  type Item (a, a, a, a, a, a, a, a, a, a) = a
  fromList [a, b, c, d, e, f, g, h, i, j] = (a, b, c, d, e, f, g, h, i, j)
  fromList _                              = errorLengthMismatch
  toList (a, b, c, d, e, f, g, h, i, j) = [a, b, c, d, e, f, g, h, i, j]

instance IsList (a, a, a, a, a, a, a, a, a, a, a) where
  type Item (a, a, a, a, a, a, a, a, a, a, a) = a
  fromList [a, b, c, d, e, f, g, h, i, j, k] = (a, b, c, d, e, f, g, h, i, j, k)
  fromList _                                 = errorLengthMismatch
  toList (a, b, c, d, e, f, g, h, i, j, k) = [a, b, c, d, e, f, g, h, i, j, k]

instance IsList (a, a, a, a, a, a, a, a, a, a, a, a) where
  type Item (a, a, a, a, a, a, a, a, a, a, a, a) = a
  fromList [a, b, c, d, e, f, g, h, i, j, k, l] = (a, b, c, d, e, f, g, h, i, j, k, l)
  fromList _                                    = errorLengthMismatch
  toList (a, b, c, d, e, f, g, h, i, j, k, l) = [a, b, c, d, e, f, g, h, i, j, k, l]

instance IsList (a, a, a, a, a, a, a, a, a, a, a, a, a) where
  type Item (a, a, a, a, a, a, a, a, a, a, a, a, a) = a
  fromList [a, b, c, d, e, f, g, h, i, j, k, l, m] = (a, b, c, d, e, f, g, h, i, j, k, l, m)
  fromList _                                       = errorLengthMismatch
  toList (a, b, c, d, e, f, g, h, i, j, k, l, m) = [a, b, c, d, e, f, g, h, i, j, k, l, m]

instance IsList (a, a, a, a, a, a, a, a, a, a, a, a, a, a) where
  type Item (a, a, a, a, a, a, a, a, a, a, a, a, a, a) = a
  fromList [a, b, c, d, e, f, g, h, i, j, k, l, m, n] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  fromList _                                          = errorLengthMismatch
  toList (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = [a, b, c, d, e, f, g, h, i, j, k, l, m, n]

instance IsList (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a) where
  type Item (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a) = a
  fromList [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  fromList _                                             = errorLengthMismatch
  toList (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o]

errorLengthMismatch :: HasCallStack => a
errorLengthMismatch = error "length mismatch"

-- Utility constraints

type IsHomolisttuple (n :: Nat) a = IsList (Homotuple n a)
type IsHomotupleItem (n :: Nat) a = a ~ Item (Homotuple n a)

-- List-like

replicate :: forall (n :: Nat) a. (IsHomolisttuple n a, IsHomotupleItem n a, KnownNat n) => a -> Homotuple n a
replicate = fromList . L.replicate (fromInteger $ natVal (Proxy :: Proxy n))

-- Functor-like

(<$>)
  :: ( IsHomolisttuple n a
     , IsHomolisttuple n b
     , IsHomotupleItem n a
     , IsHomotupleItem n b
     )
  => (a -> b) -> Homotuple n a -> Homotuple n b
f <$> t = fromList $ L.map f $ toList t

-- Applicative-like

(<*>)
  :: ( IsHomolisttuple n0 (a -> b)
     , IsHomolisttuple n1 a
     , IsHomolisttuple (n0 * n1) b
     , IsHomotupleItem n0 (a -> b)
     , IsHomotupleItem n1 a
     , IsHomotupleItem (n0 * n1) b
     )
  => Homotuple n0 (a -> b) -> Homotuple n1 a -> Homotuple (n0 * n1) b
f <*> t = fromList $ toList f A.<*> toList t

pure :: Single c => a -> c a
pure = wrap

-- Monad-like

(>>=)
  :: ( IsHomolisttuple n0 a
     , IsHomolisttuple n1 b
     , IsHomolisttuple (n0 * n1) b
     , IsHomotupleItem n0 a
     , IsHomotupleItem n1 b
     , IsHomotupleItem (n0 * n1) b
     )
  => Homotuple n0 a -> (a -> Homotuple n1 b) -> Homotuple (n0 * n1) b
m >>= f = fromList $ toList m M.>>= (toList . f)

-- Semigroup-like

(<>)
  :: ( IsHomolisttuple n0 a
     , IsHomolisttuple n1 a
     , IsHomolisttuple (n0 + n1) a
     , IsHomotupleItem n0 a
     , IsHomotupleItem n1 a
     , IsHomotupleItem (n0 + n1) a
     )
  => Homotuple n0 a
  -> Homotuple n1 a
  -> Homotuple (n0 + n1) a
a <> b = fromList $ toList a S.<> toList b

infixr 6 <>

-- Monoid-like

empty :: Homotuple 0 a
empty = ()
