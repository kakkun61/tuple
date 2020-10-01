{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE PatternSynonyms        #-}
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
-- Homotuples, whose items are the same type or which are lists with type-level length.

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
    -- * Semigroupe-like
  , (<>)
    -- * Monoid-like
  , pattern Empty
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

-- 0

type instance Homotuple 0 a = Proxy a

instance IsList (Proxy a) where
  type Item (Proxy a) = a
  fromList [] = Proxy
  fromList _  = errorLengthMismatch
  toList _ = []

---- embed 2

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

---- embed 16

---- embed 17

---- embed 18

---- embed 19

---- embed 20

---- embed 21

---- embed 22

---- embed 23

---- embed 24

---- embed 25

---- embed 26

---- embed 27

---- embed 28

---- embed 29

---- embed 30

---- embed 31

---- embed 32

---- embed 33

---- embed 34

---- embed 35

---- embed 36

---- embed 37

---- embed 38

---- embed 39

---- embed 40

---- embed 41

---- embed 42

---- embed 43

---- embed 44

---- embed 45

---- embed 46

---- embed 47

---- embed 48

---- embed 49

---- embed 50

---- embed 51

---- embed 52

---- embed 53

---- embed 54

---- embed 55

---- embed 56

---- embed 57

---- embed 58

---- embed 59

---- embed 60

---- embed 61

---- embed 62

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

pattern Empty :: Homotuple 0 a
pattern Empty = Proxy
