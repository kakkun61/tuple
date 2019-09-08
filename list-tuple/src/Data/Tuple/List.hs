{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-orphans #-}

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

import Data.Tuple.List.Data (type (!!), Cons, pattern Cons, pattern Cons', HasAt (at, (!!)), HasAt' (at', (!!!)),
                             HasCons (cons), HasCons' (cons'), HasHead (head), HasHead' (head'), HasInit (init),
                             HasInit' (init'), HasLast (last), HasLast' (last'), HasLength (length),
                             HasReverse (reverse), HasReverse' (reverse'), HasTail (tail), HasTail' (tail'),
                             HasUncons (uncons), HasUncons' (uncons'), Head, Init, Last, Length, pattern Null, Reverse,
                             Tail)
import Data.Tuple.List.TH   (list)

import Prelude (error, id, mconcat, sequence, (<$>))

import Data.Proxy        (Proxy)
import Data.Tuple.Single (Single (unwrap, wrap))
import GHC.TypeLits      (ErrorMessage (Text), TypeError)

-- 0

--- Unit

type instance Head () = TypeError (Text "empty tuple")
type instance Tail () = TypeError (Text "empty tuple")
type instance Init () = TypeError (Text "empty tuple")
type instance Last () = TypeError (Text "empty tuple")
type instance Length () = 0

instance HasLength ()

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

-- n

$(mconcat <$> sequence (list <$> [3 .. 15]))
