{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE Safe                   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
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
    -- This classes are for n-tuples (n ≧ 2) and for concrete 1-tuples, 2-tupes.
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
    -- * Indexing tuples
  , type (!!)
  , HasAt' (..)
  , HasAt (..)
  ) where

import           Prelude               (Int, Integral, fromInteger, id, ($))

import           Data.Functor.Identity (Identity)
import           Data.Kind             (Type)
import           Data.Proxy            (Proxy (Proxy))
import           Data.Tuple.OneTuple   (OneTuple)
import           Data.Tuple.Only       (Only)
import           Data.Tuple.Single     (Single (unwrap, wrap))
import           GHC.TypeLits          (ErrorMessage (Text), KnownNat, Nat,
                                        TypeError, natVal)

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

pattern Null :: HasLength t => t
pattern Null <- (length -> 0 :: Int)

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

class HasAt t (n :: Nat) where
  (!!) :: t -> proxy n -> t !! n
  default (!!) :: HasAt' t n (t !! n) => t -> proxy n -> t !! n
  (!!) = (!!!)

-- 0

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

instance Single c => HasReverse' (c a) (c a) where
  reverse' = id

instance Single c => HasAt' (c a) 0 a where
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

-- 3

type instance Cons a (b, c) = (a, b, c)
type instance Head (a, b, c) = a
type instance Tail (a, b, c) = (b, c)
type instance Init (a, b, c) = (a, b)
type instance Last (a, b, c) = c
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

type instance Reverse (a, b, c) = (c, b, a)

instance HasReverse' (a, b, c) (c, b, a) where
  reverse' (a, b, c) = (c, b, a)

instance HasReverse (a, b, c)

type instance (a, b, c) !! 0 = a

instance HasAt' (a, b, c) 0 a where
  (a, _, _) !!! _ = a

instance HasAt (a, b, c) 0

type instance (a, b, c) !! 1 = b

instance HasAt' (a, b, c) 1 b where
  (_, b, _) !!! _ = b

instance HasAt (a, b, c) 1

type instance (a, b, c) !! 2 = c

instance HasAt' (a, b, c) 2 c where
  (_, _, c) !!! _ = c

instance HasAt (a, b, c) 2

-- 4

type instance Cons a (b, c, d) = (a, b, c, d)
type instance Head (a, b, c, d) = a
type instance Tail (a, b, c, d) = (b, c, d)
type instance Init (a, b, c, d) = (a, b, c)
type instance Last (a, b, c, d) = d
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

type instance Reverse (a, b, c, d) = (d, c, b, a)

instance HasReverse' (a, b, c, d) (d, c, b, a) where
  reverse' (a, b, c, d) = (d, c, b, a)

instance HasReverse (a, b, c, d)

type instance (a, b, c, d) !! 0 = a

instance HasAt' (a, b, c, d) 0 a where
  (a, _, _, _) !!! _ = a

instance HasAt (a, b, c, d) 0

type instance (a, b, c, d) !! 1 = b

instance HasAt' (a, b, c, d) 1 b where
  (_, b, _, _) !!! _ = b

instance HasAt (a, b, c, d) 1

type instance (a, b, c, d) !! 2 = c

instance HasAt' (a, b, c, d) 2 c where
  (_, _, c, _) !!! _ = c

instance HasAt (a, b, c, d) 2

type instance (a, b, c, d) !! 3 = d

instance HasAt' (a, b, c, d) 3 d where
  (_, _, _, d) !!! _ = d

instance HasAt (a, b, c, d) 3

-- 5

type instance Cons a (b, c, d, e) = (a, b, c, d, e)
type instance Head (a, b, c, d, e) = a
type instance Tail (a, b, c, d, e) = (b, c, d, e)
type instance Init (a, b, c, d, e) = (a, b, c, d)
type instance Last (a, b, c, d, e) = e
type instance Length (a, b, c, d, e) = 5

instance HasHead' (a, b, c, d, e) a where
  head' (a, _, _, _, _) = a

instance HasTail' (a, b, c, d, e) (b, c, d, e) where
  tail' (_, b, c, d, e) = (b, c, d, e)

instance HasInit' (a, b, c, d, e) (a, b, c, d) where
  init' (a, b, c, d, _) = (a, b, c, d)

instance HasLast' (a, b, c, d, e) e where
  last' (_, _, _, _, e) = e

instance HasCons' (a, b, c, d, e) a (b, c, d, e) where
  cons' a (b, c, d, e) = (a, b, c, d, e)

instance HasUncons' (a, b, c, d, e) a (b, c, d, e) where
  uncons' (a, b, c, d, e) = (a, (b, c, d, e))

instance HasHead (a, b, c, d, e)

instance HasTail (a, b, c, d, e)

instance HasInit (a, b, c, d, e)

instance HasLast (a, b, c, d, e)

instance HasCons a (b, c, d, e)

instance HasUncons (a, b, c, d, e)

instance HasLength (a, b, c, d, e)

{-# COMPLETE Cons' :: (,,,,) #-}
{-# COMPLETE Cons :: (,,,,) #-}

type instance Reverse (a, b, c, d, e) = (e, d, c, b, a)

instance HasReverse' (a, b, c, d, e) (e, d, c, b, a) where
  reverse' (a, b, c, d, e) = (e, d, c, b, a)

instance HasReverse (a, b, c, d, e)

type instance (a, b, c, d, e) !! 0 = a

instance HasAt' (a, b, c, d, e) 0 a where
  (a, _, _, _, _) !!! _ = a

instance HasAt (a, b, c, d, e) 0

type instance (a, b, c, d, e) !! 1 = b

instance HasAt' (a, b, c, d, e) 1 b where
  (_, b, _, _, _) !!! _ = b

instance HasAt (a, b, c, d, e) 1

type instance (a, b, c, d, e) !! 2 = c

instance HasAt' (a, b, c, d, e) 2 c where
  (_, _, c, _, _) !!! _ = c

instance HasAt (a, b, c, d, e) 2

type instance (a, b, c, d, e) !! 3 = d

instance HasAt' (a, b, c, d, e) 3 d where
  (_, _, _, d, _) !!! _ = d

instance HasAt (a, b, c, d, e) 3

type instance (a, b, c, d, e) !! 4 = e

instance HasAt' (a, b, c, d, e) 4 e where
  (_, _, _, _, e) !!! _ = e

instance HasAt (a, b, c, d, e) 4

-- 6

type instance Cons a (b, c, d, e, f) = (a, b, c, d, e, f)
type instance Head (a, b, c, d, e, f) = a
type instance Tail (a, b, c, d, e, f) = (b, c, d, e, f)
type instance Init (a, b, c, d, e, f) = (a, b, c, d, e)
type instance Last (a, b, c, d, e, f) = f
type instance Length (a, b, c, d, e, f) = 6

instance HasHead' (a, b, c, d, e, f) a where
  head' (a, _, _, _, _, _) = a

instance HasTail' (a, b, c, d, e, f) (b, c, d, e, f) where
  tail' (_, b, c, d, e, f) = (b, c, d, e, f)

instance HasInit' (a, b, c, d, e, f) (a, b, c, d, e) where
  init' (a, b, c, d, e, _) = (a, b, c, d, e)

instance HasLast' (a, b, c, d, e, f) f where
  last' (_, _, _, _, _, f) = f

instance HasCons' (a, b, c, d, e, f) a (b, c, d, e, f) where
  cons' a (b, c, d, e, f) = (a, b, c, d, e, f)

instance HasUncons' (a, b, c, d, e, f) a (b, c, d, e, f) where
  uncons' (a, b, c, d, e, f) = (a, (b, c, d, e, f))

instance HasHead (a, b, c, d, e, f)

instance HasTail (a, b, c, d, e, f)

instance HasInit (a, b, c, d, e, f)

instance HasLast (a, b, c, d, e, f)

instance HasCons a (b, c, d, e, f)

instance HasUncons (a, b, c, d, e, f)

instance HasLength (a, b, c, d, e, f)

{-# COMPLETE Cons' :: (,,,,,) #-}
{-# COMPLETE Cons :: (,,,,,) #-}

type instance Reverse (a, b, c, d, e, f) = (f, e, d, c, b, a)

instance HasReverse' (a, b, c, d, e, f) (f, e, d, c, b, a) where
  reverse' (a, b, c, d, e, f) = (f, e, d, c, b, a)

instance HasReverse (a, b, c, d, e, f)

type instance (a, b, c, d, e, f) !! 0 = a

instance HasAt' (a, b, c, d, e, f) 0 a where
  (a, _, _, _, _, _) !!! _ = a

instance HasAt (a, b, c, d, e, f) 0

type instance (a, b, c, d, e, f) !! 1 = b

instance HasAt' (a, b, c, d, e, f) 1 b where
  (_, b, _, _, _, _) !!! _ = b

instance HasAt (a, b, c, d, e, f) 1

type instance (a, b, c, d, e, f) !! 2 = c

instance HasAt' (a, b, c, d, e, f) 2 c where
  (_, _, c, _, _, _) !!! _ = c

instance HasAt (a, b, c, d, e, f) 2

type instance (a, b, c, d, e, f) !! 3 = d

instance HasAt' (a, b, c, d, e, f) 3 d where
  (_, _, _, d, _, _) !!! _ = d

instance HasAt (a, b, c, d, e, f) 3

type instance (a, b, c, d, e, f) !! 4 = e

instance HasAt' (a, b, c, d, e, f) 4 e where
  (_, _, _, _, e, _) !!! _ = e

instance HasAt (a, b, c, d, e, f) 4

type instance (a, b, c, d, e, f) !! 5 = f

instance HasAt' (a, b, c, d, e, f) 5 f where
  (_, _, _, _, _, f) !!! _ = f

instance HasAt (a, b, c, d, e, f) 5

-- 7

type instance Cons a (b, c, d, e, f, g) = (a, b, c, d, e, f, g)
type instance Head (a, b, c, d, e, f, g) = a
type instance Tail (a, b, c, d, e, f, g) = (b, c, d, e, f, g)
type instance Init (a, b, c, d, e, f, g) = (a, b, c, d, e, f)
type instance Last (a, b, c, d, e, f, g) = g
type instance Length (a, b, c, d, e, f, g) = 7

instance HasHead' (a, b, c, d, e, f, g) a where
  head' (a, _, _, _, _, _, _) = a

instance HasTail' (a, b, c, d, e, f, g) (b, c, d, e, f, g) where
  tail' (_, b, c, d, e, f, g) = (b, c, d, e, f, g)

instance HasInit' (a, b, c, d, e, f, g) (a, b, c, d, e, f) where
  init' (a, b, c, d, e, f, _) = (a, b, c, d, e, f)

instance HasLast' (a, b, c, d, e, f, g) g where
  last' (_, _, _, _, _, _, g) = g

instance HasCons' (a, b, c, d, e, f, g) a (b, c, d, e, f, g) where
  cons' a (b, c, d, e, f, g) = (a, b, c, d, e, f, g)

instance HasUncons' (a, b, c, d, e, f, g) a (b, c, d, e, f, g) where
  uncons' (a, b, c, d, e, f, g) = (a, (b, c, d, e, f, g))

instance HasHead (a, b, c, d, e, f, g)

instance HasTail (a, b, c, d, e, f, g)

instance HasInit (a, b, c, d, e, f, g)

instance HasLast (a, b, c, d, e, f, g)

instance HasCons a (b, c, d, e, f, g)

instance HasUncons (a, b, c, d, e, f, g)

instance HasLength (a, b, c, d, e, f, g)

{-# COMPLETE Cons' :: (,,,,,,) #-}
{-# COMPLETE Cons :: (,,,,,,) #-}

type instance Reverse (a, b, c, d, e, f, g) = (g, f, e, d, c, b, a)

instance HasReverse' (a, b, c, d, e, f, g) (g, f, e, d, c, b, a) where
  reverse' (a, b, c, d, e, f, g) = (g, f, e, d, c, b, a)

instance HasReverse (a, b, c, d, e, f, g)

type instance (a, b, c, d, e, f, g) !! 0 = a

instance HasAt' (a, b, c, d, e, f, g) 0 a where
  (a, _, _, _, _, _, _) !!! _ = a

instance HasAt (a, b, c, d, e, f, g) 0

type instance (a, b, c, d, e, f, g) !! 1 = b

instance HasAt' (a, b, c, d, e, f, g) 1 b where
  (_, b, _, _, _, _, _) !!! _ = b

instance HasAt (a, b, c, d, e, f, g) 1

type instance (a, b, c, d, e, f, g) !! 2 = c

instance HasAt' (a, b, c, d, e, f, g) 2 c where
  (_, _, c, _, _, _, _) !!! _ = c

instance HasAt (a, b, c, d, e, f, g) 2

type instance (a, b, c, d, e, f, g) !! 3 = d

instance HasAt' (a, b, c, d, e, f, g) 3 d where
  (_, _, _, d, _, _, _) !!! _ = d

instance HasAt (a, b, c, d, e, f, g) 3

type instance (a, b, c, d, e, f, g) !! 4 = e

instance HasAt' (a, b, c, d, e, f, g) 4 e where
  (_, _, _, _, e, _, _) !!! _ = e

instance HasAt (a, b, c, d, e, f, g) 4

type instance (a, b, c, d, e, f, g) !! 5 = f

instance HasAt' (a, b, c, d, e, f, g) 5 f where
  (_, _, _, _, _, f, _) !!! _ = f

instance HasAt (a, b, c, d, e, f, g) 5

type instance (a, b, c, d, e, f, g) !! 6 = g

instance HasAt' (a, b, c, d, e, f, g) 6 g where
  (_, _, _, _, _, _, g) !!! _ = g

instance HasAt (a, b, c, d, e, f, g) 6

-- 8

type instance Cons a (b, c, d, e, f, g, h) = (a, b, c, d, e, f, g, h)
type instance Head (a, b, c, d, e, f, g, h) = a
type instance Tail (a, b, c, d, e, f, g, h) = (b, c, d, e, f, g, h)
type instance Init (a, b, c, d, e, f, g, h) = (a, b, c, d, e, f, g)
type instance Last (a, b, c, d, e, f, g, h) = h
type instance Length (a, b, c, d, e, f, g, h) = 8

instance HasHead' (a, b, c, d, e, f, g, h) a where
  head' (a, _, _, _, _, _, _, _) = a

instance HasTail' (a, b, c, d, e, f, g, h) (b, c, d, e, f, g, h) where
  tail' (_, b, c, d, e, f, g, h) = (b, c, d, e, f, g, h)

instance HasInit' (a, b, c, d, e, f, g, h) (a, b, c, d, e, f, g) where
  init' (a, b, c, d, e, f, g, _) = (a, b, c, d, e, f, g)

instance HasLast' (a, b, c, d, e, f, g, h) h where
  last' (_, _, _, _, _, _, _, h) = h

instance HasCons' (a, b, c, d, e, f, g, h) a (b, c, d, e, f, g, h) where
  cons' a (b, c, d, e, f, g, h) = (a, b, c, d, e, f, g, h)

instance HasUncons' (a, b, c, d, e, f, g, h) a (b, c, d, e, f, g, h) where
  uncons' (a, b, c, d, e, f, g, h) = (a, (b, c, d, e, f, g, h))

instance HasHead (a, b, c, d, e, f, g, h)

instance HasTail (a, b, c, d, e, f, g, h)

instance HasInit (a, b, c, d, e, f, g, h)

instance HasLast (a, b, c, d, e, f, g, h)

instance HasCons a (b, c, d, e, f, g, h)

instance HasUncons (a, b, c, d, e, f, g, h)

instance HasLength (a, b, c, d, e, f, g, h)

{-# COMPLETE Cons' :: (,,,,,,,) #-}
{-# COMPLETE Cons :: (,,,,,,,) #-}

type instance Reverse (a, b, c, d, e, f, g, h) = (h, g, f, e, d, c, b, a)

instance HasReverse' (a, b, c, d, e, f, g, h) (h, g, f, e, d, c, b, a) where
  reverse' (a, b, c, d, e, f, g, h) = (h, g, f, e, d, c, b, a)

instance HasReverse (a, b, c, d, e, f, g, h)

type instance (a, b, c, d, e, f, g, h) !! 0 = a

instance HasAt' (a, b, c, d, e, f, g, h) 0 a where
  (a, _, _, _, _, _, _, _) !!! _ = a

instance HasAt (a, b, c, d, e, f, g, h) 0

type instance (a, b, c, d, e, f, g, h) !! 1 = b

instance HasAt' (a, b, c, d, e, f, g, h) 1 b where
  (_, b, _, _, _, _, _, _) !!! _ = b

instance HasAt (a, b, c, d, e, f, g, h) 1

type instance (a, b, c, d, e, f, g, h) !! 2 = c

instance HasAt' (a, b, c, d, e, f, g, h) 2 c where
  (_, _, c, _, _, _, _, _) !!! _ = c

instance HasAt (a, b, c, d, e, f, g, h) 2

type instance (a, b, c, d, e, f, g, h) !! 3 = d

instance HasAt' (a, b, c, d, e, f, g, h) 3 d where
  (_, _, _, d, _, _, _, _) !!! _ = d

instance HasAt (a, b, c, d, e, f, g, h) 3

type instance (a, b, c, d, e, f, g, h) !! 4 = e

instance HasAt' (a, b, c, d, e, f, g, h) 4 e where
  (_, _, _, _, e, _, _, _) !!! _ = e

instance HasAt (a, b, c, d, e, f, g, h) 4

type instance (a, b, c, d, e, f, g, h) !! 5 = f

instance HasAt' (a, b, c, d, e, f, g, h) 5 f where
  (_, _, _, _, _, f, _, _) !!! _ = f

instance HasAt (a, b, c, d, e, f, g, h) 5

type instance (a, b, c, d, e, f, g, h) !! 6 = g

instance HasAt' (a, b, c, d, e, f, g, h) 6 g where
  (_, _, _, _, _, _, g, _) !!! _ = g

instance HasAt (a, b, c, d, e, f, g, h) 6

type instance (a, b, c, d, e, f, g, h) !! 7 = h

instance HasAt' (a, b, c, d, e, f, g, h) 7 h where
  (_, _, _, _, _, _, _, h) !!! _ = h

instance HasAt (a, b, c, d, e, f, g, h) 7

-- 9

type instance Cons a (b, c, d, e, f, g, h, i) = (a, b, c, d, e, f, g, h, i)
type instance Head (a, b, c, d, e, f, g, h, i) = a
type instance Tail (a, b, c, d, e, f, g, h, i) = (b, c, d, e, f, g, h, i)
type instance Init (a, b, c, d, e, f, g, h, i) = (a, b, c, d, e, f, g, h)
type instance Last (a, b, c, d, e, f, g, h, i) = i
type instance Length (a, b, c, d, e, f, g, h, i) = 9

instance HasHead' (a, b, c, d, e, f, g, h, i) a where
  head' (a, _, _, _, _, _, _, _, _) = a

instance HasTail' (a, b, c, d, e, f, g, h, i) (b, c, d, e, f, g, h, i) where
  tail' (_, b, c, d, e, f, g, h, i) = (b, c, d, e, f, g, h, i)

instance HasInit' (a, b, c, d, e, f, g, h, i) (a, b, c, d, e, f, g, h) where
  init' (a, b, c, d, e, f, g, h, _) = (a, b, c, d, e, f, g, h)

instance HasLast' (a, b, c, d, e, f, g, h, i) i where
  last' (_, _, _, _, _, _, _, _, i) = i

instance HasCons' (a, b, c, d, e, f, g, h, i) a (b, c, d, e, f, g, h, i) where
  cons' a (b, c, d, e, f, g, h, i) = (a, b, c, d, e, f, g, h, i)

instance HasUncons' (a, b, c, d, e, f, g, h, i) a (b, c, d, e, f, g, h, i) where
  uncons' (a, b, c, d, e, f, g, h, i) = (a, (b, c, d, e, f, g, h, i))

instance HasHead (a, b, c, d, e, f, g, h, i)

instance HasTail (a, b, c, d, e, f, g, h, i)

instance HasInit (a, b, c, d, e, f, g, h, i)

instance HasLast (a, b, c, d, e, f, g, h, i)

instance HasCons a (b, c, d, e, f, g, h, i)

instance HasUncons (a, b, c, d, e, f, g, h, i)

instance HasLength (a, b, c, d, e, f, g, h, i)

{-# COMPLETE Cons' :: (,,,,,,,,) #-}
{-# COMPLETE Cons :: (,,,,,,,,) #-}

type instance Reverse (a, b, c, d, e, f, g, h, i) = (i, h, g, f, e, d, c, b, a)

instance HasReverse' (a, b, c, d, e, f, g, h, i) (i, h, g, f, e, d, c, b, a) where
  reverse' (a, b, c, d, e, f, g, h, i) = (i, h, g, f, e, d, c, b, a)

instance HasReverse (a, b, c, d, e, f, g, h, i)

type instance (a, b, c, d, e, f, g, h, i) !! 0 = a

instance HasAt' (a, b, c, d, e, f, g, h, i) 0 a where
  (a, _, _, _, _, _, _, _, _) !!! _ = a

instance HasAt (a, b, c, d, e, f, g, h, i) 0

type instance (a, b, c, d, e, f, g, h, i) !! 1 = b

instance HasAt' (a, b, c, d, e, f, g, h, i) 1 b where
  (_, b, _, _, _, _, _, _, _) !!! _ = b

instance HasAt (a, b, c, d, e, f, g, h, i) 1

type instance (a, b, c, d, e, f, g, h, i) !! 2 = c

instance HasAt' (a, b, c, d, e, f, g, h, i) 2 c where
  (_, _, c, _, _, _, _, _, _) !!! _ = c

instance HasAt (a, b, c, d, e, f, g, h, i) 2

type instance (a, b, c, d, e, f, g, h, i) !! 3 = d

instance HasAt' (a, b, c, d, e, f, g, h, i) 3 d where
  (_, _, _, d, _, _, _, _, _) !!! _ = d

instance HasAt (a, b, c, d, e, f, g, h, i) 3

type instance (a, b, c, d, e, f, g, h, i) !! 4 = e

instance HasAt' (a, b, c, d, e, f, g, h, i) 4 e where
  (_, _, _, _, e, _, _, _, _) !!! _ = e

instance HasAt (a, b, c, d, e, f, g, h, i) 4

type instance (a, b, c, d, e, f, g, h, i) !! 5 = f

instance HasAt' (a, b, c, d, e, f, g, h, i) 5 f where
  (_, _, _, _, _, f, _, _, _) !!! _ = f

instance HasAt (a, b, c, d, e, f, g, h, i) 5

type instance (a, b, c, d, e, f, g, h, i) !! 6 = g

instance HasAt' (a, b, c, d, e, f, g, h, i) 6 g where
  (_, _, _, _, _, _, g, _, _) !!! _ = g

instance HasAt (a, b, c, d, e, f, g, h, i) 6

type instance (a, b, c, d, e, f, g, h, i) !! 7 = h

instance HasAt' (a, b, c, d, e, f, g, h, i) 7 h where
  (_, _, _, _, _, _, _, h, _) !!! _ = h

instance HasAt (a, b, c, d, e, f, g, h, i) 7

type instance (a, b, c, d, e, f, g, h, i) !! 8 = i

instance HasAt' (a, b, c, d, e, f, g, h, i) 8 i where
  (_, _, _, _, _, _, _, _, i) !!! _ = i

instance HasAt (a, b, c, d, e, f, g, h, i) 8

-- 10

type instance Cons a (b, c, d, e, f, g, h, i, j) = (a, b, c, d, e, f, g, h, i, j)
type instance Head (a, b, c, d, e, f, g, h, i, j) = a
type instance Tail (a, b, c, d, e, f, g, h, i, j) = (b, c, d, e, f, g, h, i, j)
type instance Init (a, b, c, d, e, f, g, h, i, j) = (a, b, c, d, e, f, g, h, i)
type instance Last (a, b, c, d, e, f, g, h, i, j) = j
type instance Length (a, b, c, d, e, f, g, h, i, j) = 10

instance HasHead' (a, b, c, d, e, f, g, h, i, j) a where
  head' (a, _, _, _, _, _, _, _, _, _) = a

instance HasTail' (a, b, c, d, e, f, g, h, i, j) (b, c, d, e, f, g, h, i, j) where
  tail' (_, b, c, d, e, f, g, h, i, j) = (b, c, d, e, f, g, h, i, j)

instance HasInit' (a, b, c, d, e, f, g, h, i, j) (a, b, c, d, e, f, g, h, i) where
  init' (a, b, c, d, e, f, g, h, i, _) = (a, b, c, d, e, f, g, h, i)

instance HasLast' (a, b, c, d, e, f, g, h, i, j) j where
  last' (_, _, _, _, _, _, _, _, _, j) = j

instance HasCons' (a, b, c, d, e, f, g, h, i, j) a (b, c, d, e, f, g, h, i, j) where
  cons' a (b, c, d, e, f, g, h, i, j) = (a, b, c, d, e, f, g, h, i, j)

instance HasUncons' (a, b, c, d, e, f, g, h, i, j) a (b, c, d, e, f, g, h, i, j) where
  uncons' (a, b, c, d, e, f, g, h, i, j) = (a, (b, c, d, e, f, g, h, i, j))

instance HasHead (a, b, c, d, e, f, g, h, i, j)

instance HasTail (a, b, c, d, e, f, g, h, i, j)

instance HasInit (a, b, c, d, e, f, g, h, i, j)

instance HasLast (a, b, c, d, e, f, g, h, i, j)

instance HasCons a (b, c, d, e, f, g, h, i, j)

instance HasUncons (a, b, c, d, e, f, g, h, i, j)

instance HasLength (a, b, c, d, e, f, g, h, i, j)

{-# COMPLETE Cons' :: (,,,,,,,,,) #-}
{-# COMPLETE Cons :: (,,,,,,,,,) #-}

type instance Reverse (a, b, c, d, e, f, g, h, i, j) = (j, i, h, g, f, e, d, c, b, a)

instance HasReverse' (a, b, c, d, e, f, g, h, i, j) (j, i, h, g, f, e, d, c, b, a) where
  reverse' (a, b, c, d, e, f, g, h, i, j) = (j, i, h, g, f, e, d, c, b, a)

instance HasReverse (a, b, c, d, e, f, g, h, i, j)

type instance (a, b, c, d, e, f, g, h, i, j) !! 0 = a

instance HasAt' (a, b, c, d, e, f, g, h, i, j) 0 a where
  (a, _, _, _, _, _, _, _, _, _) !!! _ = a

instance HasAt (a, b, c, d, e, f, g, h, i, j) 0

type instance (a, b, c, d, e, f, g, h, i, j) !! 1 = b

instance HasAt' (a, b, c, d, e, f, g, h, i, j) 1 b where
  (_, b, _, _, _, _, _, _, _, _) !!! _ = b

instance HasAt (a, b, c, d, e, f, g, h, i, j) 1

type instance (a, b, c, d, e, f, g, h, i, j) !! 2 = c

instance HasAt' (a, b, c, d, e, f, g, h, i, j) 2 c where
  (_, _, c, _, _, _, _, _, _, _) !!! _ = c

instance HasAt (a, b, c, d, e, f, g, h, i, j) 2

type instance (a, b, c, d, e, f, g, h, i, j) !! 3 = d

instance HasAt' (a, b, c, d, e, f, g, h, i, j) 3 d where
  (_, _, _, d, _, _, _, _, _, _) !!! _ = d

instance HasAt (a, b, c, d, e, f, g, h, i, j) 3

type instance (a, b, c, d, e, f, g, h, i, j) !! 4 = e

instance HasAt' (a, b, c, d, e, f, g, h, i, j) 4 e where
  (_, _, _, _, e, _, _, _, _, _) !!! _ = e

instance HasAt (a, b, c, d, e, f, g, h, i, j) 4

type instance (a, b, c, d, e, f, g, h, i, j) !! 5 = f

instance HasAt' (a, b, c, d, e, f, g, h, i, j) 5 f where
  (_, _, _, _, _, f, _, _, _, _) !!! _ = f

instance HasAt (a, b, c, d, e, f, g, h, i, j) 5

type instance (a, b, c, d, e, f, g, h, i, j) !! 6 = g

instance HasAt' (a, b, c, d, e, f, g, h, i, j) 6 g where
  (_, _, _, _, _, _, g, _, _, _) !!! _ = g

instance HasAt (a, b, c, d, e, f, g, h, i, j) 6

type instance (a, b, c, d, e, f, g, h, i, j) !! 7 = h

instance HasAt' (a, b, c, d, e, f, g, h, i, j) 7 h where
  (_, _, _, _, _, _, _, h, _, _) !!! _ = h

instance HasAt (a, b, c, d, e, f, g, h, i, j) 7

type instance (a, b, c, d, e, f, g, h, i, j) !! 8 = i

instance HasAt' (a, b, c, d, e, f, g, h, i, j) 8 i where
  (_, _, _, _, _, _, _, _, i, _) !!! _ = i

instance HasAt (a, b, c, d, e, f, g, h, i, j) 8

type instance (a, b, c, d, e, f, g, h, i, j) !! 9 = j

instance HasAt' (a, b, c, d, e, f, g, h, i, j) 9 j where
  (_, _, _, _, _, _, _, _, _, j) !!! _ = j

instance HasAt (a, b, c, d, e, f, g, h, i, j) 9

-- 11

type instance Cons a (b, c, d, e, f, g, h, i, j, k) = (a, b, c, d, e, f, g, h, i, j, k)
type instance Head (a, b, c, d, e, f, g, h, i, j, k) = a
type instance Tail (a, b, c, d, e, f, g, h, i, j, k) = (b, c, d, e, f, g, h, i, j, k)
type instance Init (a, b, c, d, e, f, g, h, i, j, k) = (a, b, c, d, e, f, g, h, i, j)
type instance Last (a, b, c, d, e, f, g, h, i, j, k) = k
type instance Length (a, b, c, d, e, f, g, h, i, j, k) = 11

instance HasHead' (a, b, c, d, e, f, g, h, i, j, k) a where
  head' (a, _, _, _, _, _, _, _, _, _, _) = a

instance HasTail' (a, b, c, d, e, f, g, h, i, j, k) (b, c, d, e, f, g, h, i, j, k) where
  tail' (_, b, c, d, e, f, g, h, i, j, k) = (b, c, d, e, f, g, h, i, j, k)

instance HasInit' (a, b, c, d, e, f, g, h, i, j, k) (a, b, c, d, e, f, g, h, i, j) where
  init' (a, b, c, d, e, f, g, h, i, j, _) = (a, b, c, d, e, f, g, h, i, j)

instance HasLast' (a, b, c, d, e, f, g, h, i, j, k) k where
  last' (_, _, _, _, _, _, _, _, _, _, k) = k

instance HasCons' (a, b, c, d, e, f, g, h, i, j, k) a (b, c, d, e, f, g, h, i, j, k) where
  cons' a (b, c, d, e, f, g, h, i, j, k) = (a, b, c, d, e, f, g, h, i, j, k)

instance HasUncons' (a, b, c, d, e, f, g, h, i, j, k) a (b, c, d, e, f, g, h, i, j, k) where
  uncons' (a, b, c, d, e, f, g, h, i, j, k) = (a, (b, c, d, e, f, g, h, i, j, k))

instance HasHead (a, b, c, d, e, f, g, h, i, j, k)

instance HasTail (a, b, c, d, e, f, g, h, i, j, k)

instance HasInit (a, b, c, d, e, f, g, h, i, j, k)

instance HasLast (a, b, c, d, e, f, g, h, i, j, k)

instance HasCons a (b, c, d, e, f, g, h, i, j, k)

instance HasUncons (a, b, c, d, e, f, g, h, i, j, k)

instance HasLength (a, b, c, d, e, f, g, h, i, j, k)

{-# COMPLETE Cons' :: (,,,,,,,,,,) #-}
{-# COMPLETE Cons :: (,,,,,,,,,,) #-}

type instance Reverse (a, b, c, d, e, f, g, h, i, j, k) = (k, j, i, h, g, f, e, d, c, b, a)

instance HasReverse' (a, b, c, d, e, f, g, h, i, j, k) (k, j, i, h, g, f, e, d, c, b, a) where
  reverse' (a, b, c, d, e, f, g, h, i, j, k) = (k, j, i, h, g, f, e, d, c, b, a)

instance HasReverse (a, b, c, d, e, f, g, h, i, j, k)

type instance (a, b, c, d, e, f, g, h, i, j, k) !! 0 = a

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k) 0 a where
  (a, _, _, _, _, _, _, _, _, _, _) !!! _ = a

instance HasAt (a, b, c, d, e, f, g, h, i, j, k) 0

type instance (a, b, c, d, e, f, g, h, i, j, k) !! 1 = b

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k) 1 b where
  (_, b, _, _, _, _, _, _, _, _, _) !!! _ = b

instance HasAt (a, b, c, d, e, f, g, h, i, j, k) 1

type instance (a, b, c, d, e, f, g, h, i, j, k) !! 2 = c

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k) 2 c where
  (_, _, c, _, _, _, _, _, _, _, _) !!! _ = c

instance HasAt (a, b, c, d, e, f, g, h, i, j, k) 2

type instance (a, b, c, d, e, f, g, h, i, j, k) !! 3 = d

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k) 3 d where
  (_, _, _, d, _, _, _, _, _, _, _) !!! _ = d

instance HasAt (a, b, c, d, e, f, g, h, i, j, k) 3

type instance (a, b, c, d, e, f, g, h, i, j, k) !! 4 = e

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k) 4 e where
  (_, _, _, _, e, _, _, _, _, _, _) !!! _ = e

instance HasAt (a, b, c, d, e, f, g, h, i, j, k) 4

type instance (a, b, c, d, e, f, g, h, i, j, k) !! 5 = f

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k) 5 f where
  (_, _, _, _, _, f, _, _, _, _, _) !!! _ = f

instance HasAt (a, b, c, d, e, f, g, h, i, j, k) 5

type instance (a, b, c, d, e, f, g, h, i, j, k) !! 6 = g

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k) 6 g where
  (_, _, _, _, _, _, g, _, _, _, _) !!! _ = g

instance HasAt (a, b, c, d, e, f, g, h, i, j, k) 6

type instance (a, b, c, d, e, f, g, h, i, j, k) !! 7 = h

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k) 7 h where
  (_, _, _, _, _, _, _, h, _, _, _) !!! _ = h

instance HasAt (a, b, c, d, e, f, g, h, i, j, k) 7

type instance (a, b, c, d, e, f, g, h, i, j, k) !! 8 = i

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k) 8 i where
  (_, _, _, _, _, _, _, _, i, _, _) !!! _ = i

instance HasAt (a, b, c, d, e, f, g, h, i, j, k) 8

type instance (a, b, c, d, e, f, g, h, i, j, k) !! 9 = j

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k) 9 j where
  (_, _, _, _, _, _, _, _, _, j, _) !!! _ = j

instance HasAt (a, b, c, d, e, f, g, h, i, j, k) 9

type instance (a, b, c, d, e, f, g, h, i, j, k) !! 10 = k

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k) 10 k where
  (_, _, _, _, _, _, _, _, _, _, k) !!! _ = k

instance HasAt (a, b, c, d, e, f, g, h, i, j, k) 10

-- 12

type instance Cons a (b, c, d, e, f, g, h, i, j, k, l) = (a, b, c, d, e, f, g, h, i, j, k, l)
type instance Head (a, b, c, d, e, f, g, h, i, j, k, l) = a
type instance Tail (a, b, c, d, e, f, g, h, i, j, k, l) = (b, c, d, e, f, g, h, i, j, k, l)
type instance Init (a, b, c, d, e, f, g, h, i, j, k, l) = (a, b, c, d, e, f, g, h, i, j, k)
type instance Last (a, b, c, d, e, f, g, h, i, j, k, l) = l
type instance Length (a, b, c, d, e, f, g, h, i, j, k, l) = 12

instance HasHead' (a, b, c, d, e, f, g, h, i, j, k, l) a where
  head' (a, _, _, _, _, _, _, _, _, _, _, _) = a

instance HasTail' (a, b, c, d, e, f, g, h, i, j, k, l) (b, c, d, e, f, g, h, i, j, k, l) where
  tail' (_, b, c, d, e, f, g, h, i, j, k, l) = (b, c, d, e, f, g, h, i, j, k, l)

instance HasInit' (a, b, c, d, e, f, g, h, i, j, k, l) (a, b, c, d, e, f, g, h, i, j, k) where
  init' (a, b, c, d, e, f, g, h, i, j, k, _) = (a, b, c, d, e, f, g, h, i, j, k)

instance HasLast' (a, b, c, d, e, f, g, h, i, j, k, l) l where
  last' (_, _, _, _, _, _, _, _, _, _, _, l) = l

instance HasCons' (a, b, c, d, e, f, g, h, i, j, k, l) a (b, c, d, e, f, g, h, i, j, k, l) where
  cons' a (b, c, d, e, f, g, h, i, j, k, l) = (a, b, c, d, e, f, g, h, i, j, k, l)

instance HasUncons' (a, b, c, d, e, f, g, h, i, j, k, l) a (b, c, d, e, f, g, h, i, j, k, l) where
  uncons' (a, b, c, d, e, f, g, h, i, j, k, l) = (a, (b, c, d, e, f, g, h, i, j, k, l))

instance HasHead (a, b, c, d, e, f, g, h, i, j, k, l)

instance HasTail (a, b, c, d, e, f, g, h, i, j, k, l)

instance HasInit (a, b, c, d, e, f, g, h, i, j, k, l)

instance HasLast (a, b, c, d, e, f, g, h, i, j, k, l)

instance HasCons a (b, c, d, e, f, g, h, i, j, k, l)

instance HasUncons (a, b, c, d, e, f, g, h, i, j, k, l)

instance HasLength (a, b, c, d, e, f, g, h, i, j, k, l)

{-# COMPLETE Cons' :: (,,,,,,,,,,,) #-}
{-# COMPLETE Cons :: (,,,,,,,,,,,) #-}

type instance Reverse (a, b, c, d, e, f, g, h, i, j, k, l) = (l, k, j, i, h, g, f, e, d, c, b, a)

instance HasReverse' (a, b, c, d, e, f, g, h, i, j, k, l) (l, k, j, i, h, g, f, e, d, c, b, a) where
  reverse' (a, b, c, d, e, f, g, h, i, j, k, l) = (l, k, j, i, h, g, f, e, d, c, b, a)

instance HasReverse (a, b, c, d, e, f, g, h, i, j, k, l)

type instance (a, b, c, d, e, f, g, h, i, j, k, l) !! 0 = a

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l) 0 a where
  (a, _, _, _, _, _, _, _, _, _, _, _) !!! _ = a

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l) 0

type instance (a, b, c, d, e, f, g, h, i, j, k, l) !! 1 = b

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l) 1 b where
  (_, b, _, _, _, _, _, _, _, _, _, _) !!! _ = b

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l) 1

type instance (a, b, c, d, e, f, g, h, i, j, k, l) !! 2 = c

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l) 2 c where
  (_, _, c, _, _, _, _, _, _, _, _, _) !!! _ = c

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l) 2

type instance (a, b, c, d, e, f, g, h, i, j, k, l) !! 3 = d

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l) 3 d where
  (_, _, _, d, _, _, _, _, _, _, _, _) !!! _ = d

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l) 3

type instance (a, b, c, d, e, f, g, h, i, j, k, l) !! 4 = e

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l) 4 e where
  (_, _, _, _, e, _, _, _, _, _, _, _) !!! _ = e

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l) 4

type instance (a, b, c, d, e, f, g, h, i, j, k, l) !! 5 = f

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l) 5 f where
  (_, _, _, _, _, f, _, _, _, _, _, _) !!! _ = f

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l) 5

type instance (a, b, c, d, e, f, g, h, i, j, k, l) !! 6 = g

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l) 6 g where
  (_, _, _, _, _, _, g, _, _, _, _, _) !!! _ = g

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l) 6

type instance (a, b, c, d, e, f, g, h, i, j, k, l) !! 7 = h

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l) 7 h where
  (_, _, _, _, _, _, _, h, _, _, _, _) !!! _ = h

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l) 7

type instance (a, b, c, d, e, f, g, h, i, j, k, l) !! 8 = i

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l) 8 i where
  (_, _, _, _, _, _, _, _, i, _, _, _) !!! _ = i

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l) 8

type instance (a, b, c, d, e, f, g, h, i, j, k, l) !! 9 = j

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l) 9 j where
  (_, _, _, _, _, _, _, _, _, j, _, _) !!! _ = j

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l) 9

type instance (a, b, c, d, e, f, g, h, i, j, k, l) !! 10 = k

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l) 10 k where
  (_, _, _, _, _, _, _, _, _, _, k, _) !!! _ = k

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l) 10

type instance (a, b, c, d, e, f, g, h, i, j, k, l) !! 11 = l

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l) 11 l where
  (_, _, _, _, _, _, _, _, _, _, _, l) !!! _ = l

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l) 11

-- 13

type instance Cons a (b, c, d, e, f, g, h, i, j, k, l, m) = (a, b, c, d, e, f, g, h, i, j, k, l, m)
type instance Head (a, b, c, d, e, f, g, h, i, j, k, l, m) = a
type instance Tail (a, b, c, d, e, f, g, h, i, j, k, l, m) = (b, c, d, e, f, g, h, i, j, k, l, m)
type instance Init (a, b, c, d, e, f, g, h, i, j, k, l, m) = (a, b, c, d, e, f, g, h, i, j, k, l)
type instance Last (a, b, c, d, e, f, g, h, i, j, k, l, m) = m
type instance Length (a, b, c, d, e, f, g, h, i, j, k, l, m) = 13

instance HasHead' (a, b, c, d, e, f, g, h, i, j, k, l, m) a where
  head' (a, _, _, _, _, _, _, _, _, _, _, _, _) = a

instance HasTail' (a, b, c, d, e, f, g, h, i, j, k, l, m) (b, c, d, e, f, g, h, i, j, k, l, m) where
  tail' (_, b, c, d, e, f, g, h, i, j, k, l, m) = (b, c, d, e, f, g, h, i, j, k, l, m)

instance HasInit' (a, b, c, d, e, f, g, h, i, j, k, l, m) (a, b, c, d, e, f, g, h, i, j, k, l) where
  init' (a, b, c, d, e, f, g, h, i, j, k, l, _) = (a, b, c, d, e, f, g, h, i, j, k, l)

instance HasLast' (a, b, c, d, e, f, g, h, i, j, k, l, m) m where
  last' (_, _, _, _, _, _, _, _, _, _, _, _, m) = m

instance HasCons' (a, b, c, d, e, f, g, h, i, j, k, l, m) a (b, c, d, e, f, g, h, i, j, k, l, m) where
  cons' a (b, c, d, e, f, g, h, i, j, k, l, m) = (a, b, c, d, e, f, g, h, i, j, k, l, m)

instance HasUncons' (a, b, c, d, e, f, g, h, i, j, k, l, m) a (b, c, d, e, f, g, h, i, j, k, l, m) where
  uncons' (a, b, c, d, e, f, g, h, i, j, k, l, m) = (a, (b, c, d, e, f, g, h, i, j, k, l, m))

instance HasHead (a, b, c, d, e, f, g, h, i, j, k, l, m)

instance HasTail (a, b, c, d, e, f, g, h, i, j, k, l, m)

instance HasInit (a, b, c, d, e, f, g, h, i, j, k, l, m)

instance HasLast (a, b, c, d, e, f, g, h, i, j, k, l, m)

instance HasCons a (b, c, d, e, f, g, h, i, j, k, l, m)

instance HasUncons (a, b, c, d, e, f, g, h, i, j, k, l, m)

instance HasLength (a, b, c, d, e, f, g, h, i, j, k, l, m)

{-# COMPLETE Cons' :: (,,,,,,,,,,,,) #-}
{-# COMPLETE Cons :: (,,,,,,,,,,,,) #-}

type instance Reverse (a, b, c, d, e, f, g, h, i, j, k, l, m) = (m, l, k, j, i, h, g, f, e, d, c, b, a)

instance HasReverse' (a, b, c, d, e, f, g, h, i, j, k, l, m) (m, l, k, j, i, h, g, f, e, d, c, b, a) where
  reverse' (a, b, c, d, e, f, g, h, i, j, k, l, m) = (m, l, k, j, i, h, g, f, e, d, c, b, a)

instance HasReverse (a, b, c, d, e, f, g, h, i, j, k, l, m)

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m) !! 0 = a

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m) 0 a where
  (a, _, _, _, _, _, _, _, _, _, _, _, _) !!! _ = a

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m) 0

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m) !! 1 = b

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m) 1 b where
  (_, b, _, _, _, _, _, _, _, _, _, _, _) !!! _ = b

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m) 1

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m) !! 2 = c

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m) 2 c where
  (_, _, c, _, _, _, _, _, _, _, _, _, _) !!! _ = c

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m) 2

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m) !! 3 = d

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m) 3 d where
  (_, _, _, d, _, _, _, _, _, _, _, _, _) !!! _ = d

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m) 3

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m) !! 4 = e

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m) 4 e where
  (_, _, _, _, e, _, _, _, _, _, _, _, _) !!! _ = e

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m) 4

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m) !! 5 = f

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m) 5 f where
  (_, _, _, _, _, f, _, _, _, _, _, _, _) !!! _ = f

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m) 5

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m) !! 6 = g

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m) 6 g where
  (_, _, _, _, _, _, g, _, _, _, _, _, _) !!! _ = g

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m) 6

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m) !! 7 = h

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m) 7 h where
  (_, _, _, _, _, _, _, h, _, _, _, _, _) !!! _ = h

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m) 7

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m) !! 8 = i

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m) 8 i where
  (_, _, _, _, _, _, _, _, i, _, _, _, _) !!! _ = i

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m) 8

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m) !! 9 = j

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m) 9 j where
  (_, _, _, _, _, _, _, _, _, j, _, _, _) !!! _ = j

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m) 9

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m) !! 10 = k

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m) 10 k where
  (_, _, _, _, _, _, _, _, _, _, k, _, _) !!! _ = k

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m) 10

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m) !! 11 = l

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m) 11 l where
  (_, _, _, _, _, _, _, _, _, _, _, l, _) !!! _ = l

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m) 11

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m) !! 12 = m

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m) 12 m where
  (_, _, _, _, _, _, _, _, _, _, _, _, m) !!! _ = m

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m) 12

-- 14

type instance Cons a (b, c, d, e, f, g, h, i, j, k, l, m, n) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
type instance Head (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = a
type instance Tail (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (b, c, d, e, f, g, h, i, j, k, l, m, n)
type instance Init (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (a, b, c, d, e, f, g, h, i, j, k, l, m)
type instance Last (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = n
type instance Length (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = 14

instance HasHead' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) a where
  head' (a, _, _, _, _, _, _, _, _, _, _, _, _, _) = a

instance HasTail' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) (b, c, d, e, f, g, h, i, j, k, l, m, n) where
  tail' (_, b, c, d, e, f, g, h, i, j, k, l, m, n) = (b, c, d, e, f, g, h, i, j, k, l, m, n)

instance HasInit' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  init' (a, b, c, d, e, f, g, h, i, j, k, l, m, _) = (a, b, c, d, e, f, g, h, i, j, k, l, m)

instance HasLast' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) n where
  last' (_, _, _, _, _, _, _, _, _, _, _, _, _, n) = n

instance HasCons' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) a (b, c, d, e, f, g, h, i, j, k, l, m, n) where
  cons' a (b, c, d, e, f, g, h, i, j, k, l, m, n) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

instance HasUncons' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) a (b, c, d, e, f, g, h, i, j, k, l, m, n) where
  uncons' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n))

instance HasHead (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

instance HasTail (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

instance HasInit (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

instance HasLast (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

instance HasCons a (b, c, d, e, f, g, h, i, j, k, l, m, n)

instance HasUncons (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

instance HasLength (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

{-# COMPLETE Cons' :: (,,,,,,,,,,,,,) #-}
{-# COMPLETE Cons :: (,,,,,,,,,,,,,) #-}

type instance Reverse (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (n, m, l, k, j, i, h, g, f, e, d, c, b, a)

instance HasReverse' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) (n, m, l, k, j, i, h, g, f, e, d, c, b, a) where
  reverse' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (n, m, l, k, j, i, h, g, f, e, d, c, b, a)

instance HasReverse (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n) !! 0 = a

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 0 a where
  (a, _, _, _, _, _, _, _, _, _, _, _, _, _) !!! _ = a

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 0

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n) !! 1 = b

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 1 b where
  (_, b, _, _, _, _, _, _, _, _, _, _, _, _) !!! _ = b

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 1

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n) !! 2 = c

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 2 c where
  (_, _, c, _, _, _, _, _, _, _, _, _, _, _) !!! _ = c

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 2

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n) !! 3 = d

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 3 d where
  (_, _, _, d, _, _, _, _, _, _, _, _, _, _) !!! _ = d

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 3

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n) !! 4 = e

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 4 e where
  (_, _, _, _, e, _, _, _, _, _, _, _, _, _) !!! _ = e

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 4

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n) !! 5 = f

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 5 f where
  (_, _, _, _, _, f, _, _, _, _, _, _, _, _) !!! _ = f

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 5

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n) !! 6 = g

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 6 g where
  (_, _, _, _, _, _, g, _, _, _, _, _, _, _) !!! _ = g

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 6

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n) !! 7 = h

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 7 h where
  (_, _, _, _, _, _, _, h, _, _, _, _, _, _) !!! _ = h

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 7

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n) !! 8 = i

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 8 i where
  (_, _, _, _, _, _, _, _, i, _, _, _, _, _) !!! _ = i

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 8

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n) !! 9 = j

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 9 j where
  (_, _, _, _, _, _, _, _, _, j, _, _, _, _) !!! _ = j

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 9

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n) !! 10 = k

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 10 k where
  (_, _, _, _, _, _, _, _, _, _, k, _, _, _) !!! _ = k

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 10

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n) !! 11 = l

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 11 l where
  (_, _, _, _, _, _, _, _, _, _, _, l, _, _) !!! _ = l

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 11

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n) !! 12 = m

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 12 m where
  (_, _, _, _, _, _, _, _, _, _, _, _, m, _) !!! _ = m

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 12

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n) !! 13 = n

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 13 n where
  (_, _, _, _, _, _, _, _, _, _, _, _, _, n) !!! _ = n

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n) 13

-- 15

type instance Cons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
type instance Head (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = a
type instance Tail (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o)
type instance Init (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
type instance Last (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = o
type instance Length (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = 15

instance HasHead' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) a where
  head' (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _) = a

instance HasTail' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) (b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  tail' (_, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance HasInit' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  init' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, _) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

instance HasLast' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) o where
  last' (_, _, _, _, _, _, _, _, _, _, _, _, _, _, o) = o

instance HasCons' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) a (b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  cons' a (b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance HasUncons' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) a (b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  uncons' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o))

instance HasHead (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance HasTail (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance HasInit (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance HasLast (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance HasCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance HasUncons (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance HasLength (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

{-# COMPLETE Cons' :: (,,,,,,,,,,,,,,) #-}
{-# COMPLETE Cons :: (,,,,,,,,,,,,,,) #-}

type instance Reverse (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (o, n, m, l, k, j, i, h, g, f, e, d, c, b, a)

instance HasReverse' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) (o, n, m, l, k, j, i, h, g, f, e, d, c, b, a) where
  reverse' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (o, n, m, l, k, j, i, h, g, f, e, d, c, b, a)

instance HasReverse (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 0 = a

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 0 a where
  (a, _, _, _, _, _, _, _, _, _, _, _, _, _, _) !!! _ = a

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 0

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 1 = b

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 1 b where
  (_, b, _, _, _, _, _, _, _, _, _, _, _, _, _) !!! _ = b

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 1

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 2 = c

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 2 c where
  (_, _, c, _, _, _, _, _, _, _, _, _, _, _, _) !!! _ = c

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 2

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 3 = d

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 3 d where
  (_, _, _, d, _, _, _, _, _, _, _, _, _, _, _) !!! _ = d

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 3

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 4 = e

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 4 e where
  (_, _, _, _, e, _, _, _, _, _, _, _, _, _, _) !!! _ = e

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 4

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 5 = f

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 5 f where
  (_, _, _, _, _, f, _, _, _, _, _, _, _, _, _) !!! _ = f

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 5

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 6 = g

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 6 g where
  (_, _, _, _, _, _, g, _, _, _, _, _, _, _, _) !!! _ = g

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 6

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 7 = h

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 7 h where
  (_, _, _, _, _, _, _, h, _, _, _, _, _, _, _) !!! _ = h

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 7

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 8 = i

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 8 i where
  (_, _, _, _, _, _, _, _, i, _, _, _, _, _, _) !!! _ = i

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 8

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 9 = j

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 9 j where
  (_, _, _, _, _, _, _, _, _, j, _, _, _, _, _) !!! _ = j

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 9

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 10 = k

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 10 k where
  (_, _, _, _, _, _, _, _, _, _, k, _, _, _, _) !!! _ = k

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 10

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 11 = l

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 11 l where
  (_, _, _, _, _, _, _, _, _, _, _, l, _, _, _) !!! _ = l

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 11

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 12 = m

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 12 m where
  (_, _, _, _, _, _, _, _, _, _, _, _, m, _, _) !!! _ = m

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 12

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 13 = n

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 13 n where
  (_, _, _, _, _, _, _, _, _, _, _, _, _, n, _) !!! _ = n

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 13

type instance (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) !! 14 = o

instance HasAt' (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 14 o where
  (_, _, _, _, _, _, _, _, _, _, _, _, _, _, o) !!! _ = o

instance HasAt (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) 14
