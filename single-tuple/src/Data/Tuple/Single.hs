{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe            #-}
{-# LANGUAGE ViewPatterns    #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Copyright   :  Kazuki Okamoto
-- License     :  see LICENSE
-- Maintainer  :  kazuki.okamoto@kakkun61.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- A class for 1-tuples.

module Data.Tuple.Single
  ( Single (..)
  , pattern Single
  ) where

import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Tuple.Only       (Only (Only, fromOnly))

#if MIN_VERSION_OneTuple(0,3,0)
#if !MIN_VERSION_base(4,15,0)
import qualified Data.Tuple.Solo as OneTuple
#endif
#else
import Data.Tuple.OneTuple (OneTuple (OneTuple), only)
#endif

#if MIN_VERSION_ghc_prim(0,10,0)
import GHC.Tuple (Solo (MkSolo))
#elif MIN_VERSION_ghc_prim(0,7,0)
import GHC.Tuple (Solo (Solo))
#else
import GHC.Tuple (Unit (Unit))
#endif

class Single t where
  wrap :: a -> t a
  unwrap :: t a -> a

pattern Single :: Single t => a -> t a
pattern Single a <- (unwrap -> a) where
  Single a = wrap a

instance Single Identity where
  wrap = Identity
  unwrap = runIdentity

{-# COMPLETE Single :: Identity #-}

instance Single Only where
  wrap = Only
  unwrap = fromOnly

{-# COMPLETE Single :: Only #-}

#if MIN_VERSION_OneTuple(0,3,0)
#if !MIN_VERSION_base(4,15,0)
instance Single OneTuple.Solo where
  wrap = OneTuple.Solo
  unwrap = OneTuple.getSolo

{-# COMPLETE Single :: OneTuple.Solo #-}
#endif
#else
instance Single OneTuple where
  wrap = OneTuple
  unwrap = only

{-# COMPLETE Single :: OneTuple #-}
#endif

#if MIN_VERSION_ghc_prim(0,10,0)
instance Single Solo where
  wrap = MkSolo
  unwrap (MkSolo a) = a

{-# COMPLETE Single :: Solo #-}
#elif MIN_VERSION_ghc_prim(0,7,0)
instance Single Solo where
  wrap = Solo
  unwrap (Solo a) = a

{-# COMPLETE Single :: Solo #-}
#else
instance Single Unit where
  wrap = Unit
  unwrap (Unit a) = a

{-# COMPLETE Single :: Unit #-}
#endif
