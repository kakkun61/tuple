{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE Trustworthy           #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Copyright   :  Kazuki Okamoto
-- License     :  see LICENSE
-- Maintainer  :  kazuki.okamoto@kakkun61.com
-- Stability   :  experimental
-- Portability :  GHC

module Data.Tuple.List.TH (list) where

import Data.Tuple.List.Data (type (!!), Cons, HasAt, HasAt' ((!!!)), HasCons, HasCons' (cons'), HasHead,
                             HasHead' (head'), HasInit, HasInit' (init'), HasLast, HasLast' (last'), HasLength,
                             HasReverse, HasReverse' (reverse'), HasTail, HasTail' (tail'), HasUncons,
                             HasUncons' (uncons'), Head, Init, Last, Length, Reverse, Tail)

import Language.Haskell.TH (Dec, Exp (TupE, VarE), Name, Pat (TupP, VarP, WildP), Q, TyLit (NumTyLit),
                            Type (AppT, LitT, TupleT, VarT), newName)

import Data.Traversable (for)

list :: Word -> Q [Dec]
list n =
  mconcat <$> sequence (($ n) <$> decs)
  where
    decs =
      [ consType
      , headType
      , tailType
      , initType
      , lastType
      , lengthType
      , hasHead'Instance
      , hasTail'Instance
      , hasInit'Instance
      , hasLast'Instance
      , hasCons'Instance
      , hasUncons'Instance
      , hasHeadInstance
      , hasTailInstance
      , hasInitInstance
      , hasLastInstance
      , hasConsInstance
      , hasUnconsInstance
      , hasLengthInstance
      , reverseType
      , hasReverse'Instance
      , hasReverseInstance
      , atTypes
      , hasAt'Instances
      , hasAtInstances
      ]

consType :: Word -> Q [Dec]
consType n = do
  abc@(a:bcd) <- newNames
  [d| type instance Cons $(pure $ VarT a) $(pure $ tuple (n - 1) bcd) = $(pure $ tuple n abc) |]

headType :: Word -> Q [Dec]
headType n = do
  abc@(a:_) <- newNames
  [d| type instance Head $(pure $ tuple n abc) = $(pure $ VarT a) |]

tailType :: Word -> Q [Dec]
tailType n = do
  abc@(_:bcd) <- newNames
  [d| type instance Tail $(pure $ tuple n abc) = $(pure $ tuple (n - 1) bcd) |]

initType :: Word -> Q [Dec]
initType n = do
  abc <- newNames
  [d| type instance Init $(pure $ tuple n abc) = $(pure $ tuple (n - 1) abc) |]

lastType :: Word -> Q [Dec]
lastType n = do
  abc <- newNames
  [d| type instance Last $(pure $ tuple n abc) = $(pure $ VarT $ abc Prelude.!! (fromIntegral n - 1)) |]

lengthType :: Word -> Q [Dec]
lengthType n = do
  abc <- newNames
  [d| type instance Length $(pure $ tuple n abc) = $(pure $ LitT $ NumTyLit $ fromIntegral n) |]

hasHead'Instance :: Word -> Q [Dec]
hasHead'Instance n = do
  abc@(a:_) <- newNames
  [d| instance HasHead' $(pure $ tuple n abc) $(pure $ VarT a) where
        head' $(pure $ TupP $ VarP a : replicate (fromIntegral n - 1) WildP) = $(pure $ VarE a)
   |]

hasTail'Instance :: Word -> Q [Dec]
hasTail'Instance n = do
  abc@(_:bcd) <- newNames
  let bcd' = take (fromIntegral n - 1) bcd
  [d| instance HasTail' $(pure $ tuple n abc) $(pure $ tuple (n - 1) bcd) where
        tail' $(pure $ TupP $ WildP : (VarP <$> bcd')) = $(pure $ TupE $ VarE <$> bcd')
   |]

hasInit'Instance :: Word -> Q [Dec]
hasInit'Instance n = do
  abc <- newNames
  let abc' = take (fromIntegral n - 1) abc
  [d| instance HasInit' $(pure $ tuple n abc) $(pure $ tuple (n - 1) abc) where
        init' $(pure $ TupP $ (VarP <$> abc') ++ [WildP]) = $(pure $ TupE $ VarE <$> abc')
   |]

hasLast'Instance :: Word -> Q [Dec]
hasLast'Instance n = do
  abc <- newNames
  let c = abc Prelude.!! (fromIntegral n - 1)
  [d| instance HasLast' $(pure $ tuple n abc) $(pure $ VarT c) where
        last' $(pure $ TupP $ replicate (fromIntegral n - 1) WildP ++ [VarP c]) = $(pure $ VarE c)
   |]

hasCons'Instance :: Word -> Q [Dec]
hasCons'Instance n = do
  abc@(a:bcd) <- newNames
  let
    abc' = take (fromIntegral n) abc
    bcd' = take (fromIntegral n - 1) bcd
  [d| instance HasCons' $(pure $ tuple n abc) $(pure $ VarT a) $(pure $ tuple (n - 1) bcd) where
        cons' $(pure $ VarP a) $(pure $ TupP $ VarP <$> bcd') = $(pure $ TupE $ VarE <$> abc')
   |]

hasUncons'Instance :: Word -> Q [Dec]
hasUncons'Instance n = do
  abc@(a:bcd) <- newNames
  let
    abc' = take (fromIntegral n) abc
    bcd' = take (fromIntegral n - 1) bcd
  [d| instance HasUncons' $(pure $ tuple n abc) $(pure $ VarT a) $(pure $ tuple (n - 1) bcd) where
        uncons' $(pure $ TupP $ VarP <$> abc') = ($(pure $ VarE a), $(pure $ TupE $ VarE <$> bcd'))
   |]

hasHeadInstance :: Word -> Q [Dec]
hasHeadInstance n = do
  abc <- newNames
  [d| instance HasHead $(pure $ tuple n abc) |]

hasTailInstance :: Word -> Q [Dec]
hasTailInstance n = do
  abc <- newNames
  [d| instance HasTail $(pure $ tuple n abc) |]

hasInitInstance :: Word -> Q [Dec]
hasInitInstance n = do
  abc <- newNames
  [d| instance HasInit $(pure $ tuple n abc) |]

hasLastInstance :: Word -> Q [Dec]
hasLastInstance n = do
  abc <- newNames
  [d| instance HasLast $(pure $ tuple n abc) |]

hasConsInstance :: Word -> Q [Dec]
hasConsInstance n = do
  a:bcd <- newNames
  [d| instance HasCons $(pure $ VarT a) $(pure $ tuple (n - 1) bcd) |]

hasUnconsInstance :: Word -> Q [Dec]
hasUnconsInstance n = do
  abc <- newNames
  [d| instance HasUncons $(pure $ tuple n abc) |]

hasLengthInstance :: Word -> Q [Dec]
hasLengthInstance n = do
  abc <- newNames
  [d| instance HasLength $(pure $ tuple n abc) |]

reverseType :: Word -> Q [Dec]
reverseType n = do
  abc <- newNames
  let
    abc' = take (fromIntegral n) abc
    cba' = Prelude.reverse abc'
  [d| type instance Reverse $(pure $ tuple n abc) = $(pure $ tuple n cba') |]

hasReverse'Instance :: Word -> Q [Dec]
hasReverse'Instance n = do
  abc <- newNames
  let
    abc' = take (fromIntegral n) abc
    cba' = Prelude.reverse abc'
  [d| instance HasReverse' $(pure $ tuple n abc) $(pure $ tuple n cba') where
        reverse' $(pure $ TupP $ VarP <$> abc') = $(pure $ TupE $ VarE <$> cba')
   |]

hasReverseInstance :: Word -> Q [Dec]
hasReverseInstance n = do
  abc <- newNames
  [d| instance HasReverse $(pure $ tuple n abc) |]

atTypes :: Word -> Q [Dec]
atTypes n =
  mconcat <$> for [0 .. n - 1] atType
  where
    atType :: Word -> Q [Dec]
    atType i = do
      abc <- newNames
      let
        ii = fromIntegral i :: Int
        iii = fromIntegral i :: Integer
      [d| type instance $(pure $ tuple n abc) !! $(pure $ LitT $ NumTyLit iii) = $(pure $ VarT $ abc Prelude.!! ii) |]

hasAt'Instances :: Word -> Q [Dec]
hasAt'Instances n =
  mconcat <$> for [0 .. n - 1] hasAt'Instance
  where
    hasAt'Instance :: Word -> Q [Dec]
    hasAt'Instance i = do
      abc <- newNames
      let
        iii = fromIntegral i :: Integer
        x = abc Prelude.!! (fromIntegral i)
      [d| instance HasAt' $(pure $ tuple n abc) $(pure $ LitT $ NumTyLit iii) $(pure $ VarT x) where
            $(pure $ TupP $ overwrite i (VarP x) $ replicate (fromIntegral n) WildP) !!! $(pure $ WildP) = $(pure $ VarE x)
       |]
    overwrite :: Word -> a -> [a] -> [a]
    overwrite 0 a (_:xs) = a : xs
    overwrite i a (x:xs) = x : overwrite (i - 1) a xs
    overwrite _ _ []     = error "hasAt'Instances.overwrite"

hasAtInstances :: Word -> Q [Dec]
hasAtInstances n =
  mconcat <$> for [0 .. n - 1] hasAtInstance
  where
    hasAtInstance :: Word -> Q [Dec]
    hasAtInstance i = do
      abc <- newNames
      [d| instance HasAt $(pure $ tuple n abc) $(pure $ LitT $ NumTyLit $ fromIntegral i) |]

newNames :: Q [Name]
newNames = for ['a' ..] $ newName . (:[])

tuple :: Word -> [Name] -> Type
tuple n names = fst $ app n (TupleT (fromIntegral n)) names
  where
    app :: Word -> Type -> [Name] -> (Type, [Name])
    app 0 f ns = (f, ns)
    app m f ns =
      let (f', ns') = app (m - 1) f ns
      in (AppT f' (VarT $ Prelude.head ns'), Prelude.tail ns')
