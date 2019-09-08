{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Safe                  #-}

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

import Language.Haskell.TH (Body (NormalB), Clause (Clause), Dec (FunD, InstanceD, TySynInstD), Exp (TupE, VarE), Name,
                            Pat (TupP, VarP, WildP), Q, TyLit (NumTyLit), TySynEqn (TySynEqn),
                            Type (AppT, ConT, LitT, TupleT, VarT), newName)

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
  pure
    [ TySynInstD
       ''Cons
       $ TySynEqn
           [ VarT a
           , tuple (n - 1) bcd
           ]
           (tuple n abc)
    ]

headType :: Word -> Q [Dec]
headType n = do
  abc@(a:_) <- newNames
  pure
    [ TySynInstD
        ''Head
        $ TySynEqn
            [tuple n abc]
            (VarT a)
    ]

tailType :: Word -> Q [Dec]
tailType n = do
  abc@(_:bcd) <- newNames
  pure
    [ TySynInstD
        ''Tail
        $ TySynEqn
            [tuple n abc]
            (tuple (n - 1) bcd)
    ]

initType :: Word -> Q [Dec]
initType n = do
  abc <- newNames
  pure
    [ TySynInstD
        ''Init
        $ TySynEqn
            [tuple n abc]
            (tuple (n - 1) abc)
    ]

lastType :: Word -> Q [Dec]
lastType n = do
  abc <- newNames
  pure
    [ TySynInstD
        ''Last
        $ TySynEqn
            [tuple n abc]
            (VarT $ abc Prelude.!! (fromIntegral n - 1))
    ]

lengthType :: Word -> Q [Dec]
lengthType n = do
  abc <- newNames
  pure
    [ TySynInstD
        ''Length
        $ TySynEqn
            [tuple n abc]
            (LitT $ NumTyLit $ fromIntegral n)
    ]

hasHead'Instance :: Word -> Q [Dec]
hasHead'Instance n = do
  abc@(a:_) <- newNames
  pure
    [ InstanceD
        Nothing
        []
        (AppT (AppT (ConT ''HasHead') (tuple n abc)) (VarT a))
        [FunD 'head' [Clause [TupP $ VarP a : replicate (fromIntegral n - 1) WildP] (NormalB $ VarE a) []]]
    ]

hasTail'Instance :: Word -> Q [Dec]
hasTail'Instance n = do
  abc@(_:bcd) <- newNames
  let bcd' = take (fromIntegral n - 1) bcd
  pure
    [ InstanceD
        Nothing
        []
        (AppT (AppT (ConT ''HasTail') (tuple n abc)) (tuple (n - 1) bcd))
        [FunD 'tail' [Clause [TupP $ WildP : (VarP <$> bcd')] (NormalB $ TupE $ VarE <$> bcd') []]]
    ]

hasInit'Instance :: Word -> Q [Dec]
hasInit'Instance n = do
  abc <- newNames
  let abc' = take (fromIntegral n - 1) abc
  pure
    [ InstanceD
        Nothing
        []
        (AppT (AppT (ConT ''HasInit') (tuple n abc)) (tuple (n - 1) abc))
        [FunD 'init' [Clause [TupP $ (VarP <$> abc') ++ [WildP]] (NormalB $ TupE $ VarE <$> abc') []]]
    ]

hasLast'Instance :: Word -> Q [Dec]
hasLast'Instance n = do
  abc <- newNames
  let c = abc Prelude.!! (fromIntegral n - 1)
  pure
    [ InstanceD
        Nothing
        []
        (AppT (AppT (ConT ''HasLast') (tuple n abc)) (VarT c))
        [FunD 'last' [Clause [TupP $ replicate (fromIntegral n - 1) WildP ++ [VarP c]] (NormalB $ VarE c) []]]
    ]

hasCons'Instance :: Word -> Q [Dec]
hasCons'Instance n = do
  abc@(a:bcd) <- newNames
  let
    abc' = take (fromIntegral n) abc
    bcd' = take (fromIntegral n - 1) bcd
  pure
    [ InstanceD
        Nothing
        []
        (AppT (AppT (AppT (ConT ''HasCons') (tuple n abc)) (VarT a)) (tuple (n - 1) bcd))
        [FunD 'cons' [Clause [VarP a, TupP $ VarP <$> bcd'] (NormalB $ TupE $ VarE <$> abc') []]]
    ]

hasUncons'Instance :: Word -> Q [Dec]
hasUncons'Instance n = do
  abc@(a:bcd) <- newNames
  let
    abc' = take (fromIntegral n) abc
    bcd' = take (fromIntegral n - 1) bcd
  pure
    [ InstanceD
        Nothing
        []
        (AppT (AppT (AppT (ConT ''HasUncons') (tuple n abc)) (VarT a)) (tuple (n - 1) bcd))
        [FunD 'uncons' [Clause [TupP $ VarP <$> abc'] (NormalB $ TupE [VarE a, TupE $ VarE <$> bcd']) []]]
    ]

hasHeadInstance :: Word -> Q [Dec]
hasHeadInstance n = do
  abc <- newNames
  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''HasHead) (tuple n abc))
        []
    ]

hasTailInstance :: Word -> Q [Dec]
hasTailInstance n = do
  abc <- newNames
  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''HasTail) (tuple n abc))
        []
    ]

hasInitInstance :: Word -> Q [Dec]
hasInitInstance n = do
  abc <- newNames
  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''HasInit) (tuple n abc))
        []
    ]

hasLastInstance :: Word -> Q [Dec]
hasLastInstance n = do
  abc <- newNames
  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''HasLast) (tuple n abc))
        []
    ]

hasConsInstance :: Word -> Q [Dec]
hasConsInstance n = do
  a:bcd <- newNames
  pure
    [ InstanceD
        Nothing
        []
        (AppT (AppT (ConT ''HasCons) (VarT a)) (tuple (n - 1) bcd))
        []
    ]

hasUnconsInstance :: Word -> Q [Dec]
hasUnconsInstance n = do
  abc <- newNames
  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''HasUncons) (tuple n abc))
        []
    ]

hasLengthInstance :: Word -> Q [Dec]
hasLengthInstance n = do
  abc <- newNames
  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''HasLength) (tuple n abc))
        []
    ]

reverseType :: Word -> Q [Dec]
reverseType n = do
  abc <- newNames
  let
    abc' = take (fromIntegral n) abc
    cba' = Prelude.reverse abc'
  pure
    [ TySynInstD
        ''Reverse
        (TySynEqn [tuple n abc] (tuple n cba'))
    ]

hasReverse'Instance :: Word -> Q [Dec]
hasReverse'Instance n = do
  abc <- newNames
  let
    abc' = take (fromIntegral n) abc
    cba' = Prelude.reverse abc'
  pure
    [ InstanceD
        Nothing
        []
        (AppT (AppT (ConT ''HasReverse') (tuple n abc)) (tuple n cba'))
        [FunD 'reverse' [Clause [TupP $ VarP <$> abc'] (NormalB $ TupE $ VarE <$> cba') []]]
    ]

hasReverseInstance :: Word -> Q [Dec]
hasReverseInstance n = do
  abc <- newNames
  pure
    [ InstanceD
        Nothing
        []
        (AppT (ConT ''HasReverse) (tuple n abc))
        []
    ]

atTypes :: Word -> Q [Dec]
atTypes n =
  for [0 .. n - 1] atType
  where
    atType :: Word -> Q Dec
    atType i = do
      abc <- newNames
      let
        ii :: Int
        ii = fromIntegral i
        iii :: Integer
        iii = fromIntegral i
      pure $ TySynInstD ''(!!) (TySynEqn [tuple n abc, LitT $ NumTyLit iii] (VarT $ abc Prelude.!! ii))

hasAt'Instances :: Word -> Q [Dec]
hasAt'Instances n =
  for [0 .. n - 1] hasAt'Instance
  where
    hasAt'Instance :: Word -> Q Dec
    hasAt'Instance i = do
      abc <- newNames
      let
        iii :: Integer
        iii = fromIntegral i
        x = abc Prelude.!! (fromIntegral i)
      pure $
        InstanceD
          Nothing
          []
          (AppT (AppT (AppT (ConT ''HasAt') (tuple n abc)) (LitT $ NumTyLit iii)) (VarT x))
          [ FunD
              '(!!!)
              [ Clause
                  [TupP $ overwrite i (VarP x) $ replicate (fromIntegral n) WildP, WildP]
                  (NormalB (VarE x))
                  []
              ]
          ]
    overwrite :: Word -> a -> [a] -> [a]
    overwrite 0 a (_:xs) = a : xs
    overwrite i a (x:xs) = x : overwrite (i - 1) a xs
    overwrite _ _ [] = error "hasAt'Instances.overwrite"

hasAtInstances :: Word -> Q [Dec]
hasAtInstances n =
  for [0 .. n - 1] hasAtInstance
  where
    hasAtInstance :: Word -> Q Dec
    hasAtInstance i = do
      abc <- newNames
      pure $
        InstanceD
          Nothing
          []
          (AppT (AppT (ConT ''HasAt) (tuple n abc)) (LitT $ NumTyLit $ fromIntegral i))
          []

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
