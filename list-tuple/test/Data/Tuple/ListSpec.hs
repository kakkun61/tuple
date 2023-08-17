{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors -Wno-redundant-constraints -Wno-incomplete-patterns #-}

module Data.Tuple.ListSpec (spec) where

import Data.Tuple.List

import Test.Hspec
import Test.ShouldNotTypecheck

import Prelude hiding (head, init, last, length, reverse, tail, (!!))

import Data.Proxy
import Data.Tuple.Single

spec :: Spec
spec = do
  describe "0-tuple" $ do
    describe "Unit" $ do
      it "head'" $ do
        shouldNotTypecheck (head' () :: ())

      it "tail'" $ do
        shouldNotTypecheck (tail' () :: ())

      it "init'" $ do
        shouldNotTypecheck (init' () :: ())

      it "last'" $ do
        shouldNotTypecheck (last' () :: ())

      it "cons'" $ do
        shouldNotTypecheck (cons' 'a' () :: ())

      it "uncons'" $ do
        shouldNotTypecheck (uncons' () :: ((), ()))

      it "head" $ do
        shouldNotTypecheck $ head ()

      it "tail" $ do
        shouldNotTypecheck $ tail ()

      it "init" $ do
        shouldNotTypecheck $ init ()

      it "last" $ do
        shouldNotTypecheck $ last ()

      it "cons" $ do
        shouldNotTypecheck (cons 'a' () :: ())

      it "uncons" $ do
        shouldNotTypecheck $ uncons ()

      it "Length" $ do
        (Proxy :: Proxy (Length ())) `shouldBe` (Proxy :: Proxy 0)

      it "length" $ do
        length () `shouldBe` (0 :: Int)

      it "Null" $ do
        case () of { Null -> True } `shouldBe` True

      describe "Cons'" $ do
        it "construct" $ do
          shouldNotTypecheck $ Cons' 'a' ()
        it "deconstruct" $ do
          shouldNotTypecheck $ case () of { Cons' _ _ -> () ; _ -> () }

      describe "Cons" $ do
        it "construct" $ do
          shouldNotTypecheck $ Cons 'a' ()
        it "deconstruct" $ do
          shouldNotTypecheck $ case () of { Cons _ _ -> () ; _ -> () }

      it "reverse'" $ do
        reverse' () `shouldBe` ()

      it "reverse" $ do
        reverse () `shouldBe` ()

      it "(!!!)" $ do
        shouldNotTypecheck (() !!! (Proxy :: Proxy 0) :: ())

      it "at'" $ do
        shouldNotTypecheck $ at' @() @0 @() ()

      it "(!!)" $ do
        shouldNotTypecheck $ () !! (Proxy :: Proxy 0)

      it "at" $ do
        shouldNotTypecheck $ at @() @0 ()

    describe "Proxy" $ do
      it "head'" $ do
        shouldNotTypecheck (head' (Proxy :: Proxy Int) :: ())

      -- When searching for "HasTail'" instance for "Proxy", "HasTail' (Proxy a) ()" is matched.
      -- But it is type error, because of "TypeError" constraint.
      -- However on deferred type error environent this does not throw type error,
      -- becouse the "tail'" implementation of "HasHead' (Proxy a) ()" does not call any "TypeError"'s functions.
      -- it "tail'" $ do
      --   shouldNotTypecheck (tail' (Proxy :: Proxy Int) :: ())

      -- The same to tail'.
      -- it "init'" $ do
      --   shouldNotTypecheck (init' (Proxy :: Proxy Int) :: ())

      it "last'" $ do
        shouldNotTypecheck (last' (Proxy :: Proxy Int) :: ())

      it "cons'" $ do
        let a = 0 :: Int
        shouldNotTypecheck (cons' a (Proxy :: Proxy Int) :: Proxy Int)

      it "uncons'" $ do
        shouldNotTypecheck (uncons' (Proxy :: Proxy Int) :: ((), ()))

      it "head" $ do
        shouldNotTypecheck $ head (Proxy :: Proxy Int)

      it "tail" $ do
        shouldNotTypecheck $ tail (Proxy :: Proxy Int)

      it "init" $ do
        shouldNotTypecheck $ init (Proxy :: Proxy Int)

      it "last" $ do
        shouldNotTypecheck $ last (Proxy :: Proxy Int)

      it "uncons" $ do
        shouldNotTypecheck $ uncons (Proxy :: Proxy Int)

      it "Length" $ do
        (Proxy :: Proxy (Length (Proxy ()))) `shouldBe` (Proxy :: Proxy 0)

      it "length" $ do
       length (Proxy :: Proxy Int) `shouldBe` (0 :: Int)

      it "Null" $ do
        case (Proxy :: Proxy Int) of { Null -> True } `shouldBe` True

      describe "Cons'" $ do
        it "construct" $ do
          shouldNotTypecheck $ Cons' 'a' ()
        it "deconstruct" $ do
          shouldNotTypecheck $ case (Proxy :: Proxy Int) of { Cons' _ _ -> () ; _ -> () }

      describe "Cons" $ do
        it "construct" $ do
          shouldNotTypecheck $ Cons 'a' ()
        it "deconstruct" $ do
          shouldNotTypecheck $ case (Proxy :: Proxy Int) of { Cons _ _ -> () ; _ -> () }

      it "reverse'" $ do
        reverse' (Proxy :: Proxy Int) `shouldBe` (Proxy :: Proxy Int)

      it "reverse" $ do
        reverse (Proxy :: Proxy Int) `shouldBe` (Proxy :: Proxy Int)

      it "(!!!)" $ do
        shouldNotTypecheck ((Proxy :: Proxy Int) !!! (Proxy :: Proxy 0) :: Int)

      it "at'" $ do
        shouldNotTypecheck $ at' @(Proxy Int) @0 @Int Proxy

      it "(!!)" $ do
        shouldNotTypecheck $ (Proxy :: Proxy Int) !! (Proxy :: Proxy 0)

      it "at" $ do
        shouldNotTypecheck $ at @(Proxy Int) @0 Proxy

  describe "2-tuple" $ do
    describe "Single" $ do
      it "head'" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        head' (a, b) `shouldBe` a

      it "tail'" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        shouldNotTypecheck $ tail' (a, b)

      it "init'" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        shouldNotTypecheck $ init' (a, b)

      it "last'" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        last' (a, b) `shouldBe` b

      it "head" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        head (a, b) `shouldBe` a

      it "tail" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        shouldNotTypecheck $ tail (a, b)

      it "init" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        shouldNotTypecheck $ init (a, b)

      it "last" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        last (a, b) `shouldBe` b

      it "cons" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        shouldNotTypecheck $ cons a (Single b)

      it "uncons" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        shouldNotTypecheck $ uncons (a, b)

      it "Length" $ do
        let
          target :: Length (Int, Int) ~ 2 => ()
          target = ()
        seq target $ pure () :: IO ()

      it "length" $ do
        length ((0, 1) :: (Int, Int)) `shouldBe` (2 :: Int)

      it "Null" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        shouldNotTypecheck $ case (a, b) of { Null -> () }

      describe "Cons'" $ do
        it "construct" $ do
          let
            a, b :: Int
            a = 0
            b = 1
          shouldNotTypecheck $ (Cons' a (Single b) :: (Int, Int))

        it "deconstruct" $ do
          let
            a, b :: Int
            a = 0
            b = 1
          shouldNotTypecheck $ case (a, b) of { Cons' _ (Single _) -> () }

      describe "Cons" $ do
        it "construct" $ do
          let
            a, b :: Int
            a = 0
            b = 1
          shouldNotTypecheck $ Cons a (Single b)

        it "deconstruct" $ do
          let
            a, b :: Int
            a = 0
            b = 1
          shouldNotTypecheck $ case (a, b) of { Cons _ (Single _) -> () }

      it "reverse'" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        reverse' (a, b) `shouldBe` (b, a)

      it "reverse" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        reverse (a, b) `shouldBe` (b, a)

      it "(!!!)" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        (a, b) !!! (Proxy :: Proxy 0) `shouldBe` a
        (a, b) !!! (Proxy :: Proxy 1) `shouldBe` b

      it "at'" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        at' @_ @0 @_ (a, b) `shouldBe` a
        at' @_ @1 @_ (a, b) `shouldBe` b

      it "(!!)" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        (a, b) !! (Proxy :: Proxy 0) `shouldBe` a
        (a, b) !! (Proxy :: Proxy 1) `shouldBe` b

      it "at" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        at @_ @0 (a, b) `shouldBe` a
        at @_ @1 (a, b) `shouldBe` b

  describe "3-tuple" $ do
    it "head'" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      head' (a, b, c) `shouldBe` a

    it "tail'" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      tail' (a, b, c) `shouldBe` (b, c)

    it "init'" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      init' (a, b, c) `shouldBe` (a, b)

    it "last'" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      last' (a, b, c) `shouldBe` c

    it "cons'" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      cons' a (b, c) `shouldBe` (a, b, c)

    it "uncons'" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      uncons' (a, b, c) `shouldBe` (a, (b, c))

    it "head" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      head (a, b, c) `shouldBe` a

    it "tail" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      tail (a, b, c) `shouldBe` (b, c)

    it "init" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      init (a, b, c) `shouldBe` (a, b)

    it "last" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      last (a, b, c) `shouldBe` c

    it "cons" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      cons a (b, c) `shouldBe` (a, b, c)

    it "uncons" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      uncons (a, b, c) `shouldBe` (a, (b, c))

    it "Length" $ do
      let
        target :: Length (Int, Int, Int) ~ 3 => ()
        target = ()
      seq target $ pure () :: IO ()

    it "length" $ do
      length ((0, 1, 2) :: (Int, Int, Int)) `shouldBe` (3 :: Int)

    it "Null" $ do
      shouldNotTypecheck $ case (0, 1, 2) :: (Int, Int, Int) of { Null -> () }

    describe "Cons'" $ do
      it "construct" $ do
        let
          a, b, c :: Int
          a = 0
          b = 1
          c = 2
        Cons' a (b, c) `shouldBe` (a, b, c)

      it "deconstruct" $ do
        let
          a, b, c :: Int
          a = 0
          b = 1
          c = 2
        case (a, b, c) of { Cons' a' (b', c') -> (a, b, c) == (a', b', c') } `shouldBe` True

    describe "Cons" $ do
      it "construct" $ do
        let
          a, b, c :: Int
          a = 0
          b = 1
          c = 2
        Cons a (b, c) `shouldBe` (a, b, c)

      it "deconstruct" $ do
        let
          a, b, c :: Int
          a = 0
          b = 1
          c = 2
        case (a, b, c) of { Cons a' (b', c') -> (a, b, c) == (a', b', c') } `shouldBe` True

    it "reverse'" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      reverse' (a, b, c) `shouldBe` (c, b, a)

    it "reverse" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      reverse (a, b, c) `shouldBe` (c, b, a)

    it "(!!!)" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      (a, b, c) !!! (Proxy :: Proxy 0) `shouldBe` a
      (a, b, c) !!! (Proxy :: Proxy 1) `shouldBe` b
      (a, b, c) !!! (Proxy :: Proxy 2) `shouldBe` c

    it "at'" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      at' @_ @0 @_ (a, b, c) `shouldBe` a
      at' @_ @1 @_ (a, b, c) `shouldBe` b
      at' @_ @2 @_ (a, b, c) `shouldBe` c

    it "(!!)" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      (a, b, c) !! (Proxy :: Proxy 0) `shouldBe` a
      (a, b, c) !! (Proxy :: Proxy 1) `shouldBe` b
      (a, b, c) !! (Proxy :: Proxy 2) `shouldBe` c

    it "at" $ do
      let
        a, b, c :: Int
        a = 0
        b = 1
        c = 2
      at @_ @0 (a, b, c) `shouldBe` a
      at @_ @1 (a, b, c) `shouldBe` b
      at @_ @2 (a, b, c) `shouldBe` c
