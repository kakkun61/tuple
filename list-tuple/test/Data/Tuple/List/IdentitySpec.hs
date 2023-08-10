{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications#-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors -Wno-redundant-constraints #-}

module Data.Tuple.List.IdentitySpec (spec) where

import Data.Tuple.List
import Data.Tuple.List.Identity ()

import Test.Hspec

import Prelude hiding (head, tail, init, last, length, reverse, (!!))

import           Data.Functor.Identity
import           Data.Proxy
import           Test.ShouldNotTypecheck

spec :: Spec
spec = do
  describe "1-tuple" $ do
    describe "Identity" $ do
      it "head" $ do
        let a = ()
        head (Identity a) `shouldBe` a

      it "tail" $ do
        tail (Identity ()) `shouldBe` ()

      it "init" $ do
        init (Identity ()) `shouldBe` ()

      it "last" $ do
        let a = ()
        last (Identity a) `shouldBe` a

      it "cons" $ do
        let a = ()
        cons a () `shouldBe` Identity a

      it "uncons" $ do
        let a = ()
        uncons (Identity a) `shouldBe` (a, ())

      it "Length" $ do
        let
          target :: Length (Identity Int) ~ 1 => ()
          target = ()
        seq target $ pure () :: IO ()

      it "length" $ do
        length (Identity ()) `shouldBe` (1 :: Int)

      it "Null" $ do
        shouldNotTypecheck $ case Identity () of { Null -> False ; _ -> False }

      describe "Cons'" $ do
        it "construct" $ do
          let a = ()
          (Cons a () :: Identity ()) `shouldBe` Identity a

        it "deconstruct" $ do
          case Identity () of { Cons () () -> True } `shouldBe` True

      it "reverse" $ do
        let a = ()
        reverse (Identity a) `shouldBe` Identity ()

      it "(!!)" $ do
        let a = ()
        Identity a !! (Proxy :: Proxy 0) `shouldBe` a

      it "at" $ do
        let a = ()
        at @_ @0 (Identity a) `shouldBe` a

  describe "2-tuple" $ do
    describe "Identity" $ do
      it "tail'" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        tail' (a, b) `shouldBe` Identity b

      it "init'" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        init' (a, b) `shouldBe` Identity a

      it "tail" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        tail (a, b) `shouldBe` Identity b

      it "init" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        init (a, b) `shouldBe` Identity a

      it "cons'" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        cons' a (Identity b) `shouldBe` (a, b)

      it "uncons'" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        uncons' (a, b) `shouldBe` (a, (Identity b))

      it "uncons" $ do
        let
          a, b :: Int
          a = 0
          b = 1
        uncons (a, b) `shouldBe` (a, (Identity b))

      describe "Cons" $ do
        it "construct" $ do
          let
            a, b :: Int
            a = 0
            b = 1
          Cons a (Identity b) `shouldBe` (a, b)
