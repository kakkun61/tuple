{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

module Data.Tuple.ListSpec (spec) where

import Data.Tuple.List

import Test.Hspec
import Test.ShouldNotTypecheck

import Prelude (Applicative (pure), Bool (False, True), seq, ($), Eq ((==)), Int)

import           Control.DeepSeq                        (NFData, force)
import           Control.Exception
import           Data.Proxy
import           Data.String.AnsiEscapeCodes.Strip.Text
import qualified Data.Text                              as T
import           Data.Tuple.Only
import           Data.Tuple.Single

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

      it "uncons" $ do
        shouldNotTypecheck $ uncons ()

      it "length" $ do
        length () `shouldBe` 0

      it "Null" $ do
        case () of { Null -> True ; _ -> False } `shouldBe` True

      describe "Cons'" $ do
        it "deconstruct" $ do
          shouldNotTypecheck $ case () of { Cons' _ _ -> False }

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

      -- When searching for "HasTail'" instance for "Proxy", "HasTail' (c a) ()" is matched.
      -- But it is type error, because "Proxy" satisfies the "Single" constraint.
      -- However on deferred type error environent this does not throw type error,
      -- becouse the "tail'" implementation of "HasHead' (c a) ()" does not call any "Single"'s functions.
      -- If "instance TypeError (Text "empty tuple") => HasTail' (Proxy a) ()" is added,
      -- it does not overlap "HasTail' (c a) ()" because they have different constraints.
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

      it "length" $ do
       length (Proxy :: Proxy Int) `shouldBe` 0

      it "Null" $ do
        case (Proxy :: Proxy Int) of { Null -> True ; _ -> False } `shouldBe` True

      describe "Cons'" $ do
        it "deconstruct" $ do
          shouldNotTypecheck $ case (Proxy :: Proxy Int) of { Cons' _ _ -> False }

      it "reverse'" $ do
        reverse' (Proxy :: Proxy Int) `shouldBe` (Proxy :: Proxy Int)

      it "reverse" $ do
        reverse (Proxy :: Proxy Int) `shouldBe` (Proxy :: Proxy Int)

      it "(!!!)" $ do
        shouldNotTypecheck ((Proxy :: Proxy Int) !!! (Proxy :: Proxy 0) :: Int

      it "at'" $ do
        shouldNotTypecheck $ at' @(Proxy Int) @0 @Int Proxy

      it "(!!)" $ do
        shouldNotTypecheck $ (Proxy :: Proxy Int) !! (Proxy :: Proxy 0)

      it "at" $ do
        shouldNotTypecheck $ at @(Proxy Int) @0 Proxy

  describe "1-tuple" $ do
    describe "Single" $ do
      it "head'" $ do
        let a = 0 :: Int
        head' (Single a :: Only Int) `shouldBe` a

      it "tail'" $ do
        tail' (Single 0 :: Only Int) `shouldBe` ()

      it "init'" $ do
        init' (Single 0 :: Only Int) `shouldBe` ()

      it "last'" $ do
        let a = 0 :: Int
        last' (Single a :: Only Int) `shouldBe` a

      it "cons'" $ do
        let a = 0 :: Int
        cons' a () `shouldBe` (Single a :: Only Int)

      it "uncons'" $ do
        let a = 0 :: Int
        uncons' (Single a :: Only Int) `shouldBe` (a, ())

      it "head" $ do
        let a = 0 :: Int
        shouldNotTypecheck (head (Single a :: Only Int) :: Int)

      it "tail" $ do
        shouldNotTypecheck (tail (Single 0 :: Only Int) :: ())

      it "init" $ do
        shouldNotTypecheck (init (Single 0 :: Only Int) :: ())

      it "last" $ do
        let a = 0 :: Int
        shouldNotTypecheck (last (Single a :: Only Int) :: Int)

      it "cons" $ do
        let a = 0 :: Int
        shouldNotTypecheck (cons a () :: Only Int)

      it "uncons" $ do
        let a = 0 :: Int
        shouldNotTypecheck (uncons (Single a :: Only Int) :: (Int, ()))

      it "Null" $ do
        shouldNotTypecheck $ case Single 0 :: Only Int of { Null -> () }

      describe "Cons'" $ do
        it "construct" $ do
          let a = 0 :: Int
          (Cons' a () :: Only Int) `shouldBe` Only a

        it "deconstruct" $ do
          let a = 0 :: Int
          case Single a :: Only Int of { Cons' b () | a == b -> True ; _ -> False } `shouldBe` True

      describe "Cons" $ do
        it "construct" $ do
          let a = 0 :: Int
          shouldNotTypecheck (Cons a () :: Only Int)

        it "deconstruct" $ do
          let a = 0 :: Int
          shouldNotTypecheck $ case Single a :: Only Int of { Cons b () | a == b -> () }

      it "reverse'" $ do
        let a = Single 0 :: Only Int
        reverse' a `shouldBe` a

      it "reverse" $ do
        let a = Single 0 :: Only Int
        shouldNotTypecheck (reverse a :: Only Int)

      it "(!!!)" $ do
        let a = 0 :: Int
        (Single a :: Only Int) !!! (Proxy :: Proxy 0) `shouldBe` a
        shouldNotTypecheck ((Single a :: Only Int) !!! (Proxy :: Proxy 1) :: Int)

      it "(at')" $ do
        let a = 0 :: Int
        at' @(Only Int) @0 @Int (Single a) `shouldBe` a
        shouldNotTypecheck $ at' @(Only Int) @1 @Int (Single a)

      it "(!!)" $ do
        let a = 0 :: Int
        shouldNotTypecheck ((Single a :: Only Int) !! (Proxy :: Proxy 0) :: Int)
        shouldNotTypecheck ((Single a :: Only Int) !! (Proxy :: Proxy 1) :: Int)

      it "(at)" $ do
        let a = 0 :: Int
        at @(Only Int) @0 (Single a) `shouldBe` a
        shouldNotTypecheck $ at @(Only Int) @1 (Single a)
