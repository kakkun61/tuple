{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fdefer-type-errors -Wno-type-errors #-}

module Data.Tuple.ListSpec (spec) where

import Data.Tuple.List

import Test.Hspec

import Prelude (Applicative (pure), Bool (False, True), seq, ($))

import           Control.DeepSeq                        (NFData, force)
import           Control.Exception
import           Data.Proxy
import           Data.String.AnsiEscapeCodes.Strip.Text
import qualified Data.Text                              as T
import           Test.ShouldNotTypecheck

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

      it "Last'" $ do
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

      it "(!!)" $ do
        shouldNotTypecheck $ () !! (Proxy :: Proxy 0)

    describe "Proxy" $ do
      it "head'" $ do
        shouldNotTypecheck (head' (Proxy :: Proxy ()) :: ())

      -- When searching for "HasTail'" instance for "Proxy", "HasTail' (c a) ()" is matched.
      -- But it is type error, because "Proxy" satisfies the "Single" constraint.
      -- However on deferred type error environent this does not throw type error,
      -- becouse the "tail'" implementation of "HasHead' (c a) ()" does not call any "Single"'s functions.
      -- If "instance TypeError (Text "empty tuple") => HasTail' (Proxy a) ()" is added,
      -- it does not overlap "HasTail' (c a) ()" because they have different constraints.
      -- it "tail'" $ do
      --   shouldNotTypecheck (tail' (Proxy :: Proxy ()) :: ())

      -- The same to tail'.
      -- it "init'" $ do
      --   shouldNotTypecheck (init' (Proxy :: Proxy ()) :: ())

      it "Last'" $ do
        shouldNotTypecheck (last' (Proxy :: Proxy ()) :: ())

      it "cons'" $ do
        shouldNotTypecheck (cons' 'a' (Proxy :: Proxy ()) :: Proxy ())

      it "uncons'" $ do
        shouldNotTypecheck (uncons' (Proxy :: Proxy ()) :: ((), ()))

      it "head" $ do
        shouldNotTypecheck $ head (Proxy :: Proxy ())

      it "tail" $ do
        shouldNotTypecheck $ tail (Proxy :: Proxy ())

      it "init" $ do
        shouldNotTypecheck $ init (Proxy :: Proxy ())

      it "last" $ do
        shouldNotTypecheck $ last (Proxy :: Proxy ())

      it "uncons" $ do
        shouldNotTypecheck $ uncons (Proxy :: Proxy ())

      it "length" $ do
       length (Proxy :: Proxy ()) `shouldBe` 0

      it "Null" $ do
        case (Proxy :: Proxy ()) of { Null -> True ; _ -> False } `shouldBe` True

      describe "Cons'" $ do
        it "deconstruct" $ do
          shouldNotTypecheck $ case (Proxy :: Proxy ()) of { Cons' _ _ -> False }

      it "reverse'" $ do
        reverse' (Proxy :: Proxy ()) `shouldBe` (Proxy :: Proxy ())

      it "reverse" $ do
        reverse (Proxy :: Proxy ()) `shouldBe` (Proxy :: Proxy ())

      it "(!!!)" $ do
        shouldNotTypecheck ((Proxy :: Proxy ()) !!! (Proxy :: Proxy 0) :: ())

      it "(!!)" $ do
        shouldNotTypecheck $ (Proxy :: Proxy ()) !! (Proxy :: Proxy 0)
