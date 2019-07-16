module Data.Tuple.SingleSpec (spec) where

import Data.Tuple.Single

import Test.Hspec

import Data.Functor.Identity
import Data.Tuple.OneTuple
import Data.Tuple.Only

spec :: Spec
spec = do
  describe "Identity" $ do
    it "unwrap . wrap" $ do
      unwrap (wrap () :: Identity ()) `shouldBe` ()

    it "wrap . unwrap" $ do
      let a = Identity ()
      wrap (unwrap a) `shouldBe` a

    it "pattern destruct construct" $ do
      let Single a = Single () :: Identity ()
      a `shouldBe` ()

  describe "Only" $ do
    it "unwrap . wrap" $ do
      unwrap (wrap () :: Only ()) `shouldBe` ()

    it "wrap . unwrap" $ do
      let a = Only ()
      wrap (unwrap a) `shouldBe` a

    it "pattern destruct construct" $ do
      let Single a = Single () :: Only ()
      a `shouldBe` ()

  describe "OneTuple" $ do
    it "unwrap . wrap" $ do
      unwrap (wrap () :: OneTuple ()) `shouldBe` ()

    it "wrap . unwrap" $ do
      let a = OneTuple ()
      wrap (unwrap a) `shouldBe` a

    it "pattern destruct construct" $ do
      let Single a = Single () :: OneTuple ()
      a `shouldBe` ()
