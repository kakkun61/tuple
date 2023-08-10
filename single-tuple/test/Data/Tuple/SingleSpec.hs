{-# LANGUAGE CPP #-}
module Data.Tuple.SingleSpec (spec) where

import Data.Tuple.Single

import Test.Hspec

import Data.Functor.Identity
import Data.Tuple.Only

#if MIN_VERSION_OneTuple(0,3,0)
#if !MIN_VERSION_base(4,15,0)
import qualified Data.Tuple.Solo as OneTuple
#endif
#else
import Data.Tuple.OneTuple
#endif

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

#if MIN_VERSION_OneTuple(0,3,0)
#if !MIN_VERSION_base(4,15,0)
  describe "OneTuple.Solo" $ do
    it "unwrap . wrap" $ do
      unwrap (wrap () :: OneTuple.Solo ()) `shouldBe` ()

    it "wrap . unwrap" $ do
      let a = OneTuple.Solo ()
      wrap (unwrap a) `shouldBe` a

    it "pattern destruct construct" $ do
      let Single a = Single () :: OneTuple.Solo ()
      a `shouldBe` ()
#endif
#else
  describe "OneTuple" $ do
    it "unwrap . wrap" $ do
      unwrap (wrap () :: OneTuple ()) `shouldBe` ()

    it "wrap . unwrap" $ do
      let a = OneTuple ()
      wrap (unwrap a) `shouldBe` a

    it "pattern destruct construct" $ do
      let Single a = Single () :: OneTuple ()
      a `shouldBe` ()
#endif
