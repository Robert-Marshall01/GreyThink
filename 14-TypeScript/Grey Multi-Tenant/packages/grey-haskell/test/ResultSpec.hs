{-# LANGUAGE OverloadedStrings #-}

module ResultSpec (spec) where

import Test.Hspec
import Grey.Error.Result
import Grey.Error.GreyError (GreyError(..))
import qualified Grey.Error.GreyError as GE
import Grey.Error.Codes (ErrorCode(..))

spec :: Spec
spec = do
  describe "Result constructors" $ do
    it "ok creates successful result" $ do
      let result = ok "test" :: Result String
      isOk result `shouldBe` True
      isErr result `shouldBe` False

    it "err creates failed result" $ do
      let e = GE.validationError "test error" Nothing
      let result = err e :: Result String
      isOk result `shouldBe` False
      isErr result `shouldBe` True

  describe "fromResult" $ do
    it "calls onOk for successful result" $ do
      let result = ok (5 :: Int)
      fromResult (const "error") show result `shouldBe` "5"

    it "calls onErr for failed result" $ do
      let e = GE.validationError "error" Nothing
      let result = err e :: Result Int
      fromResult (const "error") show result `shouldBe` "error"

  describe "getOr" $ do
    it "returns value for successful result" $ do
      let result = ok (5 :: Int)
      getOr 0 result `shouldBe` 5

    it "returns default for failed result" $ do
      let e = GE.validationError "error" Nothing
      let result = err e :: Result Int
      getOr 0 result `shouldBe` 0

  describe "mapResult (Functor)" $ do
    it "transforms successful value" $ do
      let result = ok (5 :: Int)
      let mapped = fmap (* 2) result
      fromResult (const 0) id mapped `shouldBe` 10

    it "propagates error" $ do
      let e = GE.validationError "error" Nothing
      let result = err e :: Result Int
      let mapped = fmap (* 2) result
      isErr mapped `shouldBe` True

  describe "Applicative" $ do
    it "applies function in Ok to value in Ok" $ do
      let f = ok ((* 2) :: Int -> Int)
      let x = ok (5 :: Int)
      fromResult (const 0) id (f <*> x) `shouldBe` 10

    it "propagates left error" $ do
      let e = GE.validationError "error" Nothing
      let f = err e :: Result (Int -> Int)
      let x = ok (5 :: Int)
      isErr (f <*> x) `shouldBe` True

    it "propagates right error" $ do
      let e = GE.validationError "error" Nothing
      let f = ok ((* 2) :: Int -> Int)
      let x = err e :: Result Int
      isErr (f <*> x) `shouldBe` True

  describe "Monad (flatMap)" $ do
    it "chains successful operations" $ do
      let result = ok (5 :: Int) >>= \x -> ok (x * 2)
      fromResult (const 0) id result `shouldBe` 10

    it "short-circuits on first error" $ do
      let e = GE.validationError "error" Nothing
      let result = err e >>= \x -> ok ((x :: Int) * 2)
      isErr result `shouldBe` True

    it "short-circuits on second error" $ do
      let e = GE.validationError "error" Nothing
      let result = ok (5 :: Int) >>= \_ -> err e
      isErr result `shouldBe` True

  describe "partition" $ do
    it "separates errors and successes" $ do
      let e = GE.validationError "error" Nothing
      let results = [ok (1 :: Int), err e, ok 2, ok 3]
      let (errs, oks) = partition results
      length errs `shouldBe` 1
      oks `shouldBe` [1, 2, 3]

    it "handles all successes" $ do
      let results = [ok (1 :: Int), ok 2, ok 3]
      let (errs, oks) = partition results
      length errs `shouldBe` 0
      oks `shouldBe` [1, 2, 3]

    it "handles all errors" $ do
      let e = GE.validationError "error" Nothing
      let results = [err e, err e] :: [Result Int]
      let (errs, oks) = partition results
      length errs `shouldBe` 2
      oks `shouldBe` []
