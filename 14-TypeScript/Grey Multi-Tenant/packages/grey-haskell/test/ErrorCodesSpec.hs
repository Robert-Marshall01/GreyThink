{-# LANGUAGE OverloadedStrings #-}

module ErrorCodesSpec (spec) where

import Test.Hspec
import Grey.Error.Codes

spec :: Spec
spec = do
  describe "fromHttpStatus" $ do
    it "returns Unauthorized for 401" $ do
      fromHttpStatus 401 `shouldBe` Unauthorized

    it "returns Forbidden for 403" $ do
      fromHttpStatus 403 `shouldBe` Forbidden

    it "returns NotFound for 404" $ do
      fromHttpStatus 404 `shouldBe` NotFound

    it "returns ValidationError for 400" $ do
      fromHttpStatus 400 `shouldBe` ValidationError

    it "returns ValidationError for 422" $ do
      fromHttpStatus 422 `shouldBe` ValidationError

    it "returns Timeout for 408" $ do
      fromHttpStatus 408 `shouldBe` Timeout

    it "returns ServerError for 500" $ do
      fromHttpStatus 500 `shouldBe` ServerError

    it "returns ServerError for 503" $ do
      fromHttpStatus 503 `shouldBe` ServerError

    it "returns Unknown for unrecognized status" $ do
      fromHttpStatus 418 `shouldBe` Unknown

  describe "fromGrpcStatus" $ do
    it "returns Unauthorized for UNAUTHENTICATED (16)" $ do
      fromGrpcStatus 16 `shouldBe` Unauthorized

    it "returns Forbidden for PERMISSION_DENIED (7)" $ do
      fromGrpcStatus 7 `shouldBe` Forbidden

    it "returns NotFound for NOT_FOUND (5)" $ do
      fromGrpcStatus 5 `shouldBe` NotFound

    it "returns ValidationError for INVALID_ARGUMENT (3)" $ do
      fromGrpcStatus 3 `shouldBe` ValidationError

    it "returns Timeout for DEADLINE_EXCEEDED (4)" $ do
      fromGrpcStatus 4 `shouldBe` Timeout

    it "returns NetworkError for UNAVAILABLE (14)" $ do
      fromGrpcStatus 14 `shouldBe` NetworkError

    it "returns ServerError for INTERNAL (13)" $ do
      fromGrpcStatus 13 `shouldBe` ServerError

  describe "isRetryable" $ do
    it "returns True for NetworkError" $ do
      isRetryable NetworkError `shouldBe` True

    it "returns True for Timeout" $ do
      isRetryable Timeout `shouldBe` True

    it "returns True for ServerError" $ do
      isRetryable ServerError `shouldBe` True

    it "returns False for Unauthorized" $ do
      isRetryable Unauthorized `shouldBe` False

    it "returns False for ValidationError" $ do
      isRetryable ValidationError `shouldBe` False

    it "returns False for NotFound" $ do
      isRetryable NotFound `shouldBe` False

  describe "errorMessage" $ do
    it "returns non-empty message for all codes" $ do
      errorMessage Unauthorized `shouldNotBe` ""
      errorMessage Forbidden `shouldNotBe` ""
      errorMessage NotFound `shouldNotBe` ""
      errorMessage ValidationError `shouldNotBe` ""
      errorMessage NetworkError `shouldNotBe` ""
      errorMessage Timeout `shouldNotBe` ""
      errorMessage ServerError `shouldNotBe` ""
      errorMessage Unknown `shouldNotBe` ""
