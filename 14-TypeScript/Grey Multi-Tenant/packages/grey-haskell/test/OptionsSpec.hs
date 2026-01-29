{-# LANGUAGE OverloadedStrings #-}

module OptionsSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Grey.Config.Options

spec :: Spec
spec = do
  describe "defaultOptions" $ do
    it "has localhost as host" $ do
      optHost defaultOptions `shouldBe` "localhost"

    it "has port 50051" $ do
      optPort defaultOptions `shouldBe` 50051

    it "has TLS disabled" $ do
      optUseTls defaultOptions `shouldBe` False

    it "has 30 second timeout" $ do
      optTimeoutSecs defaultOptions `shouldBe` 30

    it "has no auth token" $ do
      optAuthToken defaultOptions `shouldBe` Nothing

    it "has empty custom headers" $ do
      optCustomHeaders defaultOptions `shouldBe` Map.empty

  describe "localOptions" $ do
    it "uses default port when Nothing" $ do
      optPort (localOptions Nothing) `shouldBe` 50051

    it "uses custom port when provided" $ do
      optPort (localOptions (Just 9000)) `shouldBe` 9000

    it "has TLS disabled" $ do
      optUseTls (localOptions Nothing) `shouldBe` False

  describe "productionOptions" $ do
    it "uses provided host" $ do
      optHost (productionOptions "api.example.com" Nothing) `shouldBe` "api.example.com"

    it "uses default port 443 when Nothing" $ do
      optPort (productionOptions "api.example.com" Nothing) `shouldBe` 443

    it "uses custom port when provided" $ do
      optPort (productionOptions "api.example.com" (Just 8443)) `shouldBe` 8443

    it "has TLS enabled" $ do
      optUseTls (productionOptions "api.example.com" Nothing) `shouldBe` True

  describe "withHost" $ do
    it "updates host" $ do
      let opts = withHost "newhost" defaultOptions
      optHost opts `shouldBe` "newhost"

    it "preserves other fields" $ do
      let opts = withHost "newhost" defaultOptions
      optPort opts `shouldBe` 50051

  describe "withPort" $ do
    it "updates port" $ do
      let opts = withPort 9000 defaultOptions
      optPort opts `shouldBe` 9000

  describe "withTls" $ do
    it "enables TLS" $ do
      let opts = withTls True defaultOptions
      optUseTls opts `shouldBe` True

    it "disables TLS" $ do
      let opts = withTls False (productionOptions "host" Nothing)
      optUseTls opts `shouldBe` False

  describe "withTimeout" $ do
    it "updates timeout" $ do
      let opts = withTimeout 60 defaultOptions
      optTimeoutSecs opts `shouldBe` 60

  describe "withAuthToken" $ do
    it "sets auth token" $ do
      let opts = withAuthToken "token123" defaultOptions
      optAuthToken opts `shouldBe` Just "token123"

  describe "withHeader" $ do
    it "adds custom header" $ do
      let opts = withHeader "X-Custom" "value" defaultOptions
      Map.lookup "X-Custom" (optCustomHeaders opts) `shouldBe` Just "value"

    it "can add multiple headers" $ do
      let opts = withHeader "X-One" "1" $ withHeader "X-Two" "2" defaultOptions
      Map.lookup "X-One" (optCustomHeaders opts) `shouldBe` Just "1"
      Map.lookup "X-Two" (optCustomHeaders opts) `shouldBe` Just "2"

  describe "getEndpoint" $ do
    it "returns host:port format" $ do
      getEndpoint defaultOptions `shouldBe` "localhost:50051"

    it "works with production options" $ do
      getEndpoint (productionOptions "api.example.com" Nothing) `shouldBe` "api.example.com:443"
