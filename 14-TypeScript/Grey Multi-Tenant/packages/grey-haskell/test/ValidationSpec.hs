{-# LANGUAGE OverloadedStrings #-}

module ValidationSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Grey.Error.Result (Result(..), isErr, isOk)
import Grey.Error.GreyError (GreyError(..), errorCode)
import Grey.Error.Codes (ErrorCode(..))

import qualified Grey.Domain.AuthClient as Auth
import qualified Grey.Domain.UserClient as Users
import qualified Grey.Domain.ProjectsClient as Projects
import qualified Grey.Domain.QueryClient as Query
import qualified Grey.Domain.MutationClient as Mutation

spec :: Spec
spec = do
  describe "Auth validation" $ do
    describe "validateCredentials" $ do
      it "rejects empty username" $ do
        let creds = Auth.LoginCredentials { Auth.username = "", Auth.password = "pass" }
        let result = Auth.validateCredentials creds
        result `shouldSatisfy` isLeft
        getErrorCode result `shouldBe` ValidationError

      it "rejects whitespace-only username" $ do
        let creds = Auth.LoginCredentials { Auth.username = "   ", Auth.password = "pass" }
        let result = Auth.validateCredentials creds
        result `shouldSatisfy` isLeft

      it "rejects empty password" $ do
        let creds = Auth.LoginCredentials { Auth.username = "user", Auth.password = "" }
        let result = Auth.validateCredentials creds
        result `shouldSatisfy` isLeft

      it "accepts valid credentials" $ do
        let creds = Auth.LoginCredentials { Auth.username = "user", Auth.password = "pass" }
        let result = Auth.validateCredentials creds
        result `shouldSatisfy` isRight

    describe "validateRefreshToken" $ do
      it "rejects empty token" $ do
        let result = Auth.validateRefreshToken ""
        result `shouldSatisfy` isLeft

      it "accepts valid token" $ do
        let result = Auth.validateRefreshToken "token123"
        result `shouldSatisfy` isRight

  describe "User validation" $ do
    describe "validateUserId" $ do
      it "rejects empty user ID" $ do
        let result = Users.validateUserId ""
        result `shouldSatisfy` isLeft
        getErrorCode result `shouldBe` ValidationError

      it "accepts valid user ID" $ do
        let result = Users.validateUserId "user-123"
        result `shouldSatisfy` isRight

  describe "Projects validation" $ do
    describe "validateProjectId" $ do
      it "rejects empty project ID" $ do
        let result = Projects.validateProjectId ""
        result `shouldSatisfy` isLeft

      it "accepts valid project ID" $ do
        let result = Projects.validateProjectId "proj-123"
        result `shouldSatisfy` isRight

    describe "validateCreateData" $ do
      it "rejects empty name" $ do
        let d = Projects.CreateProjectData
              { Projects.cpName = ""
              , Projects.cpDescription = Nothing
              , Projects.cpMetadata = Map.empty
              }
        let result = Projects.validateCreateData d
        result `shouldSatisfy` isLeft

      it "accepts valid data" $ do
        let d = Projects.CreateProjectData
              { Projects.cpName = "My Project"
              , Projects.cpDescription = Nothing
              , Projects.cpMetadata = Map.empty
              }
        let result = Projects.validateCreateData d
        result `shouldSatisfy` isRight

    describe "validateListOptions" $ do
      it "rejects page < 1" $ do
        let opts = Projects.defaultListOptions { Projects.loPage = 0 }
        let result = Projects.validateListOptions opts
        result `shouldSatisfy` isLeft

      it "rejects perPage < 1" $ do
        let opts = Projects.defaultListOptions { Projects.loPerPage = 0 }
        let result = Projects.validateListOptions opts
        result `shouldSatisfy` isLeft

      it "rejects perPage > 100" $ do
        let opts = Projects.defaultListOptions { Projects.loPerPage = 101 }
        let result = Projects.validateListOptions opts
        result `shouldSatisfy` isLeft

      it "accepts default options" $ do
        let result = Projects.validateListOptions Projects.defaultListOptions
        result `shouldSatisfy` isRight

  describe "Query validation" $ do
    describe "validateQuery" $ do
      it "rejects empty query" $ do
        let req = Query.QueryRequest
              { Query.qrQuery = ""
              , Query.qrVariables = Map.empty
              , Query.qrOperationName = Nothing
              }
        let result = Query.validateQuery req
        result `shouldSatisfy` isLeft

      it "accepts valid query" $ do
        let req = Query.QueryRequest
              { Query.qrQuery = "{ users { id } }"
              , Query.qrVariables = Map.empty
              , Query.qrOperationName = Nothing
              }
        let result = Query.validateQuery req
        result `shouldSatisfy` isRight

    describe "validateBatch" $ do
      it "rejects empty batch" $ do
        let result = Query.validateBatch []
        result `shouldSatisfy` isLeft

      it "accepts valid batch" $ do
        let req = Query.QueryRequest
              { Query.qrQuery = "{ users { id } }"
              , Query.qrVariables = Map.empty
              , Query.qrOperationName = Nothing
              }
        let result = Query.validateBatch [req]
        result `shouldSatisfy` isRight

  describe "Mutation validation" $ do
    describe "validateMutation" $ do
      it "rejects empty mutation" $ do
        let req = Mutation.MutationRequest
              { Mutation.mrMutation = ""
              , Mutation.mrVariables = Map.empty
              , Mutation.mrOperationName = Nothing
              }
        let result = Mutation.validateMutation req
        result `shouldSatisfy` isLeft

      it "accepts valid mutation" $ do
        let req = Mutation.MutationRequest
              { Mutation.mrMutation = "mutation { create }"
              , Mutation.mrVariables = Map.empty
              , Mutation.mrOperationName = Nothing
              }
        let result = Mutation.validateMutation req
        result `shouldSatisfy` isRight

-- Helper functions
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

getErrorCode :: Either GreyError a -> ErrorCode
getErrorCode (Left e) = errorCode e
getErrorCode _ = error "Expected Left"
