{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Grey.Domain.AuthClient
-- Description : Domain client for authentication operations
-- License     : MIT
--
-- High-level authentication client with validation and error handling.

module Grey.Domain.AuthClient
  ( -- * Data Types
    AuthTokens(..)
  , LoginCredentials(..)
    -- * Operations
  , login
  , logout
  , refresh
    -- * Validation
  , validateCredentials
  , validateRefreshToken
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Grey.Error.GreyError (GreyError)
import qualified Grey.Error.GreyError as GE
import Grey.Error.Result (Result(..))
import Grey.GRPC.Client (GrpcClient)
import qualified Grey.GRPC.AuthService as GRPC

-- | Authentication tokens returned from login/refresh.
data AuthTokens = AuthTokens
  { accessToken  :: !Text
  , refreshToken :: !Text
  , expiresIn    :: !Int
  } deriving (Eq, Show)

-- | Login credentials.
data LoginCredentials = LoginCredentials
  { username :: !Text
  , password :: !Text
  } deriving (Eq, Show)

-- | Convert gRPC response to domain type.
toAuthTokens :: GRPC.LoginResponse -> AuthTokens
toAuthTokens resp = AuthTokens
  { accessToken = GRPC.lrAccessToken resp
  , refreshToken = GRPC.lrRefreshToken resp
  , expiresIn = GRPC.lrExpiresIn resp
  }

-- | Convert refresh response to domain type.
toAuthTokensFromRefresh :: GRPC.RefreshResponse -> AuthTokens
toAuthTokensFromRefresh resp = AuthTokens
  { accessToken = GRPC.rrAccessToken resp
  , refreshToken = GRPC.rrRefreshToken resp
  , expiresIn = GRPC.rrExpiresIn resp
  }

-- | Validate login credentials.
validateCredentials :: LoginCredentials -> Either GreyError LoginCredentials
validateCredentials creds
  | T.null (T.strip (username creds)) =
      Left $ GE.validationError "Username is required" Nothing
  | T.null (T.strip (password creds)) =
      Left $ GE.validationError "Password is required" Nothing
  | otherwise =
      Right creds

-- | Validate a refresh token.
validateRefreshToken :: Text -> Either GreyError Text
validateRefreshToken token
  | T.null (T.strip token) =
      Left $ GE.validationError "Refresh token is required" Nothing
  | otherwise =
      Right token

-- | Authenticate with username and password.
login :: GrpcClient -> LoginCredentials -> IO (Result AuthTokens)
login client creds = case validateCredentials creds of
  Left err -> return $ Err err
  Right validCreds -> do
    let req = GRPC.LoginRequest
          { GRPC.lrUsername = username validCreds
          , GRPC.lrPassword = password validCreds
          }
    result <- GRPC.login client req
    return $ fmap toAuthTokens result

-- | Logout the current user.
logout :: GrpcClient -> IO (Result ())
logout client = do
  result <- GRPC.logout client GRPC.LogoutRequest
  return $ fmap (const ()) result

-- | Refresh authentication tokens.
refresh :: GrpcClient -> Text -> IO (Result AuthTokens)
refresh client token = case validateRefreshToken token of
  Left err -> return $ Err err
  Right validToken -> do
    let req = GRPC.RefreshRequest
          { GRPC.rrRefreshToken = validToken
          }
    result <- GRPC.refresh client req
    return $ fmap toAuthTokensFromRefresh result
