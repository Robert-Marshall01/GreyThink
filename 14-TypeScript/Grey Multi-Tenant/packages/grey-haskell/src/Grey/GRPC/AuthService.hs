{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Grey.GRPC.AuthService
-- Description : gRPC stubs for the Auth service
-- License     : MIT
--
-- Generated gRPC service stubs for authentication operations.
-- In production, these would be generated from proto files.

module Grey.GRPC.AuthService
  ( -- * Request Types
    LoginRequest(..)
  , LogoutRequest(..)
  , RefreshRequest(..)
    -- * Response Types
  , LoginResponse(..)
  , LogoutResponse(..)
  , RefreshResponse(..)
    -- * Service Methods
  , login
  , logout
  , refresh
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

import Grey.Config.Options (Options)
import Grey.Error.GreyError (GreyError)
import Grey.Error.Result (Result(..))
import Grey.GRPC.Client (GrpcClient, runGrpc, makeRequest)

-- | Login request message.
data LoginRequest = LoginRequest
  { lrUsername :: !Text
  , lrPassword :: !Text
  } deriving (Eq, Show, Generic)

-- | Login response message.
data LoginResponse = LoginResponse
  { lrAccessToken  :: !Text
  , lrRefreshToken :: !Text
  , lrExpiresIn    :: !Int
  } deriving (Eq, Show, Generic)

-- | Logout request message.
data LogoutRequest = LogoutRequest
  deriving (Eq, Show, Generic)

-- | Logout response message.
data LogoutResponse = LogoutResponse
  { lorSuccess :: !Bool
  } deriving (Eq, Show, Generic)

-- | Refresh request message.
data RefreshRequest = RefreshRequest
  { rrRefreshToken :: !Text
  } deriving (Eq, Show, Generic)

-- | Refresh response message.
data RefreshResponse = RefreshResponse
  { rrAccessToken  :: !Text
  , rrRefreshToken :: !Text
  , rrExpiresIn    :: !Int
  } deriving (Eq, Show, Generic)

-- | Call the Login RPC.
login :: GrpcClient -> LoginRequest -> IO (Result LoginResponse)
login client req = runGrpc client "grey.auth.AuthService" "Login" req

-- | Call the Logout RPC.
logout :: GrpcClient -> LogoutRequest -> IO (Result LogoutResponse)
logout client req = runGrpc client "grey.auth.AuthService" "Logout" req

-- | Call the Refresh RPC.
refresh :: GrpcClient -> RefreshRequest -> IO (Result RefreshResponse)
refresh client req = runGrpc client "grey.auth.AuthService" "Refresh" req
