{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Grey.GRPC.UserService
-- Description : gRPC stubs for the User service
-- License     : MIT
--
-- Generated gRPC service stubs for user operations.

module Grey.GRPC.UserService
  ( -- * Request Types
    GetUserRequest(..)
  , GetCurrentUserRequest(..)
    -- * Response Types
  , UserResponse(..)
    -- * Service Methods
  , getUser
  , getCurrentUser
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Grey.GRPC.Client (GrpcClient, runGrpc)
import Grey.Error.Result (Result)

-- | Get user request message.
data GetUserRequest = GetUserRequest
  { gurUserId :: !Text
  } deriving (Eq, Show, Generic)

-- | Get current user request message.
data GetCurrentUserRequest = GetCurrentUserRequest
  deriving (Eq, Show, Generic)

-- | User response message.
data UserResponse = UserResponse
  { urId          :: !Text
  , urUsername    :: !Text
  , urEmail       :: !Text
  , urDisplayName :: !Text
  , urRoles       :: ![Text]
  , urCreatedAt   :: !Text  -- ISO 8601 timestamp
  , urUpdatedAt   :: !Text  -- ISO 8601 timestamp
  } deriving (Eq, Show, Generic)

-- | Call the GetUser RPC.
getUser :: GrpcClient -> GetUserRequest -> IO (Result UserResponse)
getUser client req = runGrpc client "grey.user.UserService" "GetUser" req

-- | Call the GetCurrentUser RPC.
getCurrentUser :: GrpcClient -> GetCurrentUserRequest -> IO (Result UserResponse)
getCurrentUser client req = runGrpc client "grey.user.UserService" "GetCurrentUser" req
