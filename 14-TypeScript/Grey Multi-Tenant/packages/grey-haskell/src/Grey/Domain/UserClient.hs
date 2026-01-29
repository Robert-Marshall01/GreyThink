{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Grey.Domain.UserClient
-- Description : Domain client for user operations
-- License     : MIT
--
-- High-level user client with validation and error handling.

module Grey.Domain.UserClient
  ( -- * Data Types
    User(..)
    -- * Operations
  , getUser
  , getCurrentUser
    -- * Validation
  , validateUserId
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Grey.Error.GreyError (GreyError)
import qualified Grey.Error.GreyError as GE
import Grey.Error.Result (Result(..))
import Grey.GRPC.Client (GrpcClient)
import qualified Grey.GRPC.UserService as GRPC

-- | User domain type.
data User = User
  { userId      :: !Text
  , userName    :: !Text
  , userEmail   :: !Text
  , displayName :: !Text
  , roles       :: ![Text]
  , createdAt   :: !Text
  , updatedAt   :: !Text
  } deriving (Eq, Show)

-- | Convert gRPC response to domain type.
toUser :: GRPC.UserResponse -> User
toUser resp = User
  { userId = GRPC.urId resp
  , userName = GRPC.urUsername resp
  , userEmail = GRPC.urEmail resp
  , displayName = GRPC.urDisplayName resp
  , roles = GRPC.urRoles resp
  , createdAt = GRPC.urCreatedAt resp
  , updatedAt = GRPC.urUpdatedAt resp
  }

-- | Validate a user ID.
validateUserId :: Text -> Either GreyError Text
validateUserId uid
  | T.null (T.strip uid) =
      Left $ GE.validationError "User ID is required" Nothing
  | otherwise =
      Right uid

-- | Get a user by ID.
getUser :: GrpcClient -> Text -> IO (Result User)
getUser client uid = case validateUserId uid of
  Left err -> return $ Err err
  Right validUid -> do
    let req = GRPC.GetUserRequest { GRPC.gurUserId = validUid }
    result <- GRPC.getUser client req
    return $ fmap toUser result

-- | Get the currently authenticated user.
getCurrentUser :: GrpcClient -> IO (Result User)
getCurrentUser client = do
  result <- GRPC.getCurrentUser client GRPC.GetCurrentUserRequest
  return $ fmap toUser result
