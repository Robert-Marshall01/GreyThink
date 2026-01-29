{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Grey.Error.GreyError
-- Description : Normalized error type for the Grey SDK
-- License     : MIT
--
-- Provides a normalized error type that contains code, message, and details
-- for consistent error handling.

module Grey.Error.GreyError
  ( -- * Error Type
    GreyError(..)
    -- * Constructors
  , mkError
  , unauthorized
  , forbidden
  , notFound
  , validationError
  , networkError
  , timeout
  , serverError
  , fromGrpcStatus
  , fromHttpStatus
    -- * Utilities
  , isRetryable
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Grey.Error.Codes (ErrorCode(..))
import qualified Grey.Error.Codes as Codes

-- | Normalized error type for the Grey SDK.
data GreyError = GreyError
  { errorCode    :: !ErrorCode   -- ^ The error code
  , errorMessage :: !Text        -- ^ Human-readable error message
  , errorDetails :: !(Maybe Text) -- ^ Optional additional details
  } deriving (Eq, Show, Generic)

-- | Create a new error with the given code, message, and optional details.
mkError :: ErrorCode -> Text -> Maybe Text -> GreyError
mkError = GreyError

-- | Create an unauthorized error.
unauthorized :: Maybe Text -> GreyError
unauthorized details = GreyError
  { errorCode = Unauthorized
  , errorMessage = Codes.errorMessage Unauthorized
  , errorDetails = details
  }

-- | Create a forbidden error.
forbidden :: Maybe Text -> GreyError
forbidden details = GreyError
  { errorCode = Forbidden
  , errorMessage = Codes.errorMessage Forbidden
  , errorDetails = details
  }

-- | Create a not found error.
notFound :: Maybe Text -> GreyError
notFound details = GreyError
  { errorCode = NotFound
  , errorMessage = Codes.errorMessage NotFound
  , errorDetails = details
  }

-- | Create a validation error.
validationError :: Text -> Maybe Text -> GreyError
validationError msg details = GreyError
  { errorCode = ValidationError
  , errorMessage = msg
  , errorDetails = details
  }

-- | Create a network error.
networkError :: Maybe Text -> GreyError
networkError details = GreyError
  { errorCode = NetworkError
  , errorMessage = Codes.errorMessage NetworkError
  , errorDetails = details
  }

-- | Create a timeout error.
timeout :: Maybe Text -> GreyError
timeout details = GreyError
  { errorCode = Timeout
  , errorMessage = Codes.errorMessage Timeout
  , errorDetails = details
  }

-- | Create a server error.
serverError :: Maybe Text -> GreyError
serverError details = GreyError
  { errorCode = ServerError
  , errorMessage = Codes.errorMessage ServerError
  , errorDetails = details
  }

-- | Create an error from a gRPC status code.
fromGrpcStatus :: Int -> Maybe Text -> GreyError
fromGrpcStatus status details =
  let code = Codes.fromGrpcStatus status
  in GreyError
    { errorCode = code
    , errorMessage = Codes.errorMessage code
    , errorDetails = details
    }

-- | Create an error from an HTTP status code.
fromHttpStatus :: Int -> Maybe Text -> GreyError
fromHttpStatus status details =
  let code = Codes.fromHttpStatus status
  in GreyError
    { errorCode = code
    , errorMessage = Codes.errorMessage code
    , errorDetails = details
    }

-- | Check if this error is retryable.
isRetryable :: GreyError -> Bool
isRetryable = Codes.isRetryable . errorCode
