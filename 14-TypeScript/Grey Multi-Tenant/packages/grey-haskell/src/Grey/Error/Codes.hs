{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Grey.Error.Codes
-- Description : Standard error codes for the Grey SDK
-- License     : MIT
--
-- Standard error codes used throughout the Grey SDK for consistent
-- error handling across all domain operations.

module Grey.Error.Codes
  ( -- * Error Codes
    ErrorCode(..)
  , fromHttpStatus
  , fromGrpcStatus
  , isRetryable
  , errorMessage
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Standard error codes used throughout the Grey SDK.
data ErrorCode
  = Unauthorized      -- ^ Authentication required or token invalid
  | Forbidden         -- ^ Access denied to the requested resource
  | NotFound          -- ^ Requested resource was not found
  | ValidationError   -- ^ Request validation failed
  | NetworkError      -- ^ Network connectivity issue
  | Timeout           -- ^ Request timed out
  | ServerError       -- ^ Server returned an error
  | Unknown           -- ^ Unknown or unexpected error
  deriving (Eq, Show, Generic, Ord, Enum, Bounded)

-- | Convert an HTTP status code to an error code.
fromHttpStatus :: Int -> ErrorCode
fromHttpStatus status
  | status == 401           = Unauthorized
  | status == 403           = Forbidden
  | status == 404           = NotFound
  | status == 400           = ValidationError
  | status == 422           = ValidationError
  | status == 408           = Timeout
  | status >= 500 && status < 600 = ServerError
  | otherwise               = Unknown

-- | Convert a gRPC status code to an error code.
-- Uses standard gRPC status codes.
fromGrpcStatus :: Int -> ErrorCode
fromGrpcStatus status = case status of
  1  -> Unknown          -- CANCELLED
  2  -> Unknown          -- UNKNOWN
  3  -> ValidationError  -- INVALID_ARGUMENT
  4  -> Timeout          -- DEADLINE_EXCEEDED
  5  -> NotFound         -- NOT_FOUND
  6  -> ValidationError  -- ALREADY_EXISTS
  7  -> Forbidden        -- PERMISSION_DENIED
  8  -> ServerError      -- RESOURCE_EXHAUSTED
  9  -> ValidationError  -- FAILED_PRECONDITION
  10 -> Unknown          -- ABORTED
  11 -> ValidationError  -- OUT_OF_RANGE
  12 -> Unknown          -- UNIMPLEMENTED
  13 -> ServerError      -- INTERNAL
  14 -> NetworkError     -- UNAVAILABLE
  15 -> ServerError      -- DATA_LOSS
  16 -> Unauthorized     -- UNAUTHENTICATED
  _  -> Unknown

-- | Check if an error is retryable.
isRetryable :: ErrorCode -> Bool
isRetryable code = case code of
  NetworkError -> True
  Timeout      -> True
  ServerError  -> True
  _            -> False

-- | Get a human-readable message for an error code.
errorMessage :: ErrorCode -> Text
errorMessage code = case code of
  Unauthorized    -> "Authentication required or token invalid"
  Forbidden       -> "Access denied to the requested resource"
  NotFound        -> "Requested resource was not found"
  ValidationError -> "Request validation failed"
  NetworkError    -> "Network connectivity issue"
  Timeout         -> "Request timed out"
  ServerError     -> "Server returned an error"
  Unknown         -> "Unknown or unexpected error"
