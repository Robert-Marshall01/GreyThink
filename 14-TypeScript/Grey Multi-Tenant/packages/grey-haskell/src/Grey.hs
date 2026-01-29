-- |
-- Module      : Grey
-- Description : Grey Multi-Tenant SDK for Haskell
-- License     : MIT
-- Stability   : experimental
--
-- The Grey SDK provides a pure functional interface to the Grey Multi-Tenant
-- API using gRPC transport.
--
-- = Quick Start
--
-- @
-- import Grey
--
-- main :: IO ()
-- main = do
--   -- Create a local client
--   client <- localClient Nothing
--
--   -- Login
--   result <- Auth.login (grpcClient client) credentials
--   case result of
--     Ok tokens -> putStrLn $ "Logged in: " ++ show (accessToken tokens)
--     Err e -> putStrLn $ "Error: " ++ show e
--
--   -- Cleanup
--   closeClient client
-- @
--
-- = Error Handling
--
-- All operations return a 'Result' type which is either 'Ok' with a value
-- or 'Err' with a 'GreyError'.
--
-- @
-- result <- Users.getUser grpc "user-123"
-- case result of
--   Ok user -> print user
--   Err e -> do
--     putStrLn $ "Error code: " ++ show (errorCode e)
--     putStrLn $ "Message: " ++ show (errorMessage e)
--     when (isRetryable e) $ putStrLn "This error is retryable"
-- @

module Grey
  ( -- * Client
    GreyClient(..)
  , newClient
  , localClient
  , productionClient
  , closeClient
  , withClient
  , withAuthToken
  , withTimeout
  , withHeader

    -- * Configuration
  , Options(..)
  , defaultOptions
  , localOptions
  , productionOptions

    -- * Error Handling
  , Result(..)
  , ok
  , err
  , isOk
  , isErr
  , fromResult
  , getOr
  , mapResult
  , mapError
  , flatMap

    -- * Error Types
  , GreyError(..)
  , ErrorCode(..)
  , isRetryable

    -- * Domain Clients
  , module Grey.Domain.AuthClient
  , module Grey.Domain.UserClient
  , module Grey.Domain.ProjectsClient
  , module Grey.Domain.QueryClient
  , module Grey.Domain.MutationClient

    -- * gRPC Client
  , GrpcClient
  , grpcClient
  ) where

import Grey.Client
import Grey.Config.Options
import Grey.Error.Codes (ErrorCode(..))
import Grey.Error.GreyError (GreyError(..), isRetryable)
import Grey.Error.Result
import Grey.GRPC.Client (GrpcClient)

import Grey.Domain.AuthClient
import Grey.Domain.UserClient
import Grey.Domain.ProjectsClient
import Grey.Domain.QueryClient
import Grey.Domain.MutationClient
