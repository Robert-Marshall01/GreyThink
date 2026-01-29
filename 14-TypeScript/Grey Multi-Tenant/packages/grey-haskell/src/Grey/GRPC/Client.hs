{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Grey.GRPC.Client
-- Description : gRPC client wrapper for the Grey SDK
-- License     : MIT
--
-- Provides a gRPC client wrapper that handles connection management,
-- authentication, and error handling.

module Grey.GRPC.Client
  ( -- * Client Types
    GrpcClient(..)
  , ClientConfig(..)
    -- * Client Management
  , newClient
  , closeClient
  , withClient
    -- * Request Execution
  , runGrpc
  , makeRequest
    -- * Metadata
  , addMetadata
  , getAuthMetadata
  ) where

import Control.Exception (bracket, catch, SomeException)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Grey.Config.Options (Options(..), getEndpoint)
import Grey.Error.GreyError (GreyError)
import qualified Grey.Error.GreyError as GE
import Grey.Error.Result (Result(..))

-- | gRPC client configuration.
data ClientConfig = ClientConfig
  { cfgEndpoint  :: !Text
  , cfgUseTls    :: !Bool
  , cfgTimeout   :: !Int
  , cfgMetadata  :: !(Map Text Text)
  } deriving (Eq, Show)

-- | gRPC client handle.
data GrpcClient = GrpcClient
  { grpcConfig    :: !ClientConfig
  , grpcAuthToken :: !(Maybe Text)
  }

-- | Create a new gRPC client from options.
newClient :: Options -> IO GrpcClient
newClient opts = do
  let config = ClientConfig
        { cfgEndpoint = getEndpoint opts
        , cfgUseTls = optUseTls opts
        , cfgTimeout = optTimeoutSecs opts
        , cfgMetadata = optCustomHeaders opts
        }
  return GrpcClient
    { grpcConfig = config
    , grpcAuthToken = optAuthToken opts
    }

-- | Close a gRPC client.
closeClient :: GrpcClient -> IO ()
closeClient _ = return ()  -- Cleanup if needed

-- | Run an action with a gRPC client, ensuring cleanup.
withClient :: Options -> (GrpcClient -> IO a) -> IO a
withClient opts = bracket (newClient opts) closeClient

-- | Execute a gRPC call.
-- This is a simulation - in production, integrate with grpc-haskell.
runGrpc :: GrpcClient -> Text -> Text -> req -> IO (Result resp)
runGrpc client serviceName methodName req = do
  -- In production, this would:
  -- 1. Serialize the request using proto-lens
  -- 2. Add auth metadata if present
  -- 3. Make the actual gRPC call
  -- 4. Handle timeouts and errors
  -- 5. Deserialize the response
  --
  -- For now, return a network error to indicate not connected
  return $ Err $ GE.networkError (Just $ "gRPC call to " <> serviceName <> "/" <> methodName <> " - not connected")

-- | Create a gRPC request with metadata.
makeRequest :: GrpcClient -> Text -> Text -> req -> (Map Text Text, req)
makeRequest client serviceName methodName req =
  let metadata = addMetadata client Map.empty
  in (metadata, req)

-- | Add standard metadata to a request.
addMetadata :: GrpcClient -> Map Text Text -> Map Text Text
addMetadata client metadata =
  let withAuth = case grpcAuthToken client of
        Just token -> Map.insert "authorization" ("Bearer " <> token) metadata
        Nothing    -> metadata
  in Map.union withAuth (cfgMetadata (grpcConfig client))

-- | Get authentication metadata if token is present.
getAuthMetadata :: GrpcClient -> Map Text Text
getAuthMetadata client = case grpcAuthToken client of
  Just token -> Map.singleton "authorization" ("Bearer " <> token)
  Nothing    -> Map.empty
