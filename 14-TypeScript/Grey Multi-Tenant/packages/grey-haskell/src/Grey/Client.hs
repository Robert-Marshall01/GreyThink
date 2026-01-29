{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Grey.Client
-- Description : Main Grey SDK client
-- License     : MIT
--
-- The main client facade providing access to all Grey API operations.

module Grey.Client
  ( -- * Client Type
    GreyClient(..)
    -- * Client Creation
  , newClient
  , localClient
  , productionClient
  , closeClient
  , withClient
    -- * Configuration
  , withAuthToken
  , withTimeout
  , withHeader
    -- * Re-exports
  , module Grey.Config.Options
  , module Grey.Error.Result
  , module Grey.Error.GreyError
  , module Grey.Error.Codes
  ) where

import Control.Exception (bracket)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Grey.Config.Options
import Grey.Error.Codes
import Grey.Error.GreyError
import Grey.Error.Result
import Grey.GRPC.Client (GrpcClient)
import qualified Grey.GRPC.Client as GRPC

-- Domain clients
import qualified Grey.Domain.AuthClient as Auth
import qualified Grey.Domain.UserClient as Users
import qualified Grey.Domain.ProjectsClient as Projects
import qualified Grey.Domain.QueryClient as Query
import qualified Grey.Domain.MutationClient as Mutation

-- | Grey SDK client providing access to all API operations.
data GreyClient = GreyClient
  { clientOptions :: !Options
  , grpcClient    :: !GrpcClient
  }

-- | Create a new client with the given options.
newClient :: Options -> IO GreyClient
newClient opts = do
  grpc <- GRPC.newClient opts
  return GreyClient
    { clientOptions = opts
    , grpcClient = grpc
    }

-- | Create a client for local development.
localClient :: Maybe Int -> IO GreyClient
localClient mPort = newClient (localOptions mPort)

-- | Create a client for production.
productionClient :: Text -> Maybe Int -> IO GreyClient
productionClient host mPort = newClient (productionOptions host mPort)

-- | Close the client and release resources.
closeClient :: GreyClient -> IO ()
closeClient client = GRPC.closeClient (grpcClient client)

-- | Run an action with a client, ensuring cleanup.
withClient :: Options -> (GreyClient -> IO a) -> IO a
withClient opts = bracket (newClient opts) closeClient

-- | Create a new client with an auth token.
withAuthToken :: Text -> GreyClient -> IO GreyClient
withAuthToken token client = do
  let newOpts = Grey.Config.Options.withAuthToken token (clientOptions client)
  newClient newOpts

-- | Create a new client with a timeout.
withTimeout :: Int -> GreyClient -> IO GreyClient
withTimeout secs client = do
  let newOpts = Grey.Config.Options.withTimeout secs (clientOptions client)
  newClient newOpts

-- | Create a new client with an additional header.
withHeader :: Text -> Text -> GreyClient -> IO GreyClient
withHeader name value client = do
  let newOpts = Grey.Config.Options.withHeader name value (clientOptions client)
  newClient newOpts
