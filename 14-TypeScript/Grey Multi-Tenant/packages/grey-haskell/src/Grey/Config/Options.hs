{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Grey.Config.Options
-- Description : Configuration options for the Grey SDK
-- License     : MIT
--
-- Provides configuration options for connecting to the Grey API.

module Grey.Config.Options
  ( -- * Options Type
    Options(..)
    -- * Constructors
  , defaultOptions
  , localOptions
  , productionOptions
    -- * Modifiers
  , withHost
  , withPort
  , withTls
  , withTimeout
  , withAuthToken
  , withHeader
    -- * Utilities
  , getEndpoint
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Configuration options for the Grey SDK client.
data Options = Options
  { optHost          :: !Text              -- ^ API host
  , optPort          :: !Int               -- ^ API port
  , optUseTls        :: !Bool              -- ^ Whether to use TLS
  , optTimeoutSecs   :: !Int               -- ^ Request timeout in seconds
  , optAuthToken     :: !(Maybe Text)      -- ^ Authentication token
  , optCustomHeaders :: !(Map Text Text)   -- ^ Custom headers
  } deriving (Eq, Show, Generic)

-- | Default options for local development.
defaultOptions :: Options
defaultOptions = Options
  { optHost = "localhost"
  , optPort = 50051
  , optUseTls = False
  , optTimeoutSecs = 30
  , optAuthToken = Nothing
  , optCustomHeaders = Map.empty
  }

-- | Options for local development with optional port.
localOptions :: Maybe Int -> Options
localOptions mPort = defaultOptions
  { optPort = maybe 50051 id mPort
  }

-- | Options for production with host and optional port.
productionOptions :: Text -> Maybe Int -> Options
productionOptions host mPort = Options
  { optHost = host
  , optPort = maybe 443 id mPort
  , optUseTls = True
  , optTimeoutSecs = 30
  , optAuthToken = Nothing
  , optCustomHeaders = Map.empty
  }

-- | Set the host.
withHost :: Text -> Options -> Options
withHost host opts = opts { optHost = host }

-- | Set the port.
withPort :: Int -> Options -> Options
withPort port opts = opts { optPort = port }

-- | Set whether to use TLS.
withTls :: Bool -> Options -> Options
withTls useTls opts = opts { optUseTls = useTls }

-- | Set the timeout in seconds.
withTimeout :: Int -> Options -> Options
withTimeout secs opts = opts { optTimeoutSecs = secs }

-- | Set the authentication token.
withAuthToken :: Text -> Options -> Options
withAuthToken token opts = opts { optAuthToken = Just token }

-- | Add a custom header.
withHeader :: Text -> Text -> Options -> Options
withHeader name value opts = opts
  { optCustomHeaders = Map.insert name value (optCustomHeaders opts)
  }

-- | Get the gRPC endpoint string.
getEndpoint :: Options -> Text
getEndpoint opts =
  optHost opts <> ":" <> T.pack (show (optPort opts))
