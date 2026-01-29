{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Grey.GRPC.MutationService
-- Description : gRPC stubs for the Mutation service
-- License     : MIT
--
-- Generated gRPC service stubs for mutation operations.

module Grey.GRPC.MutationService
  ( -- * Request Types
    MutationRequest(..)
  , BatchMutationRequest(..)
    -- * Response Types
  , MutationResponse(..)
  , MutationError(..)
  , BatchMutationResponse(..)
    -- * Service Methods
  , mutate
  , batchMutate
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Grey.GRPC.Client (GrpcClient, runGrpc)
import Grey.Error.Result (Result)

-- | Mutation request message.
data MutationRequest = MutationRequest
  { mrMutation      :: !Text
  , mrVariables     :: !(Map Text Text)  -- JSON-encoded values
  , mrOperationName :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

-- | Batch mutation request message.
data BatchMutationRequest = BatchMutationRequest
  { bmrMutations :: ![MutationRequest]
  } deriving (Eq, Show, Generic)

-- | Mutation error message.
data MutationError = MutationError
  { meMessage    :: !Text
  , mePath       :: ![Text]
  , meExtensions :: !(Map Text Text)
  } deriving (Eq, Show, Generic)

-- | Mutation response message.
data MutationResponse = MutationResponse
  { mrData       :: !Text  -- JSON-encoded data
  , mrErrors     :: ![MutationError]
  , mrExtensions :: !(Map Text Text)
  } deriving (Eq, Show, Generic)

-- | Batch mutation response message.
data BatchMutationResponse = BatchMutationResponse
  { bmrResults :: ![MutationResponse]
  } deriving (Eq, Show, Generic)

-- | Call the Mutate RPC.
mutate :: GrpcClient -> MutationRequest -> IO (Result MutationResponse)
mutate client req = runGrpc client "grey.mutation.MutationService" "Mutate" req

-- | Call the BatchMutate RPC.
batchMutate :: GrpcClient -> BatchMutationRequest -> IO (Result BatchMutationResponse)
batchMutate client req = runGrpc client "grey.mutation.MutationService" "BatchMutate" req
