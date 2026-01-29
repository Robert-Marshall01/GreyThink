{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Grey.GRPC.QueryService
-- Description : gRPC stubs for the Query service
-- License     : MIT
--
-- Generated gRPC service stubs for query operations.

module Grey.GRPC.QueryService
  ( -- * Request Types
    QueryRequest(..)
  , BatchQueryRequest(..)
    -- * Response Types
  , QueryResponse(..)
  , QueryError(..)
  , BatchQueryResponse(..)
    -- * Service Methods
  , query
  , batchQuery
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Grey.GRPC.Client (GrpcClient, runGrpc)
import Grey.Error.Result (Result)

-- | Query request message.
data QueryRequest = QueryRequest
  { qrQuery         :: !Text
  , qrVariables     :: !(Map Text Text)  -- JSON-encoded values
  , qrOperationName :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

-- | Batch query request message.
data BatchQueryRequest = BatchQueryRequest
  { bqrQueries :: ![QueryRequest]
  } deriving (Eq, Show, Generic)

-- | Query error message.
data QueryError = QueryError
  { qeMessage    :: !Text
  , qePath       :: ![Text]
  , qeExtensions :: !(Map Text Text)
  } deriving (Eq, Show, Generic)

-- | Query response message.
data QueryResponse = QueryResponse
  { qrData       :: !Text  -- JSON-encoded data
  , qrErrors     :: ![QueryError]
  , qrExtensions :: !(Map Text Text)
  } deriving (Eq, Show, Generic)

-- | Batch query response message.
data BatchQueryResponse = BatchQueryResponse
  { bqrResults :: ![QueryResponse]
  } deriving (Eq, Show, Generic)

-- | Call the Query RPC.
query :: GrpcClient -> QueryRequest -> IO (Result QueryResponse)
query client req = runGrpc client "grey.query.QueryService" "Query" req

-- | Call the BatchQuery RPC.
batchQuery :: GrpcClient -> BatchQueryRequest -> IO (Result BatchQueryResponse)
batchQuery client req = runGrpc client "grey.query.QueryService" "BatchQuery" req
