{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Grey.Domain.QueryClient
-- Description : Domain client for query operations
-- License     : MIT
--
-- High-level query client with validation and error handling.

module Grey.Domain.QueryClient
  ( -- * Data Types
    QueryRequest(..)
  , QueryResponse(..)
  , QueryError(..)
  , BatchQueryResponse(..)
    -- * Operations
  , query
  , querySimple
  , batchQuery
    -- * Validation
  , validateQuery
  , validateBatch
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Grey.Error.GreyError (GreyError)
import qualified Grey.Error.GreyError as GE
import Grey.Error.Result (Result(..))
import Grey.GRPC.Client (GrpcClient)
import qualified Grey.GRPC.QueryService as GRPC

-- | Query request.
data QueryRequest = QueryRequest
  { qrQuery         :: !Text
  , qrVariables     :: !(Map Text Text)
  , qrOperationName :: !(Maybe Text)
  } deriving (Eq, Show)

-- | Query error.
data QueryError = QueryError
  { qeMessage    :: !Text
  , qePath       :: ![Text]
  , qeExtensions :: !(Map Text Text)
  } deriving (Eq, Show)

-- | Query response.
data QueryResponse = QueryResponse
  { qrData       :: !Text  -- JSON-encoded
  , qrErrors     :: ![QueryError]
  , qrExtensions :: !(Map Text Text)
  } deriving (Eq, Show)

-- | Batch query response.
data BatchQueryResponse = BatchQueryResponse
  { bqrResults :: ![QueryResponse]
  } deriving (Eq, Show)

-- | Convert gRPC error to domain type.
toQueryError :: GRPC.QueryError -> QueryError
toQueryError e = QueryError
  { qeMessage = GRPC.qeMessage e
  , qePath = GRPC.qePath e
  , qeExtensions = GRPC.qeExtensions e
  }

-- | Convert gRPC response to domain type.
toQueryResponse :: GRPC.QueryResponse -> QueryResponse
toQueryResponse resp = QueryResponse
  { qrData = GRPC.qrData resp
  , qrErrors = map toQueryError (GRPC.qrErrors resp)
  , qrExtensions = GRPC.qrExtensions resp
  }

-- | Convert batch response to domain type.
toBatchResponse :: GRPC.BatchQueryResponse -> BatchQueryResponse
toBatchResponse resp = BatchQueryResponse
  { bqrResults = map toQueryResponse (GRPC.bqrResults resp)
  }

-- | Validate a query request.
validateQuery :: QueryRequest -> Either GreyError QueryRequest
validateQuery req
  | T.null (T.strip (qrQuery req)) =
      Left $ GE.validationError "Query string is required" Nothing
  | otherwise =
      Right req

-- | Validate a batch of queries.
validateBatch :: [QueryRequest] -> Either GreyError [QueryRequest]
validateBatch queries
  | null queries =
      Left $ GE.validationError "At least one query is required" Nothing
  | otherwise =
      case mapM validateQuery queries of
        Left err -> Left err
        Right _ -> Right queries

-- | Execute a query.
query :: GrpcClient -> QueryRequest -> IO (Result QueryResponse)
query client req = case validateQuery req of
  Left err -> return $ Err err
  Right validReq -> do
    let grpcReq = GRPC.QueryRequest
          { GRPC.qrQuery = qrQuery validReq
          , GRPC.qrVariables = qrVariables validReq
          , GRPC.qrOperationName = qrOperationName validReq
          }
    result <- GRPC.query client grpcReq
    return $ fmap toQueryResponse result

-- | Execute a simple query without variables.
querySimple :: GrpcClient -> Text -> IO (Result QueryResponse)
querySimple client queryStr = query client QueryRequest
  { qrQuery = queryStr
  , qrVariables = Map.empty
  , qrOperationName = Nothing
  }

-- | Execute multiple queries in a batch.
batchQuery :: GrpcClient -> [QueryRequest] -> IO (Result BatchQueryResponse)
batchQuery client queries = case validateBatch queries of
  Left err -> return $ Err err
  Right validQueries -> do
    let grpcReqs = map (\q -> GRPC.QueryRequest
          { GRPC.qrQuery = qrQuery q
          , GRPC.qrVariables = qrVariables q
          , GRPC.qrOperationName = qrOperationName q
          }) validQueries
    let batchReq = GRPC.BatchQueryRequest { GRPC.bqrQueries = grpcReqs }
    result <- GRPC.batchQuery client batchReq
    return $ fmap toBatchResponse result
