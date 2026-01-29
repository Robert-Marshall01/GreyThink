{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Grey.Domain.MutationClient
-- Description : Domain client for mutation operations
-- License     : MIT
--
-- High-level mutation client with validation and error handling.

module Grey.Domain.MutationClient
  ( -- * Data Types
    MutationRequest(..)
  , MutationResponse(..)
  , MutationError(..)
  , BatchMutationResponse(..)
    -- * Operations
  , mutate
  , mutateSimple
  , batchMutate
    -- * Validation
  , validateMutation
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
import qualified Grey.GRPC.MutationService as GRPC

-- | Mutation request.
data MutationRequest = MutationRequest
  { mrMutation      :: !Text
  , mrVariables     :: !(Map Text Text)
  , mrOperationName :: !(Maybe Text)
  } deriving (Eq, Show)

-- | Mutation error.
data MutationError = MutationError
  { meMessage    :: !Text
  , mePath       :: ![Text]
  , meExtensions :: !(Map Text Text)
  } deriving (Eq, Show)

-- | Mutation response.
data MutationResponse = MutationResponse
  { mrData       :: !Text  -- JSON-encoded
  , mrErrors     :: ![MutationError]
  , mrExtensions :: !(Map Text Text)
  } deriving (Eq, Show)

-- | Batch mutation response.
data BatchMutationResponse = BatchMutationResponse
  { bmrResults :: ![MutationResponse]
  } deriving (Eq, Show)

-- | Convert gRPC error to domain type.
toMutationError :: GRPC.MutationError -> MutationError
toMutationError e = MutationError
  { meMessage = GRPC.meMessage e
  , mePath = GRPC.mePath e
  , meExtensions = GRPC.meExtensions e
  }

-- | Convert gRPC response to domain type.
toMutationResponse :: GRPC.MutationResponse -> MutationResponse
toMutationResponse resp = MutationResponse
  { mrData = GRPC.mrData resp
  , mrErrors = map toMutationError (GRPC.mrErrors resp)
  , mrExtensions = GRPC.mrExtensions resp
  }

-- | Convert batch response to domain type.
toBatchResponse :: GRPC.BatchMutationResponse -> BatchMutationResponse
toBatchResponse resp = BatchMutationResponse
  { bmrResults = map toMutationResponse (GRPC.bmrResults resp)
  }

-- | Validate a mutation request.
validateMutation :: MutationRequest -> Either GreyError MutationRequest
validateMutation req
  | T.null (T.strip (mrMutation req)) =
      Left $ GE.validationError "Mutation string is required" Nothing
  | otherwise =
      Right req

-- | Validate a batch of mutations.
validateBatch :: [MutationRequest] -> Either GreyError [MutationRequest]
validateBatch mutations
  | null mutations =
      Left $ GE.validationError "At least one mutation is required" Nothing
  | otherwise =
      case mapM validateMutation mutations of
        Left err -> Left err
        Right _ -> Right mutations

-- | Execute a mutation.
mutate :: GrpcClient -> MutationRequest -> IO (Result MutationResponse)
mutate client req = case validateMutation req of
  Left err -> return $ Err err
  Right validReq -> do
    let grpcReq = GRPC.MutationRequest
          { GRPC.mrMutation = mrMutation validReq
          , GRPC.mrVariables = mrVariables validReq
          , GRPC.mrOperationName = mrOperationName validReq
          }
    result <- GRPC.mutate client grpcReq
    return $ fmap toMutationResponse result

-- | Execute a simple mutation without variables.
mutateSimple :: GrpcClient -> Text -> IO (Result MutationResponse)
mutateSimple client mutationStr = mutate client MutationRequest
  { mrMutation = mutationStr
  , mrVariables = Map.empty
  , mrOperationName = Nothing
  }

-- | Execute multiple mutations in a batch.
batchMutate :: GrpcClient -> [MutationRequest] -> IO (Result BatchMutationResponse)
batchMutate client mutations = case validateBatch mutations of
  Left err -> return $ Err err
  Right validMutations -> do
    let grpcReqs = map (\m -> GRPC.MutationRequest
          { GRPC.mrMutation = mrMutation m
          , GRPC.mrVariables = mrVariables m
          , GRPC.mrOperationName = mrOperationName m
          }) validMutations
    let batchReq = GRPC.BatchMutationRequest { GRPC.bmrMutations = grpcReqs }
    result <- GRPC.batchMutate client batchReq
    return $ fmap toBatchResponse result
