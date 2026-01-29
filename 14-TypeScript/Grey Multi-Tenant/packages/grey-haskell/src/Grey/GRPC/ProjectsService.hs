{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Grey.GRPC.ProjectsService
-- Description : gRPC stubs for the Projects service
-- License     : MIT
--
-- Generated gRPC service stubs for project operations.

module Grey.GRPC.ProjectsService
  ( -- * Request Types
    ListProjectsRequest(..)
  , GetProjectRequest(..)
  , CreateProjectRequest(..)
  , UpdateProjectRequest(..)
  , DeleteProjectRequest(..)
    -- * Response Types
  , ProjectResponse(..)
  , ProjectListResponse(..)
  , DeleteProjectResponse(..)
    -- * Service Methods
  , listProjects
  , getProject
  , createProject
  , updateProject
  , deleteProject
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Grey.GRPC.Client (GrpcClient, runGrpc)
import Grey.Error.Result (Result)

-- | List projects request message.
data ListProjectsRequest = ListProjectsRequest
  { lprPage     :: !Int
  , lprPerPage  :: !Int
  , lprSearch   :: !(Maybe Text)
  , lprSortBy   :: !(Maybe Text)
  , lprSortDesc :: !Bool
  } deriving (Eq, Show, Generic)

-- | Get project request message.
data GetProjectRequest = GetProjectRequest
  { gprProjectId :: !Text
  } deriving (Eq, Show, Generic)

-- | Create project request message.
data CreateProjectRequest = CreateProjectRequest
  { cprName        :: !Text
  , cprDescription :: !(Maybe Text)
  , cprMetadata    :: !(Map Text Text)
  } deriving (Eq, Show, Generic)

-- | Update project request message.
data UpdateProjectRequest = UpdateProjectRequest
  { uprProjectId   :: !Text
  , uprName        :: !(Maybe Text)
  , uprDescription :: !(Maybe Text)
  , uprMetadata    :: !(Map Text Text)
  } deriving (Eq, Show, Generic)

-- | Delete project request message.
data DeleteProjectRequest = DeleteProjectRequest
  { dprProjectId :: !Text
  } deriving (Eq, Show, Generic)

-- | Project response message.
data ProjectResponse = ProjectResponse
  { prId          :: !Text
  , prName        :: !Text
  , prDescription :: !(Maybe Text)
  , prOwnerId     :: !Text
  , prTenantId    :: !Text
  , prCreatedAt   :: !Text
  , prUpdatedAt   :: !Text
  , prMetadata    :: !(Map Text Text)
  } deriving (Eq, Show, Generic)

-- | Project list response message.
data ProjectListResponse = ProjectListResponse
  { plrItems   :: ![ProjectResponse]
  , plrTotal   :: !Int
  , plrPage    :: !Int
  , plrPerPage :: !Int
  , plrHasMore :: !Bool
  } deriving (Eq, Show, Generic)

-- | Delete project response message.
data DeleteProjectResponse = DeleteProjectResponse
  { dprSuccess :: !Bool
  } deriving (Eq, Show, Generic)

-- | Call the ListProjects RPC.
listProjects :: GrpcClient -> ListProjectsRequest -> IO (Result ProjectListResponse)
listProjects client req = runGrpc client "grey.projects.ProjectsService" "ListProjects" req

-- | Call the GetProject RPC.
getProject :: GrpcClient -> GetProjectRequest -> IO (Result ProjectResponse)
getProject client req = runGrpc client "grey.projects.ProjectsService" "GetProject" req

-- | Call the CreateProject RPC.
createProject :: GrpcClient -> CreateProjectRequest -> IO (Result ProjectResponse)
createProject client req = runGrpc client "grey.projects.ProjectsService" "CreateProject" req

-- | Call the UpdateProject RPC.
updateProject :: GrpcClient -> UpdateProjectRequest -> IO (Result ProjectResponse)
updateProject client req = runGrpc client "grey.projects.ProjectsService" "UpdateProject" req

-- | Call the DeleteProject RPC.
deleteProject :: GrpcClient -> DeleteProjectRequest -> IO (Result DeleteProjectResponse)
deleteProject client req = runGrpc client "grey.projects.ProjectsService" "DeleteProject" req
