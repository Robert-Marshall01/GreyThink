{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Grey.Domain.ProjectsClient
-- Description : Domain client for project operations
-- License     : MIT
--
-- High-level projects client with validation and error handling.

module Grey.Domain.ProjectsClient
  ( -- * Data Types
    Project(..)
  , ProjectList(..)
  , ListOptions(..)
  , CreateProjectData(..)
  , UpdateProjectData(..)
    -- * Operations
  , listProjects
  , getProject
  , createProject
  , updateProject
  , deleteProject
    -- * Validation
  , validateProjectId
  , validateCreateData
  , validateListOptions
    -- * Defaults
  , defaultListOptions
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Grey.Error.GreyError (GreyError)
import qualified Grey.Error.GreyError as GE
import Grey.Error.Result (Result(..))
import Grey.GRPC.Client (GrpcClient)
import qualified Grey.GRPC.ProjectsService as GRPC

-- | Project domain type.
data Project = Project
  { projectId          :: !Text
  , projectName        :: !Text
  , projectDescription :: !(Maybe Text)
  , projectOwnerId     :: !Text
  , projectTenantId    :: !Text
  , projectCreatedAt   :: !Text
  , projectUpdatedAt   :: !Text
  , projectMetadata    :: !(Map Text Text)
  } deriving (Eq, Show)

-- | Paginated list of projects.
data ProjectList = ProjectList
  { plItems   :: ![Project]
  , plTotal   :: !Int
  , plPage    :: !Int
  , plPerPage :: !Int
  , plHasMore :: !Bool
  } deriving (Eq, Show)

-- | Options for listing projects.
data ListOptions = ListOptions
  { loPage     :: !Int
  , loPerPage  :: !Int
  , loSearch   :: !(Maybe Text)
  , loSortBy   :: !(Maybe Text)
  , loSortDesc :: !Bool
  } deriving (Eq, Show)

-- | Default list options.
defaultListOptions :: ListOptions
defaultListOptions = ListOptions
  { loPage = 1
  , loPerPage = 20
  , loSearch = Nothing
  , loSortBy = Nothing
  , loSortDesc = False
  }

-- | Data for creating a project.
data CreateProjectData = CreateProjectData
  { cpName        :: !Text
  , cpDescription :: !(Maybe Text)
  , cpMetadata    :: !(Map Text Text)
  } deriving (Eq, Show)

-- | Data for updating a project.
data UpdateProjectData = UpdateProjectData
  { upName        :: !(Maybe Text)
  , upDescription :: !(Maybe Text)
  , upMetadata    :: !(Map Text Text)
  } deriving (Eq, Show)

-- | Convert gRPC response to domain type.
toProject :: GRPC.ProjectResponse -> Project
toProject resp = Project
  { projectId = GRPC.prId resp
  , projectName = GRPC.prName resp
  , projectDescription = GRPC.prDescription resp
  , projectOwnerId = GRPC.prOwnerId resp
  , projectTenantId = GRPC.prTenantId resp
  , projectCreatedAt = GRPC.prCreatedAt resp
  , projectUpdatedAt = GRPC.prUpdatedAt resp
  , projectMetadata = GRPC.prMetadata resp
  }

-- | Convert gRPC list response to domain type.
toProjectList :: GRPC.ProjectListResponse -> ProjectList
toProjectList resp = ProjectList
  { plItems = map toProject (GRPC.plrItems resp)
  , plTotal = GRPC.plrTotal resp
  , plPage = GRPC.plrPage resp
  , plPerPage = GRPC.plrPerPage resp
  , plHasMore = GRPC.plrHasMore resp
  }

-- | Validate a project ID.
validateProjectId :: Text -> Either GreyError Text
validateProjectId pid
  | T.null (T.strip pid) =
      Left $ GE.validationError "Project ID is required" Nothing
  | otherwise =
      Right pid

-- | Validate create project data.
validateCreateData :: CreateProjectData -> Either GreyError CreateProjectData
validateCreateData d
  | T.null (T.strip (cpName d)) =
      Left $ GE.validationError "Project name is required" Nothing
  | otherwise =
      Right d

-- | Validate list options.
validateListOptions :: ListOptions -> Either GreyError ListOptions
validateListOptions opts
  | loPage opts < 1 =
      Left $ GE.validationError "Page must be at least 1" Nothing
  | loPerPage opts < 1 || loPerPage opts > 100 =
      Left $ GE.validationError "PerPage must be between 1 and 100" Nothing
  | otherwise =
      Right opts

-- | List projects with options.
listProjects :: GrpcClient -> ListOptions -> IO (Result ProjectList)
listProjects client opts = case validateListOptions opts of
  Left err -> return $ Err err
  Right validOpts -> do
    let req = GRPC.ListProjectsRequest
          { GRPC.lprPage = loPage validOpts
          , GRPC.lprPerPage = loPerPage validOpts
          , GRPC.lprSearch = loSearch validOpts
          , GRPC.lprSortBy = loSortBy validOpts
          , GRPC.lprSortDesc = loSortDesc validOpts
          }
    result <- GRPC.listProjects client req
    return $ fmap toProjectList result

-- | Get a project by ID.
getProject :: GrpcClient -> Text -> IO (Result Project)
getProject client pid = case validateProjectId pid of
  Left err -> return $ Err err
  Right validPid -> do
    let req = GRPC.GetProjectRequest { GRPC.gprProjectId = validPid }
    result <- GRPC.getProject client req
    return $ fmap toProject result

-- | Create a new project.
createProject :: GrpcClient -> CreateProjectData -> IO (Result Project)
createProject client d = case validateCreateData d of
  Left err -> return $ Err err
  Right validData -> do
    let req = GRPC.CreateProjectRequest
          { GRPC.cprName = cpName validData
          , GRPC.cprDescription = cpDescription validData
          , GRPC.cprMetadata = cpMetadata validData
          }
    result <- GRPC.createProject client req
    return $ fmap toProject result

-- | Update an existing project.
updateProject :: GrpcClient -> Text -> UpdateProjectData -> IO (Result Project)
updateProject client pid d = case validateProjectId pid of
  Left err -> return $ Err err
  Right validPid -> do
    let req = GRPC.UpdateProjectRequest
          { GRPC.uprProjectId = validPid
          , GRPC.uprName = upName d
          , GRPC.uprDescription = upDescription d
          , GRPC.uprMetadata = upMetadata d
          }
    result <- GRPC.updateProject client req
    return $ fmap toProject result

-- | Delete a project.
deleteProject :: GrpcClient -> Text -> IO (Result ())
deleteProject client pid = case validateProjectId pid of
  Left err -> return $ Err err
  Right validPid -> do
    let req = GRPC.DeleteProjectRequest { GRPC.dprProjectId = validPid }
    result <- GRPC.deleteProject client req
    return $ fmap (const ()) result
