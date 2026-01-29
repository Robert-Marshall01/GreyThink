package com.grey.sdk.domain

import com.grey.sdk.error.GreyError
import com.grey.sdk.grpc.{GreyChannel, Project, ProjectsData, ProjectsGrpcService}
import scala.concurrent.{ExecutionContext, Future}

/** Client for project operations.
  *
  * Provides project listing and creation functionality.
  */
final class ProjectsClient(
    channel: GreyChannel,
    service: ProjectsGrpcService
)(using ec: ExecutionContext):

  /** Lists projects with pagination.
    *
    * @param page
    *   Page number (1-indexed)
    * @param pageSize
    *   Number of items per page
    * @return
    *   Future containing ProjectsData on success
    * @throws GreyError
    *   if not authenticated or on failure
    */
  def listProjects(page: Int = 1, pageSize: Int = 20): Future[ProjectsData] =
    if !channel.isAuthenticated then
      return Future.failed(GreyError.unauthorized())

    // Validate pagination
    if page < 1 then
      return Future.failed(GreyError.validation("Page must be >= 1"))
    if pageSize < 1 || pageSize > 100 then
      return Future.failed(GreyError.validation("Page size must be between 1 and 100"))

    service.listProjects(page, pageSize)
      .recoverWith { case e => Future.failed(GreyError.fromThrowable(e)) }

  /** Lists projects and returns Either instead of throwing. */
  def listProjectsSafe(page: Int = 1, pageSize: Int = 20): Future[Either[GreyError, ProjectsData]] =
    listProjects(page, pageSize)
      .map(Right(_))
      .recover { case e: GreyError => Left(e) }

  /** Creates a new project.
    *
    * @param name
    *   Project name (required)
    * @param description
    *   Optional project description
    * @return
    *   Future containing the created Project on success
    * @throws GreyError
    *   if not authenticated or on failure
    */
  def createProject(name: String, description: Option[String] = None): Future[Project] =
    if !channel.isAuthenticated then
      return Future.failed(GreyError.unauthorized())

    // Validate inputs
    if name.isEmpty then
      return Future.failed(GreyError.validation("Project name is required"))
    if name.length > 255 then
      return Future.failed(GreyError.validation("Project name must be <= 255 characters"))

    service.createProject(name, description)
      .recoverWith { case e => Future.failed(GreyError.fromThrowable(e)) }

  /** Creates project and returns Either instead of throwing. */
  def createProjectSafe(name: String, description: Option[String] = None): Future[Either[GreyError, Project]] =
    createProject(name, description)
      .map(Right(_))
      .recover { case e: GreyError => Left(e) }

object ProjectsClient:
  def apply(channel: GreyChannel, service: ProjectsGrpcService)(using ExecutionContext): ProjectsClient =
    new ProjectsClient(channel, service)
