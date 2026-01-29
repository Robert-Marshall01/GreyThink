package com.grey.sdk.grpc

import io.grpc.Status
import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

/** gRPC service for project operations.
  *
  * This is a placeholder stub. In a real implementation, this would
  * use generated ScalaPB client stubs from .proto files.
  */
final class ProjectsGrpcService(channel: GreyChannel)(using ec: ExecutionContext):

  /** Lists projects via gRPC. */
  def listProjects(page: Int = 1, pageSize: Int = 20): Future[ProjectsData] =
    // In a real implementation:
    // val request = ListProjectsRequest(page = page, pageSize = pageSize)
    // stub.listProjects(request, channel.authMetadata)
    simulateAuthenticatedGrpcCall("projects.ListProjects") {
      ProjectsData(
        projects = List(
          Project(
            id = "project_1",
            name = "Stub Project 1",
            description = Some("A stub project for testing")
          ),
          Project(
            id = "project_2",
            name = "Stub Project 2"
          )
        ),
        total = 2,
        page = page,
        pageSize = pageSize
      )
    }

  /** Creates a project via gRPC. */
  def createProject(name: String, description: Option[String]): Future[Project] =
    // In a real implementation:
    // val request = CreateProjectRequest(name = name, description = description.getOrElse(""))
    // stub.createProject(request, channel.authMetadata)
    simulateAuthenticatedGrpcCall("projects.CreateProject") {
      Project(
        id = "new_project_id",
        name = name,
        description = description,
        createdAt = Some(Instant.now().toString)
      )
    }

  /** Simulates an authenticated gRPC call. */
  private def simulateAuthenticatedGrpcCall[T](method: String)(result: => T): Future[T] =
    if !channel.isAuthenticated then
      Future.failed(
        Status.UNAUTHENTICATED
          .withDescription("Not authenticated")
          .asRuntimeException()
      )
    else
      Future {
        Thread.sleep(1)
        result
      }

object ProjectsGrpcService:
  def apply(channel: GreyChannel)(using ExecutionContext): ProjectsGrpcService =
    new ProjectsGrpcService(channel)
