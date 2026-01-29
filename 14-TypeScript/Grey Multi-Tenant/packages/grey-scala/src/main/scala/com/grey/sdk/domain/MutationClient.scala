package com.grey.sdk.domain

import com.grey.sdk.error.GreyError
import com.grey.sdk.grpc.{GreyChannel, MutationData, MutationGrpcService}
import scala.concurrent.{ExecutionContext, Future}

/** Client for generic mutation operations.
  *
  * Provides a flexible mutation interface for custom endpoints.
  */
final class MutationClient(
    channel: GreyChannel,
    service: MutationGrpcService
)(using ec: ExecutionContext):

  private val ValidMethods: Set[String] = Set("POST", "PUT", "PATCH", "DELETE")

  /** Executes a mutation against the specified endpoint.
    *
    * @param endpoint
    *   The mutation endpoint path
    * @param method
    *   The HTTP method equivalent (default: "POST")
    * @param body
    *   Optional request body
    * @return
    *   Future containing MutationData on success
    * @throws GreyError
    *   if not authenticated or on failure
    */
  def mutate(
      endpoint: String,
      method: String = "POST",
      body: Option[Any] = None
  ): Future[MutationData] =
    // Validate inputs
    if endpoint.isEmpty then
      return Future.failed(GreyError.validation("Endpoint is required"))

    val normalizedMethod = method.toUpperCase
    if !ValidMethods.contains(normalizedMethod) then
      return Future.failed(
        GreyError.validation(s"Method must be one of: ${ValidMethods.mkString(", ")}")
      )

    // Check authentication
    if !channel.isAuthenticated then
      return Future.failed(GreyError.unauthorized())

    service.mutate(endpoint, normalizedMethod, body)
      .recoverWith { case e => Future.failed(GreyError.fromThrowable(e)) }

  /** Executes mutation and returns Either instead of throwing. */
  def mutateSafe(
      endpoint: String,
      method: String = "POST",
      body: Option[Any] = None
  ): Future[Either[GreyError, MutationData]] =
    mutate(endpoint, method, body)
      .map(Right(_))
      .recover { case e: GreyError => Left(e) }

object MutationClient:
  def apply(channel: GreyChannel, service: MutationGrpcService)(using ExecutionContext): MutationClient =
    new MutationClient(channel, service)
