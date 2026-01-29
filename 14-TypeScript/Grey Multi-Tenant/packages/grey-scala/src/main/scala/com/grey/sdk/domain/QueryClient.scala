package com.grey.sdk.domain

import com.grey.sdk.error.GreyError
import com.grey.sdk.grpc.{GreyChannel, QueryData, QueryGrpcService}
import scala.concurrent.{ExecutionContext, Future}

/** Client for generic query operations.
  *
  * Provides a flexible query interface for custom endpoints.
  */
final class QueryClient(
    channel: GreyChannel,
    service: QueryGrpcService
)(using ec: ExecutionContext):

  /** Executes a query against the specified endpoint.
    *
    * @param endpoint
    *   The query endpoint path
    * @param params
    *   Optional query parameters
    * @param requireAuth
    *   Whether authentication is required (default: true)
    * @return
    *   Future containing QueryData on success
    * @throws GreyError
    *   on failure
    */
  def query(
      endpoint: String,
      params: Map[String, String] = Map.empty,
      requireAuth: Boolean = true
  ): Future[QueryData] =
    // Validate inputs
    if endpoint.isEmpty then
      return Future.failed(GreyError.validation("Endpoint is required"))

    // Check authentication if required
    if requireAuth && !channel.isAuthenticated then
      return Future.failed(GreyError.unauthorized())

    service.query(endpoint, params, requireAuth)
      .recoverWith { case e => Future.failed(GreyError.fromThrowable(e)) }

  /** Executes query and returns Either instead of throwing. */
  def querySafe(
      endpoint: String,
      params: Map[String, String] = Map.empty,
      requireAuth: Boolean = true
  ): Future[Either[GreyError, QueryData]] =
    query(endpoint, params, requireAuth)
      .map(Right(_))
      .recover { case e: GreyError => Left(e) }

object QueryClient:
  def apply(channel: GreyChannel, service: QueryGrpcService)(using ExecutionContext): QueryClient =
    new QueryClient(channel, service)
