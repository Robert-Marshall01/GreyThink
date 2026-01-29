package com.grey.sdk

import com.grey.sdk.config.GreyOptions
import com.grey.sdk.domain.*
import com.grey.sdk.grpc.*
import scala.concurrent.ExecutionContext

/** Main entry point for the Grey SDK.
  *
  * Provides access to all domain clients for interacting with the Grey API.
  *
  * Example:
  * {{{
  * import com.grey.sdk.*
  * import com.grey.sdk.config.GreyOptions
  * import scala.concurrent.ExecutionContext.Implicits.global
  *
  * val client = GreyClient(GreyOptions(host = "api.grey.example.com"))
  *
  * client.auth.login("user@example.com", "password").map { auth =>
  *   println(s"Logged in: ${auth.accessToken}")
  * }
  * }}}
  *
  * @param options
  *   Configuration options for the client
  */
final class GreyClient(val options: GreyOptions)(using ec: ExecutionContext):
  private val channel: GreyChannel = GreyChannel(options)

  // Initialize gRPC services
  private val authService: AuthGrpcService = AuthGrpcService(channel)
  private val userService: UserGrpcService = UserGrpcService(channel)
  private val projectsService: ProjectsGrpcService = ProjectsGrpcService(channel)
  private val queryService: QueryGrpcService = QueryGrpcService(channel)
  private val mutationService: MutationGrpcService = MutationGrpcService(channel)

  // Initialize domain clients
  private val _auth: AuthClient = AuthClient(channel, authService)
  private val _user: UserClient = UserClient(channel, userService)
  private val _projects: ProjectsClient = ProjectsClient(channel, projectsService)
  private val _query: QueryClient = QueryClient(channel, queryService)
  private val _mutation: MutationClient = MutationClient(channel, mutationService)

  /** Client for authentication operations (login, logout, refresh). */
  def auth: AuthClient = _auth

  /** Client for user operations (getUser). */
  def user: UserClient = _user

  /** Client for project operations (listProjects, createProject). */
  def projects: ProjectsClient = _projects

  /** Client for generic query operations. */
  def query: QueryClient = _query

  /** Client for generic mutation operations. */
  def mutation: MutationClient = _mutation

  /** Whether the client has a valid access token. */
  def isAuthenticated: Boolean = channel.isAuthenticated

  /** The current access token, if any. */
  def accessToken: Option[String] = channel.accessToken

  /** Sets the access token manually.
    *
    * Useful for restoring a session from stored credentials.
    */
  def setAccessToken(token: Option[String]): Unit =
    channel.setAccessToken(token)

  /** Gracefully shuts down the client and closes connections. */
  def shutdown(): Unit =
    _user.clearCache()
    channel.shutdown()

  /** Immediately terminates all connections. */
  def shutdownNow(): Unit =
    _user.clearCache()
    channel.shutdownNow()

object GreyClient:
  /** Creates a new Grey client with the specified options. */
  def apply(options: GreyOptions)(using ExecutionContext): GreyClient =
    new GreyClient(options)

  /** Creates a new Grey client for local development. */
  def local(port: Int = 50051)(using ExecutionContext): GreyClient =
    new GreyClient(GreyOptions.local(port))

  /** Creates a new Grey client for production. */
  def production(host: String)(using ExecutionContext): GreyClient =
    new GreyClient(GreyOptions.production(host))
