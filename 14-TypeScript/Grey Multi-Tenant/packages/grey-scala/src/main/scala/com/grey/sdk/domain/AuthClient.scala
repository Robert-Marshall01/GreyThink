package com.grey.sdk.domain

import com.grey.sdk.error.{GreyError, GreyResult}
import com.grey.sdk.grpc.{AuthData, AuthGrpcService, GreyChannel}
import io.grpc.{StatusException, StatusRuntimeException}
import scala.concurrent.{ExecutionContext, Future}

/** Client for authentication operations.
  *
  * Provides login, logout, and token refresh functionality.
  */
final class AuthClient(
    channel: GreyChannel,
    service: AuthGrpcService
)(using ec: ExecutionContext):

  /** Authenticates a user with email and password.
    *
    * On success, stores the access token for subsequent authenticated requests.
    *
    * @param email
    *   The user's email address
    * @param password
    *   The user's password
    * @return
    *   Future containing AuthData on success
    * @throws GreyError
    *   on failure
    */
  def login(email: String, password: String): Future[AuthData] =
    // Validate inputs
    if email.isEmpty then
      return Future.failed(GreyError.validation("Email is required"))
    if password.isEmpty then
      return Future.failed(GreyError.validation("Password is required"))

    service.login(email, password).map { result =>
      channel.setAccessToken(Some(result.accessToken))
      result
    }.recoverWith { case e => Future.failed(GreyError.fromThrowable(e)) }

  /** Authenticates and returns Either instead of throwing.
    *
    * @return
    *   Future containing Either[GreyError, AuthData]
    */
  def loginSafe(email: String, password: String): Future[Either[GreyError, AuthData]] =
    login(email, password)
      .map(Right(_))
      .recover { case e: GreyError => Left(e) }

  /** Logs out the current user.
    *
    * Clears the stored access token.
    *
    * @return
    *   Future completing on success
    * @throws GreyError
    *   on failure
    */
  def logout(): Future[Unit] =
    service.logout()
      .map { _ => channel.setAccessToken(None) }
      .recoverWith { case e =>
        channel.setAccessToken(None) // Always clear token
        Future.failed(GreyError.fromThrowable(e))
      }

  /** Logs out and returns Either instead of throwing. */
  def logoutSafe(): Future[Either[GreyError, Unit]] =
    logout()
      .map(Right(_))
      .recover { case e: GreyError => Left(e) }

  /** Refreshes the authentication tokens.
    *
    * @param refreshToken
    *   Optional refresh token; uses last known if not provided
    * @return
    *   Future containing new AuthData on success
    * @throws GreyError
    *   on failure
    */
  def refresh(refreshToken: Option[String] = None): Future[AuthData] =
    refreshToken match
      case Some(token) if token.nonEmpty =>
        service.refresh(token).map { result =>
          channel.setAccessToken(Some(result.accessToken))
          result
        }.recoverWith { case e => Future.failed(GreyError.fromThrowable(e)) }
      case _ =>
        Future.failed(GreyError.validation("Refresh token is required"))

  /** Refreshes and returns Either instead of throwing. */
  def refreshSafe(refreshToken: Option[String] = None): Future[Either[GreyError, AuthData]] =
    refresh(refreshToken)
      .map(Right(_))
      .recover { case e: GreyError => Left(e) }

  /** Whether the client has a valid access token. */
  def isAuthenticated: Boolean = channel.isAuthenticated

object AuthClient:
  def apply(channel: GreyChannel, service: AuthGrpcService)(using ExecutionContext): AuthClient =
    new AuthClient(channel, service)
