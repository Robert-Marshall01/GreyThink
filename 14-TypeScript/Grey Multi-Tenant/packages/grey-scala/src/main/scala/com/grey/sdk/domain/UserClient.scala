package com.grey.sdk.domain

import com.grey.sdk.error.GreyError
import com.grey.sdk.grpc.{GreyChannel, User, UserGrpcService}
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.{ExecutionContext, Future}

/** Client for user operations.
  *
  * Provides user retrieval with optional caching.
  */
final class UserClient(
    channel: GreyChannel,
    service: UserGrpcService
)(using ec: ExecutionContext):

  private val cachedUser: AtomicReference[Option[User]] = AtomicReference(None)

  /** Gets the current authenticated user.
    *
    * Returns cached data unless forceRefresh is true.
    *
    * @param forceRefresh
    *   Whether to bypass cache and fetch fresh data
    * @return
    *   Future containing User on success
    * @throws GreyError
    *   if not authenticated or on failure
    */
  def getUser(forceRefresh: Boolean = false): Future[User] =
    if !channel.isAuthenticated then
      return Future.failed(GreyError.unauthorized())

    // Return cached if available and not forcing refresh
    if !forceRefresh then
      cachedUser.get() match
        case Some(user) => return Future.successful(user)
        case None       => // Continue to fetch

    service.getUser(forceRefresh).map { user =>
      cachedUser.set(Some(user))
      user
    }.recoverWith { case e => Future.failed(GreyError.fromThrowable(e)) }

  /** Gets user and returns Either instead of throwing. */
  def getUserSafe(forceRefresh: Boolean = false): Future[Either[GreyError, User]] =
    getUser(forceRefresh)
      .map(Right(_))
      .recover { case e: GreyError => Left(e) }

  /** Clears the cached user data. */
  def clearCache(): Unit = cachedUser.set(None)

  /** Gets the cached user without making a request. */
  def getCachedUser: Option[User] = cachedUser.get()

object UserClient:
  def apply(channel: GreyChannel, service: UserGrpcService)(using ExecutionContext): UserClient =
    new UserClient(channel, service)
