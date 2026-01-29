package com.grey.sdk.error

import io.grpc.{Status, StatusException, StatusRuntimeException}

/** Normalized error shape for all Grey SDK operations.
  *
  * @param code
  *   Error code identifying the type of error
  * @param message
  *   Human-readable error message
  * @param details
  *   Optional additional error details
  */
final case class GreyError(
    code: String,
    message: String,
    details: Option[Map[String, Any]] = None
) extends Exception(s"[$code] $message")

object GreyError:
  // Common error codes
  val Unauthorized: String = "UNAUTHORIZED"
  val Forbidden: String = "FORBIDDEN"
  val NotFound: String = "NOT_FOUND"
  val ValidationError: String = "VALIDATION_ERROR"
  val NetworkError: String = "NETWORK_ERROR"
  val Timeout: String = "TIMEOUT"
  val ServerError: String = "SERVER_ERROR"
  val Unknown: String = "UNKNOWN"

  /** Creates a GreyError from a gRPC StatusException. */
  def fromGrpcException(e: StatusException): GreyError =
    fromGrpcStatus(e.getStatus, Option(e.getTrailers).map(_.toString))

  /** Creates a GreyError from a gRPC StatusRuntimeException. */
  def fromGrpcRuntimeException(e: StatusRuntimeException): GreyError =
    fromGrpcStatus(e.getStatus, Option(e.getTrailers).map(_.toString))

  /** Creates a GreyError from a gRPC Status. */
  def fromGrpcStatus(status: Status, trailers: Option[String] = None): GreyError =
    val code = grpcStatusToCode(status.getCode)
    val message = Option(status.getDescription).getOrElse(status.getCode.name)
    val details = trailers.map(t => Map[String, Any]("grpcTrailers" -> t))
    GreyError(code, message, details)

  /** Creates a GreyError from any throwable. */
  def fromThrowable(e: Throwable): GreyError = e match
    case ge: GreyError              => ge
    case se: StatusException        => fromGrpcException(se)
    case sre: StatusRuntimeException => fromGrpcRuntimeException(sre)
    case other                      => GreyError(Unknown, other.getMessage)

  /** Maps gRPC status codes to Grey error codes. */
  private def grpcStatusToCode(grpcCode: Status.Code): String = grpcCode match
    case Status.Code.UNAUTHENTICATED    => Unauthorized
    case Status.Code.PERMISSION_DENIED  => Forbidden
    case Status.Code.NOT_FOUND          => NotFound
    case Status.Code.INVALID_ARGUMENT   => ValidationError
    case Status.Code.FAILED_PRECONDITION => ValidationError
    case Status.Code.UNAVAILABLE        => NetworkError
    case Status.Code.DEADLINE_EXCEEDED  => Timeout
    case Status.Code.INTERNAL           => ServerError
    case Status.Code.UNKNOWN            => Unknown
    case _                              => Unknown

  /** Creates an unauthorized error. */
  def unauthorized(message: String = "Not authenticated"): GreyError =
    GreyError(Unauthorized, message)

  /** Creates a validation error. */
  def validation(message: String): GreyError =
    GreyError(ValidationError, message)

/** Result type for operations that can fail.
  *
  * Provides a functional alternative to throwing exceptions.
  */
enum GreyResult[+A]:
  case Success(value: A)
  case Failure(error: GreyError)

  /** Whether this result is a success. */
  def isSuccess: Boolean = this match
    case Success(_) => true
    case Failure(_) => false

  /** Whether this result is a failure. */
  def isFailure: Boolean = !isSuccess

  /** Gets the value if success, otherwise throws. */
  def get: A = this match
    case Success(v) => v
    case Failure(e) => throw e

  /** Gets the value wrapped in Option. */
  def toOption: Option[A] = this match
    case Success(v) => Some(v)
    case Failure(_) => None

  /** Gets the error wrapped in Option. */
  def errorOption: Option[GreyError] = this match
    case Success(_) => None
    case Failure(e) => Some(e)

  /** Converts to Either. */
  def toEither: Either[GreyError, A] = this match
    case Success(v) => Right(v)
    case Failure(e) => Left(e)

  /** Maps the success value. */
  def map[B](f: A => B): GreyResult[B] = this match
    case Success(v) => Success(f(v))
    case Failure(e) => Failure(e)

  /** Flat maps the success value. */
  def flatMap[B](f: A => GreyResult[B]): GreyResult[B] = this match
    case Success(v) => f(v)
    case Failure(e) => Failure(e)

  /** Folds the result. */
  def fold[B](onFailure: GreyError => B, onSuccess: A => B): B = this match
    case Success(v) => onSuccess(v)
    case Failure(e) => onFailure(e)

  /** Recovers from a failure with a fallback value. */
  def recover[B >: A](pf: PartialFunction[GreyError, B]): GreyResult[B] = this match
    case Failure(e) if pf.isDefinedAt(e) => Success(pf(e))
    case other                           => other

object GreyResult:
  /** Creates a success result. */
  def success[A](value: A): GreyResult[A] = Success(value)

  /** Creates a failure result. */
  def failure[A](error: GreyError): GreyResult[A] = Failure(error)

  /** Creates a result from an Either. */
  def fromEither[A](either: Either[GreyError, A]): GreyResult[A] = either match
    case Right(v) => Success(v)
    case Left(e)  => Failure(e)

  /** Creates a result from a Try. */
  def fromTry[A](t: scala.util.Try[A]): GreyResult[A] = t match
    case scala.util.Success(v) => Success(v)
    case scala.util.Failure(e) => Failure(GreyError.fromThrowable(e))
