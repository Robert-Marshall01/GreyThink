package com.grey.sdk.grpc

import com.grey.sdk.config.GreyOptions
import io.grpc.{CallOptions, Channel, ManagedChannel, ManagedChannelBuilder, Metadata}
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference

/** Manages the gRPC channel connection for Grey SDK.
  *
  * Handles channel lifecycle, authentication metadata, and connection options.
  */
final class GreyChannel(val options: GreyOptions):
  private val channelRef: AtomicReference[ManagedChannel] = AtomicReference(createChannel())
  private val accessTokenRef: AtomicReference[Option[String]] = AtomicReference(None)

  /** Gets the underlying gRPC channel. */
  def channel: ManagedChannel = channelRef.get()

  /** Sets the access token for authenticated requests. */
  def setAccessToken(token: Option[String]): Unit =
    accessTokenRef.set(token)

  /** Gets the current access token. */
  def accessToken: Option[String] = accessTokenRef.get()

  /** Whether the client has a valid access token. */
  def isAuthenticated: Boolean = accessToken.isDefined

  /** Creates call options with timeout. */
  def callOptions: CallOptions =
    CallOptions.DEFAULT.withDeadlineAfter(options.timeoutMs, TimeUnit.MILLISECONDS)

  /** Builds metadata with authorization header. */
  def authMetadata: Metadata =
    val metadata = new Metadata()
    accessToken.foreach { token =>
      metadata.put(
        Metadata.Key.of("authorization", Metadata.ASCII_STRING_MARSHALLER),
        s"Bearer $token"
      )
    }
    metadata

  /** Creates metadata key for authorization. */
  val authorizationKey: Metadata.Key[String] =
    Metadata.Key.of("authorization", Metadata.ASCII_STRING_MARSHALLER)

  /** Gracefully shuts down the channel. */
  def shutdown(): Unit =
    val ch = channelRef.get()
    if ch != null then
      ch.shutdown()
      try ch.awaitTermination(5, TimeUnit.SECONDS)
      catch case _: InterruptedException => ch.shutdownNow()

  /** Immediately terminates the channel. */
  def shutdownNow(): Unit =
    val ch = channelRef.get()
    if ch != null then ch.shutdownNow()

  /** Creates a new managed channel based on options. */
  private def createChannel(): ManagedChannel =
    val builder = NettyChannelBuilder.forAddress(options.host, options.port)
    if options.useTls then
      builder.useTransportSecurity()
    else
      builder.usePlaintext()
    builder.build()

object GreyChannel:
  /** Creates a new channel with the given options. */
  def apply(options: GreyOptions): GreyChannel = new GreyChannel(options)
