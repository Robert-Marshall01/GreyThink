package com.grey.sdk.config

import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import io.grpc.netty.shaded.io.grpc.netty.{GrpcSslContexts, NettyChannelBuilder}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

/** Configuration options for Grey SDK client.
  *
  * @param host
  *   The host address of the Grey API server
  * @param port
  *   The port to connect to (default: 443)
  * @param useTls
  *   Whether to use TLS for the connection (default: true)
  * @param timeoutMs
  *   Request timeout in milliseconds (default: 30000)
  */
final case class GreyOptions(
    host: String,
    port: Int = 443,
    useTls: Boolean = true,
    timeoutMs: Long = 30000
):
  require(host.nonEmpty, "Host must not be empty")
  require(port > 0 && port <= 65535, "Port must be between 1 and 65535")
  require(timeoutMs > 0, "Timeout must be positive")

  /** Returns timeout as a Duration. */
  def timeout: Duration = Duration(timeoutMs, TimeUnit.MILLISECONDS)

  /** Creates a copy with modified settings. */
  def withHost(newHost: String): GreyOptions = copy(host = newHost)
  def withPort(newPort: Int): GreyOptions = copy(port = newPort)
  def withTls(enabled: Boolean): GreyOptions = copy(useTls = enabled)
  def withTimeout(ms: Long): GreyOptions = copy(timeoutMs = ms)

object GreyOptions:
  /** Creates default options for local development. */
  def local(port: Int = 50051): GreyOptions =
    GreyOptions(host = "localhost", port = port, useTls = false)

  /** Creates default options for production. */
  def production(host: String): GreyOptions =
    GreyOptions(host = host, port = 443, useTls = true)
