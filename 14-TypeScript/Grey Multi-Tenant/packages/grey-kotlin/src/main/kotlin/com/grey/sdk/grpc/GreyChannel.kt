// =============================================================================
// Grey Multi-Tenant SDK - Kotlin
// grpc/GreyChannel.kt
//
// Shared gRPC channel management.
// =============================================================================

package com.grey.sdk.grpc

import com.grey.sdk.config.GreyOptions
import io.grpc.ManagedChannel
import io.grpc.ManagedChannelBuilder
import io.grpc.Metadata
import io.grpc.stub.MetadataUtils
import java.util.concurrent.TimeUnit

/**
 * Manages the gRPC channel for Grey SDK.
 */
class GreyChannel(private val options: GreyOptions) {

    private var channel: ManagedChannel? = null

    /**
     * Get or create the gRPC channel.
     */
    @Synchronized
    fun getChannel(): ManagedChannel {
        if (channel == null || channel!!.isShutdown) {
            channel = createChannel()
        }
        return channel!!
    }

    /**
     * Create a new gRPC channel with current options.
     */
    private fun createChannel(): ManagedChannel {
        val builder = ManagedChannelBuilder
            .forAddress(options.host, options.port)

        if (options.useTls) {
            builder.useTransportSecurity()
        } else {
            builder.usePlaintext()
        }

        return builder.build()
    }

    /**
     * Create metadata with authorization header.
     */
    fun createAuthMetadata(accessToken: String? = options.accessToken): Metadata {
        val metadata = Metadata()
        accessToken?.let {
            metadata.put(
                Metadata.Key.of("Authorization", Metadata.ASCII_STRING_MARSHALLER),
                "Bearer $it"
            )
        }
        return metadata
    }

    /**
     * Shutdown the channel.
     */
    @Synchronized
    fun shutdown() {
        channel?.shutdown()
        try {
            channel?.awaitTermination(5, TimeUnit.SECONDS)
        } catch (e: InterruptedException) {
            channel?.shutdownNow()
        }
        channel = null
    }

    /**
     * Check if the channel is active.
     */
    val isActive: Boolean
        get() = channel != null && !channel!!.isShutdown && !channel!!.isTerminated
}
