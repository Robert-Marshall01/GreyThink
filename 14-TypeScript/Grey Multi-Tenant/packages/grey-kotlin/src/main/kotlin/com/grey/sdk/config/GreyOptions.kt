// =============================================================================
// Grey Multi-Tenant SDK - Kotlin
// config/GreyOptions.kt
//
// Configuration options for the Grey SDK.
// =============================================================================

package com.grey.sdk.config

import java.util.concurrent.TimeUnit

/**
 * Configuration options for the Grey SDK.
 *
 * @property host The gRPC server host
 * @property port The gRPC server port
 * @property useTls Whether to use TLS (default: true)
 * @property timeoutMs Request timeout in milliseconds (default: 30000)
 * @property accessToken Optional access token for authenticated requests
 */
data class GreyOptions(
    val host: String,
    val port: Int = 443,
    val useTls: Boolean = true,
    val timeoutMs: Long = 30000,
    val accessToken: String? = null
) {
    /**
     * Builder for GreyOptions.
     */
    class Builder {
        private var host: String = "localhost"
        private var port: Int = 443
        private var useTls: Boolean = true
        private var timeoutMs: Long = 30000
        private var accessToken: String? = null

        fun host(host: String) = apply { this.host = host }
        fun port(port: Int) = apply { this.port = port }
        fun useTls(useTls: Boolean) = apply { this.useTls = useTls }
        fun timeoutMs(timeoutMs: Long) = apply { this.timeoutMs = timeoutMs }
        fun timeout(timeout: Long, unit: TimeUnit) = apply { this.timeoutMs = unit.toMillis(timeout) }
        fun accessToken(accessToken: String?) = apply { this.accessToken = accessToken }

        fun build(): GreyOptions = GreyOptions(
            host = host,
            port = port,
            useTls = useTls,
            timeoutMs = timeoutMs,
            accessToken = accessToken
        )
    }

    companion object {
        /**
         * Create a new builder.
         */
        fun builder(): Builder = Builder()

        /**
         * Create options for local development.
         */
        fun forLocalDev(port: Int = 50051): GreyOptions = GreyOptions(
            host = "localhost",
            port = port,
            useTls = false
        )
    }

    /**
     * Create a copy with a new access token.
     */
    fun withAccessToken(accessToken: String?): GreyOptions = copy(accessToken = accessToken)
}
