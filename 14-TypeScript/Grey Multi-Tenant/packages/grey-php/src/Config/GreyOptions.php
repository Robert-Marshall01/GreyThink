<?php

declare(strict_types=1);

namespace Grey\Sdk\Config;

/**
 * Configuration options for the Grey SDK client.
 */
final class GreyOptions
{
    public const DEFAULT_TIMEOUT_MS = 30000;
    public const DEFAULT_PORT = 443;

    public function __construct(
        private readonly string $host,
        private readonly int $port = self::DEFAULT_PORT,
        private readonly bool $useTls = true,
        private readonly int $timeoutMs = self::DEFAULT_TIMEOUT_MS,
        private readonly array $metadata = [],
    ) {}

    /**
     * Gets the server host.
     */
    public function getHost(): string
    {
        return $this->host;
    }

    /**
     * Gets the server port.
     */
    public function getPort(): int
    {
        return $this->port;
    }

    /**
     * Returns true if TLS should be used.
     */
    public function useTls(): bool
    {
        return $this->useTls;
    }

    /**
     * Gets the request timeout in milliseconds.
     */
    public function getTimeoutMs(): int
    {
        return $this->timeoutMs;
    }

    /**
     * Gets the timeout in seconds for Guzzle.
     */
    public function getTimeoutSeconds(): float
    {
        return $this->timeoutMs / 1000.0;
    }

    /**
     * Gets additional metadata headers.
     */
    public function getMetadata(): array
    {
        return $this->metadata;
    }

    /**
     * Gets the base URL for HTTP requests.
     */
    public function getBaseUrl(): string
    {
        $scheme = $this->useTls ? 'https' : 'http';
        return "{$scheme}://{$this->host}:{$this->port}";
    }

    /**
     * Creates options for local development.
     */
    public static function local(int $port = 8080): self
    {
        return new self(
            host: 'localhost',
            port: $port,
            useTls: false,
            timeoutMs: self::DEFAULT_TIMEOUT_MS,
        );
    }

    /**
     * Creates options for production with TLS.
     */
    public static function production(string $host, int $port = 443): self
    {
        return new self(
            host: $host,
            port: $port,
            useTls: true,
            timeoutMs: self::DEFAULT_TIMEOUT_MS,
        );
    }

    /**
     * Returns new options with a different timeout.
     */
    public function withTimeout(int $timeoutMs): self
    {
        return new self(
            $this->host,
            $this->port,
            $this->useTls,
            $timeoutMs,
            $this->metadata,
        );
    }

    /**
     * Returns new options with additional metadata merged.
     */
    public function withMetadata(array $metadata): self
    {
        return new self(
            $this->host,
            $this->port,
            $this->useTls,
            $this->timeoutMs,
            array_merge($this->metadata, $metadata),
        );
    }

    /**
     * Converts options to an array.
     */
    public function toArray(): array
    {
        return [
            'host' => $this->host,
            'port' => $this->port,
            'useTls' => $this->useTls,
            'timeoutMs' => $this->timeoutMs,
            'metadata' => $this->metadata,
        ];
    }
}
