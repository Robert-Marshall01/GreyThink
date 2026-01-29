<?php

declare(strict_types=1);

namespace Grey\Sdk\Error;

use Throwable;

/**
 * Represents a normalized error in the Grey SDK.
 */
final class GreyError
{
    public function __construct(
        private readonly ErrorCode $code,
        private readonly string $message,
        private readonly ?array $details = null,
    ) {}

    /**
     * Gets the error code.
     */
    public function getCode(): ErrorCode
    {
        return $this->code;
    }

    /**
     * Gets the error code as a string.
     */
    public function getCodeString(): string
    {
        return $this->code->value;
    }

    /**
     * Gets the error message.
     */
    public function getMessage(): string
    {
        return $this->message;
    }

    /**
     * Gets additional error details.
     */
    public function getDetails(): ?array
    {
        return $this->details;
    }

    /**
     * Creates an unauthorized error.
     */
    public static function unauthorized(?string $message = null): self
    {
        return new self(
            ErrorCode::UNAUTHORIZED,
            $message ?? 'Authentication required',
        );
    }

    /**
     * Creates a forbidden error.
     */
    public static function forbidden(?string $message = null): self
    {
        return new self(
            ErrorCode::FORBIDDEN,
            $message ?? 'Permission denied',
        );
    }

    /**
     * Creates a not found error.
     */
    public static function notFound(?string $message = null): self
    {
        return new self(
            ErrorCode::NOT_FOUND,
            $message ?? 'Resource not found',
        );
    }

    /**
     * Creates a validation error.
     */
    public static function validation(string $message, ?array $details = null): self
    {
        return new self(
            ErrorCode::VALIDATION_ERROR,
            $message,
            $details,
        );
    }

    /**
     * Creates a network error.
     */
    public static function network(?string $message = null): self
    {
        return new self(
            ErrorCode::NETWORK_ERROR,
            $message ?? 'Network error occurred',
        );
    }

    /**
     * Creates a timeout error.
     */
    public static function timeout(?string $message = null): self
    {
        return new self(
            ErrorCode::TIMEOUT,
            $message ?? 'Request timed out',
        );
    }

    /**
     * Creates a server error.
     */
    public static function server(?string $message = null): self
    {
        return new self(
            ErrorCode::SERVER_ERROR,
            $message ?? 'Server error occurred',
        );
    }

    /**
     * Creates an unknown error.
     */
    public static function unknown(?string $message = null): self
    {
        return new self(
            ErrorCode::UNKNOWN,
            $message ?? 'An unknown error occurred',
        );
    }

    /**
     * Creates a GreyError from an HTTP response.
     */
    public static function fromHttpResponse(int $status, ?array $body = null): self
    {
        $code = ErrorCode::fromHttpStatus($status);
        $message = $body['message'] ?? $body['error'] ?? "HTTP error $status";
        $details = $body['details'] ?? null;

        return new self($code, $message, $details);
    }

    /**
     * Creates a GreyError from an exception.
     */
    public static function fromException(Throwable $e): self
    {
        if ($e instanceof \GuzzleHttp\Exception\ConnectException) {
            return self::network($e->getMessage());
        }

        if ($e instanceof \GuzzleHttp\Exception\RequestException) {
            $response = $e->getResponse();
            if ($response !== null) {
                $status = $response->getStatusCode();
                $body = json_decode((string) $response->getBody(), true);
                return self::fromHttpResponse($status, is_array($body) ? $body : null);
            }
            return self::network($e->getMessage());
        }

        return self::unknown($e->getMessage());
    }

    /**
     * Converts the error to an array.
     */
    public function toArray(): array
    {
        return [
            'code' => $this->code->value,
            'message' => $this->message,
            'details' => $this->details,
        ];
    }

    /**
     * Converts the error to JSON.
     */
    public function toJson(): string
    {
        return json_encode($this->toArray(), JSON_THROW_ON_ERROR);
    }
}
