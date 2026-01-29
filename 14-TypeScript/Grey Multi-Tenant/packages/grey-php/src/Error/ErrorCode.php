<?php

declare(strict_types=1);

namespace Grey\Sdk\Error;

/**
 * Enum representing all possible error codes in the Grey SDK.
 */
enum ErrorCode: string
{
    case UNAUTHORIZED = 'unauthorized';
    case FORBIDDEN = 'forbidden';
    case NOT_FOUND = 'not_found';
    case VALIDATION_ERROR = 'validation_error';
    case NETWORK_ERROR = 'network_error';
    case TIMEOUT = 'timeout';
    case SERVER_ERROR = 'server_error';
    case UNKNOWN = 'unknown';

    /**
     * Creates an ErrorCode from an HTTP status code.
     */
    public static function fromHttpStatus(int $status): self
    {
        return match (true) {
            $status === 401 => self::UNAUTHORIZED,
            $status === 403 => self::FORBIDDEN,
            $status === 404 => self::NOT_FOUND,
            $status === 400, $status === 422 => self::VALIDATION_ERROR,
            $status === 408, $status === 504 => self::TIMEOUT,
            $status >= 500 => self::SERVER_ERROR,
            default => self::UNKNOWN,
        };
    }
}
