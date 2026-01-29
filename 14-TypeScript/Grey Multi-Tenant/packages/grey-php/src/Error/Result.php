<?php

declare(strict_types=1);

namespace Grey\Sdk\Error;

/**
 * Represents the result of an operation that can succeed or fail.
 *
 * @template T
 */
final class Result
{
    /**
     * @param T|null $data
     */
    private function __construct(
        private readonly bool $ok,
        private readonly mixed $data = null,
        private readonly ?GreyError $error = null,
    ) {}

    /**
     * Creates a successful result.
     *
     * @template U
     * @param U $data
     * @return Result<U>
     */
    public static function ok(mixed $data): self
    {
        return new self(true, $data);
    }

    /**
     * Creates a failed result.
     *
     * @return Result<null>
     */
    public static function err(GreyError $error): self
    {
        return new self(false, null, $error);
    }

    /**
     * Returns true if the result is successful.
     */
    public function isOk(): bool
    {
        return $this->ok;
    }

    /**
     * Returns true if the result is an error.
     */
    public function isErr(): bool
    {
        return !$this->ok;
    }

    /**
     * Gets the data if successful, null otherwise.
     *
     * @return T|null
     */
    public function getData(): mixed
    {
        return $this->data;
    }

    /**
     * Gets the error if failed, null otherwise.
     */
    public function getError(): ?GreyError
    {
        return $this->error;
    }

    /**
     * Gets the data or throws an exception if failed.
     *
     * @return T
     * @throws \RuntimeException
     */
    public function unwrap(): mixed
    {
        if (!$this->ok) {
            throw new \RuntimeException(
                "Unwrap failed: " . ($this->error?->getMessage() ?? 'Unknown error')
            );
        }
        return $this->data;
    }

    /**
     * Gets the data or returns a default value if failed.
     *
     * @template U
     * @param U $default
     * @return T|U
     */
    public function unwrapOr(mixed $default): mixed
    {
        return $this->ok ? $this->data : $default;
    }

    /**
     * Maps the data if successful.
     *
     * @template U
     * @param callable(T): U $fn
     * @return Result<U>
     */
    public function map(callable $fn): self
    {
        if ($this->ok) {
            try {
                return self::ok($fn($this->data));
            } catch (\Throwable $e) {
                return self::err(GreyError::fromException($e));
            }
        }
        return $this;
    }

    /**
     * Chains another operation if successful.
     *
     * @template U
     * @param callable(T): Result<U> $fn
     * @return Result<U>
     */
    public function then(callable $fn): self
    {
        if ($this->ok) {
            try {
                return $fn($this->data);
            } catch (\Throwable $e) {
                return self::err(GreyError::fromException($e));
            }
        }
        return $this;
    }

    /**
     * Handles an error if failed.
     *
     * @param callable(GreyError): Result<T> $fn
     * @return Result<T>
     */
    public function catch(callable $fn): self
    {
        if (!$this->ok && $this->error !== null) {
            try {
                return $fn($this->error);
            } catch (\Throwable $e) {
                return self::err(GreyError::fromException($e));
            }
        }
        return $this;
    }

    /**
     * Converts the result to an array.
     */
    public function toArray(): array
    {
        if ($this->ok) {
            return ['ok' => true, 'data' => $this->data];
        }
        return ['ok' => false, 'error' => $this->error?->toArray()];
    }
}
