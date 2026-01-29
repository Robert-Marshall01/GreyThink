<?php

declare(strict_types=1);

namespace Grey\Sdk\Domain;

use Grey\Sdk\Error\GreyError;
use Grey\Sdk\Error\Result;
use Grey\Sdk\Http\HttpClient;

/**
 * Mutation domain client for mutation operations.
 */
final class MutationClient
{
    private const VALID_METHODS = ['POST', 'PUT', 'PATCH', 'DELETE'];

    public function __construct(
        private readonly HttpClient $http,
    ) {}

    /**
     * Executes a mutation to the specified endpoint.
     *
     * @param array{method?: string, body?: array, headers?: array} $options
     * @return Result<array{success: bool, data: mixed, metadata: ?array}>
     */
    public function mutate(string $endpoint, array $options = []): Result
    {
        if (!$this->http->isAuthenticated()) {
            return Result::err(GreyError::unauthorized('User not authenticated'));
        }

        $validation = $this->validateMutation($endpoint, $options);
        if ($validation->isErr()) {
            return $validation;
        }

        $method = strtoupper($options['method'] ?? 'POST');
        $body = $options['body'] ?? [];

        return match ($method) {
            'POST' => $this->http->post($endpoint, $body),
            'PUT' => $this->http->put($endpoint, $body),
            'PATCH' => $this->http->patch($endpoint, $body),
            'DELETE' => $this->http->delete($endpoint, $body),
            default => Result::err(GreyError::validation("Invalid method: {$method}")),
        };
    }

    /**
     * Validates mutation parameters.
     */
    private function validateMutation(string $endpoint, array $options): Result
    {
        if (empty($endpoint)) {
            return Result::err(GreyError::validation('Endpoint cannot be empty'));
        }

        if (isset($options['method'])) {
            $method = strtoupper($options['method']);
            if (!in_array($method, self::VALID_METHODS, true)) {
                return Result::err(GreyError::validation(
                    'Method must be one of: ' . implode(', ', self::VALID_METHODS)
                ));
            }
        }

        return Result::ok(null);
    }
}
