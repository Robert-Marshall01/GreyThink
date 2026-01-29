<?php

declare(strict_types=1);

namespace Grey\Sdk\Domain;

use Grey\Sdk\Error\GreyError;
use Grey\Sdk\Error\Result;
use Grey\Sdk\Http\HttpClient;

/**
 * Query domain client for query operations.
 */
final class QueryClient
{
    public function __construct(
        private readonly HttpClient $http,
    ) {}

    /**
     * Executes a query to the specified endpoint.
     *
     * @param array{params?: array, requireAuth?: bool, headers?: array} $options
     * @return Result<array{data: mixed, metadata: ?array}>
     */
    public function query(string $endpoint, array $options = []): Result
    {
        $validation = $this->validateEndpoint($endpoint);
        if ($validation->isErr()) {
            return $validation;
        }

        $requireAuth = $options['requireAuth'] ?? true;

        if ($requireAuth && !$this->http->isAuthenticated()) {
            return Result::err(GreyError::unauthorized('User not authenticated'));
        }

        $query = $options['params'] ?? [];

        return $this->http->get($endpoint, $query, $requireAuth);
    }

    /**
     * Validates the query endpoint.
     */
    private function validateEndpoint(string $endpoint): Result
    {
        if (empty($endpoint)) {
            return Result::err(GreyError::validation('Endpoint cannot be empty'));
        }

        return Result::ok(null);
    }
}
