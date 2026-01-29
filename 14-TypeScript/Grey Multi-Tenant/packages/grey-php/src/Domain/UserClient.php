<?php

declare(strict_types=1);

namespace Grey\Sdk\Domain;

use Grey\Sdk\Error\GreyError;
use Grey\Sdk\Error\Result;
use Grey\Sdk\Http\HttpClient;

/**
 * User domain client for user operations.
 */
final class UserClient
{
    public function __construct(
        private readonly HttpClient $http,
    ) {}

    /**
     * Gets the currently authenticated user.
     *
     * @param array{forceRefresh?: bool} $options
     * @return Result<array{id: string, email: string, name: string, avatar: ?string, metadata: ?array}>
     */
    public function getUser(array $options = []): Result
    {
        if (!$this->http->isAuthenticated()) {
            return Result::err(GreyError::unauthorized('User not authenticated'));
        }

        $query = [];
        if (!empty($options['forceRefresh'])) {
            $query['refresh'] = 'true';
        }

        return $this->http->get('/api/user', $query);
    }
}
