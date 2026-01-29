<?php

declare(strict_types=1);

namespace Grey\Sdk;

use Grey\Sdk\Config\GreyOptions;
use Grey\Sdk\Domain\AuthClient;
use Grey\Sdk\Domain\MutationClient;
use Grey\Sdk\Domain\ProjectsClient;
use Grey\Sdk\Domain\QueryClient;
use Grey\Sdk\Domain\UserClient;
use Grey\Sdk\Http\HttpClient;

/**
 * Main Grey SDK client.
 *
 * This is the primary entry point for the Grey SDK. It provides access to all
 * domain clients through dedicated methods.
 *
 * Example usage:
 * ```php
 * $client = new GreyClient(GreyOptions::local(8080));
 *
 * $result = $client->auth()->login('user@example.com', 'password');
 * if ($result->isOk()) {
 *     $user = $client->user()->getUser()->getData();
 *     echo "Hello, " . $user['name'];
 * }
 * ```
 */
final class GreyClient
{
    private HttpClient $http;
    private ?AuthClient $authClient = null;
    private ?UserClient $userClient = null;
    private ?ProjectsClient $projectsClient = null;
    private ?QueryClient $queryClient = null;
    private ?MutationClient $mutationClient = null;

    public function __construct(
        private readonly GreyOptions $options,
    ) {
        $this->http = new HttpClient($options);
    }

    /**
     * Gets the Auth domain client.
     */
    public function auth(): AuthClient
    {
        if ($this->authClient === null) {
            $this->authClient = new AuthClient($this->http);
        }
        return $this->authClient;
    }

    /**
     * Gets the User domain client.
     */
    public function user(): UserClient
    {
        if ($this->userClient === null) {
            $this->userClient = new UserClient($this->http);
        }
        return $this->userClient;
    }

    /**
     * Gets the Projects domain client.
     */
    public function projects(): ProjectsClient
    {
        if ($this->projectsClient === null) {
            $this->projectsClient = new ProjectsClient($this->http);
        }
        return $this->projectsClient;
    }

    /**
     * Gets the Query domain client.
     */
    public function query(): QueryClient
    {
        if ($this->queryClient === null) {
            $this->queryClient = new QueryClient($this->http);
        }
        return $this->queryClient;
    }

    /**
     * Gets the Mutation domain client.
     */
    public function mutation(): MutationClient
    {
        if ($this->mutationClient === null) {
            $this->mutationClient = new MutationClient($this->http);
        }
        return $this->mutationClient;
    }

    /**
     * Returns true if the client is authenticated.
     */
    public function isAuthenticated(): bool
    {
        return $this->http->isAuthenticated();
    }

    /**
     * Gets the current options.
     */
    public function getOptions(): GreyOptions
    {
        return $this->options;
    }

    /**
     * Gets the underlying HTTP client.
     *
     * Use with caution - prefer domain clients for most operations.
     */
    public function getHttpClient(): HttpClient
    {
        return $this->http;
    }
}
