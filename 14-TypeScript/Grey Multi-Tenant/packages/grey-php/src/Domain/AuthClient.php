<?php

declare(strict_types=1);

namespace Grey\Sdk\Domain;

use Grey\Sdk\Error\GreyError;
use Grey\Sdk\Error\Result;
use Grey\Sdk\Http\HttpClient;

/**
 * Auth domain client for authentication operations.
 */
final class AuthClient
{
    public function __construct(
        private readonly HttpClient $http,
    ) {}

    /**
     * Authenticates a user with email and password.
     *
     * On success, stores the access token in the HTTP client.
     *
     * @return Result<array{accessToken: string, refreshToken: string, expiresIn: int}>
     */
    public function login(string $email, string $password): Result
    {
        $validation = $this->validateCredentials($email, $password);
        if ($validation->isErr()) {
            return $validation;
        }

        $result = $this->http->post('/api/auth/login', [
            'email' => $email,
            'password' => $password,
        ], false);

        if ($result->isOk()) {
            $data = $result->getData();
            $accessToken = $data['accessToken'] ?? $data['access_token'] ?? null;
            if ($accessToken !== null) {
                $this->http->setAccessToken($accessToken);
            }
        }

        return $result;
    }

    /**
     * Logs out the current user.
     *
     * Clears the access token from the HTTP client.
     *
     * @return Result<null>
     */
    public function logout(): Result
    {
        if (!$this->http->isAuthenticated()) {
            return Result::err(GreyError::unauthorized('Not authenticated'));
        }

        $result = $this->http->post('/api/auth/logout');

        // Clear token regardless of result
        $this->http->setAccessToken(null);

        if ($result->isErr()) {
            // Even if logout fails on server, we're logged out locally
            return Result::ok(null);
        }

        return Result::ok(null);
    }

    /**
     * Refreshes the authentication tokens.
     *
     * @return Result<array{accessToken: string, refreshToken: string, expiresIn: int}>
     */
    public function refresh(string $refreshToken): Result
    {
        if (empty($refreshToken)) {
            return Result::err(GreyError::validation('Refresh token cannot be empty'));
        }

        $result = $this->http->post('/api/auth/refresh', [
            'refreshToken' => $refreshToken,
        ], false);

        if ($result->isOk()) {
            $data = $result->getData();
            $accessToken = $data['accessToken'] ?? $data['access_token'] ?? null;
            if ($accessToken !== null) {
                $this->http->setAccessToken($accessToken);
            }
        }

        return $result;
    }

    /**
     * Validates login credentials.
     */
    private function validateCredentials(string $email, string $password): Result
    {
        if (empty($email)) {
            return Result::err(GreyError::validation('Email cannot be empty'));
        }

        if (!filter_var($email, FILTER_VALIDATE_EMAIL)) {
            return Result::err(GreyError::validation('Invalid email format'));
        }

        if (empty($password)) {
            return Result::err(GreyError::validation('Password cannot be empty'));
        }

        return Result::ok(null);
    }
}
