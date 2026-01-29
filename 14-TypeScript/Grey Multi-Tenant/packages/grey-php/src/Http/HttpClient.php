<?php

declare(strict_types=1);

namespace Grey\Sdk\Http;

use Grey\Sdk\Config\GreyOptions;
use Grey\Sdk\Error\GreyError;
use Grey\Sdk\Error\Result;
use GuzzleHttp\Client;
use GuzzleHttp\Exception\GuzzleException;
use GuzzleHttp\RequestOptions;

/**
 * HTTP client for making requests to the Grey API.
 */
final class HttpClient
{
    private Client $client;
    private ?string $accessToken = null;

    public function __construct(
        private readonly GreyOptions $options,
    ) {
        $this->client = new Client([
            'base_uri' => $options->getBaseUrl(),
            'timeout' => $options->getTimeoutSeconds(),
            'headers' => array_merge(
                ['Content-Type' => 'application/json', 'Accept' => 'application/json'],
                $options->getMetadata(),
            ),
        ]);
    }

    /**
     * Sets the access token for authenticated requests.
     */
    public function setAccessToken(?string $token): void
    {
        $this->accessToken = $token;
    }

    /**
     * Gets the current access token.
     */
    public function getAccessToken(): ?string
    {
        return $this->accessToken;
    }

    /**
     * Returns true if authenticated.
     */
    public function isAuthenticated(): bool
    {
        return $this->accessToken !== null;
    }

    /**
     * Makes a GET request.
     *
     * @return Result<array>
     */
    public function get(string $path, array $query = [], bool $requireAuth = true): Result
    {
        return $this->request('GET', $path, [
            RequestOptions::QUERY => $query,
        ], $requireAuth);
    }

    /**
     * Makes a POST request.
     *
     * @return Result<array>
     */
    public function post(string $path, array $body = [], bool $requireAuth = true): Result
    {
        return $this->request('POST', $path, [
            RequestOptions::JSON => $body,
        ], $requireAuth);
    }

    /**
     * Makes a PUT request.
     *
     * @return Result<array>
     */
    public function put(string $path, array $body = [], bool $requireAuth = true): Result
    {
        return $this->request('PUT', $path, [
            RequestOptions::JSON => $body,
        ], $requireAuth);
    }

    /**
     * Makes a PATCH request.
     *
     * @return Result<array>
     */
    public function patch(string $path, array $body = [], bool $requireAuth = true): Result
    {
        return $this->request('PATCH', $path, [
            RequestOptions::JSON => $body,
        ], $requireAuth);
    }

    /**
     * Makes a DELETE request.
     *
     * @return Result<array>
     */
    public function delete(string $path, array $body = [], bool $requireAuth = true): Result
    {
        $options = [];
        if (!empty($body)) {
            $options[RequestOptions::JSON] = $body;
        }
        return $this->request('DELETE', $path, $options, $requireAuth);
    }

    /**
     * Makes an HTTP request.
     *
     * @return Result<array>
     */
    private function request(string $method, string $path, array $options = [], bool $requireAuth = true): Result
    {
        if ($requireAuth && !$this->isAuthenticated()) {
            return Result::err(GreyError::unauthorized());
        }

        try {
            $headers = $options['headers'] ?? [];

            if ($this->accessToken !== null) {
                $headers['Authorization'] = 'Bearer ' . $this->accessToken;
            }

            $options['headers'] = $headers;

            $response = $this->client->request($method, $path, $options);
            $statusCode = $response->getStatusCode();
            $body = (string) $response->getBody();

            $data = [];
            if (!empty($body)) {
                $decoded = json_decode($body, true, 512, JSON_THROW_ON_ERROR);
                $data = is_array($decoded) ? $decoded : ['data' => $decoded];
            }

            if ($statusCode >= 200 && $statusCode < 300) {
                return Result::ok($data);
            }

            return Result::err(GreyError::fromHttpResponse($statusCode, $data));
        } catch (GuzzleException $e) {
            return Result::err(GreyError::fromException($e));
        } catch (\JsonException $e) {
            return Result::err(GreyError::validation('Invalid JSON response: ' . $e->getMessage()));
        } catch (\Throwable $e) {
            return Result::err(GreyError::fromException($e));
        }
    }
}
