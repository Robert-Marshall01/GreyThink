<?php

declare(strict_types=1);

namespace Grey\Sdk\Domain;

use Grey\Sdk\Error\GreyError;
use Grey\Sdk\Error\Result;
use Grey\Sdk\Http\HttpClient;

/**
 * Projects domain client for project operations.
 */
final class ProjectsClient
{
    public function __construct(
        private readonly HttpClient $http,
    ) {}

    /**
     * Lists projects for the authenticated user.
     *
     * @param array{page?: int, pageSize?: int, filter?: array} $options
     * @return Result<array{projects: array, total: int, page: int, pageSize: int}>
     */
    public function listProjects(array $options = []): Result
    {
        if (!$this->http->isAuthenticated()) {
            return Result::err(GreyError::unauthorized('User not authenticated'));
        }

        $validation = $this->validatePaginationOptions($options);
        if ($validation->isErr()) {
            return $validation;
        }

        $query = [];
        if (isset($options['page'])) {
            $query['page'] = $options['page'];
        }
        if (isset($options['pageSize'])) {
            $query['pageSize'] = $options['pageSize'];
        }
        if (isset($options['filter'])) {
            $query['filter'] = json_encode($options['filter']);
        }

        return $this->http->get('/api/projects', $query);
    }

    /**
     * Creates a new project.
     *
     * @param array{description?: string, metadata?: array} $options
     * @return Result<array{id: string, name: string, description: ?string, createdAt: string, updatedAt: ?string, metadata: ?array}>
     */
    public function createProject(string $name, array $options = []): Result
    {
        if (!$this->http->isAuthenticated()) {
            return Result::err(GreyError::unauthorized('User not authenticated'));
        }

        $validation = $this->validateProjectName($name);
        if ($validation->isErr()) {
            return $validation;
        }

        $body = ['name' => $name];
        
        if (isset($options['description'])) {
            $body['description'] = $options['description'];
        }
        if (isset($options['metadata'])) {
            $body['metadata'] = $options['metadata'];
        }

        return $this->http->post('/api/projects', $body);
    }

    /**
     * Validates pagination options.
     */
    private function validatePaginationOptions(array $options): Result
    {
        if (isset($options['page'])) {
            if (!is_int($options['page']) || $options['page'] < 1) {
                return Result::err(GreyError::validation('Page must be a positive integer'));
            }
        }

        if (isset($options['pageSize'])) {
            if (!is_int($options['pageSize']) || $options['pageSize'] < 1 || $options['pageSize'] > 100) {
                return Result::err(GreyError::validation('Page size must be between 1 and 100'));
            }
        }

        return Result::ok(null);
    }

    /**
     * Validates project name.
     */
    private function validateProjectName(string $name): Result
    {
        if (empty($name)) {
            return Result::err(GreyError::validation('Project name cannot be empty'));
        }

        if (strlen($name) > 255) {
            return Result::err(GreyError::validation('Project name cannot exceed 255 characters'));
        }

        return Result::ok(null);
    }
}
