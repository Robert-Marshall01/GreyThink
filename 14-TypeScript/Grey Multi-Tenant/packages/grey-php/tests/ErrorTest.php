<?php

declare(strict_types=1);

namespace Grey\Sdk\Tests;

use Grey\Sdk\Error\ErrorCode;
use Grey\Sdk\Error\GreyError;
use Grey\Sdk\Error\Result;
use PHPUnit\Framework\TestCase;

final class ErrorTest extends TestCase
{
    public function testErrorCodeFromHttpStatus(): void
    {
        $this->assertSame(ErrorCode::UNAUTHORIZED, ErrorCode::fromHttpStatus(401));
        $this->assertSame(ErrorCode::FORBIDDEN, ErrorCode::fromHttpStatus(403));
        $this->assertSame(ErrorCode::NOT_FOUND, ErrorCode::fromHttpStatus(404));
        $this->assertSame(ErrorCode::VALIDATION_ERROR, ErrorCode::fromHttpStatus(400));
        $this->assertSame(ErrorCode::VALIDATION_ERROR, ErrorCode::fromHttpStatus(422));
        $this->assertSame(ErrorCode::SERVER_ERROR, ErrorCode::fromHttpStatus(500));
        $this->assertSame(ErrorCode::SERVER_ERROR, ErrorCode::fromHttpStatus(503));
        $this->assertSame(ErrorCode::TIMEOUT, ErrorCode::fromHttpStatus(408));
        $this->assertSame(ErrorCode::UNKNOWN, ErrorCode::fromHttpStatus(418));
    }

    public function testGreyErrorCreation(): void
    {
        $error = new GreyError(ErrorCode::NOT_FOUND, 'Resource not found', ['id' => '123']);
        
        $this->assertSame(ErrorCode::NOT_FOUND, $error->getCode());
        $this->assertSame('not_found', $error->getCodeString());
        $this->assertSame('Resource not found', $error->getMessage());
        $this->assertSame(['id' => '123'], $error->getDetails());
    }

    public function testGreyErrorFactories(): void
    {
        $this->assertSame(ErrorCode::UNAUTHORIZED, GreyError::unauthorized()->getCode());
        $this->assertSame(ErrorCode::FORBIDDEN, GreyError::forbidden()->getCode());
        $this->assertSame(ErrorCode::NOT_FOUND, GreyError::notFound()->getCode());
        $this->assertSame(ErrorCode::VALIDATION_ERROR, GreyError::validation('test')->getCode());
        $this->assertSame(ErrorCode::NETWORK_ERROR, GreyError::network()->getCode());
        $this->assertSame(ErrorCode::TIMEOUT, GreyError::timeout()->getCode());
        $this->assertSame(ErrorCode::SERVER_ERROR, GreyError::server()->getCode());
        $this->assertSame(ErrorCode::UNKNOWN, GreyError::unknown()->getCode());
    }

    public function testGreyErrorToArray(): void
    {
        $error = GreyError::validation('Invalid email', ['field' => 'email']);
        $array = $error->toArray();

        $this->assertSame('validation_error', $array['code']);
        $this->assertSame('Invalid email', $array['message']);
        $this->assertSame(['field' => 'email'], $array['details']);
    }

    public function testGreyErrorFromHttpResponse(): void
    {
        $error = GreyError::fromHttpResponse(404, ['message' => 'User not found']);

        $this->assertSame(ErrorCode::NOT_FOUND, $error->getCode());
        $this->assertSame('User not found', $error->getMessage());
    }
}

final class ResultTest extends TestCase
{
    public function testResultOk(): void
    {
        $result = Result::ok(['id' => 1, 'name' => 'Test']);

        $this->assertTrue($result->isOk());
        $this->assertFalse($result->isErr());
        $this->assertSame(['id' => 1, 'name' => 'Test'], $result->getData());
        $this->assertNull($result->getError());
    }

    public function testResultErr(): void
    {
        $error = GreyError::unauthorized();
        $result = Result::err($error);

        $this->assertFalse($result->isOk());
        $this->assertTrue($result->isErr());
        $this->assertNull($result->getData());
        $this->assertSame($error, $result->getError());
    }

    public function testResultUnwrap(): void
    {
        $result = Result::ok('value');
        $this->assertSame('value', $result->unwrap());
    }

    public function testResultUnwrapThrowsOnError(): void
    {
        $result = Result::err(GreyError::unauthorized());
        
        $this->expectException(\RuntimeException::class);
        $result->unwrap();
    }

    public function testResultUnwrapOr(): void
    {
        $ok = Result::ok('value');
        $err = Result::err(GreyError::unauthorized());

        $this->assertSame('value', $ok->unwrapOr('default'));
        $this->assertSame('default', $err->unwrapOr('default'));
    }

    public function testResultMap(): void
    {
        $result = Result::ok(5);
        $mapped = $result->map(fn($x) => $x * 2);

        $this->assertTrue($mapped->isOk());
        $this->assertSame(10, $mapped->getData());
    }

    public function testResultMapSkipsOnError(): void
    {
        $result = Result::err(GreyError::unauthorized());
        $mapped = $result->map(fn($x) => $x * 2);

        $this->assertTrue($mapped->isErr());
    }

    public function testResultThen(): void
    {
        $result = Result::ok(5);
        $chained = $result->then(fn($x) => Result::ok($x * 2));

        $this->assertTrue($chained->isOk());
        $this->assertSame(10, $chained->getData());
    }

    public function testResultCatch(): void
    {
        $result = Result::err(GreyError::unauthorized());
        $caught = $result->catch(fn($e) => Result::ok('recovered'));

        $this->assertTrue($caught->isOk());
        $this->assertSame('recovered', $caught->getData());
    }

    public function testResultToArray(): void
    {
        $ok = Result::ok(['data' => 'test']);
        $this->assertSame(['ok' => true, 'data' => ['data' => 'test']], $ok->toArray());

        $err = Result::err(GreyError::unauthorized());
        $errArray = $err->toArray();
        $this->assertFalse($errArray['ok']);
        $this->assertSame('unauthorized', $errArray['error']['code']);
    }
}
