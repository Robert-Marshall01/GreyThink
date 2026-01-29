<?php

declare(strict_types=1);

namespace Grey\Sdk\Tests;

use Grey\Sdk\Config\GreyOptions;
use PHPUnit\Framework\TestCase;

final class OptionsTest extends TestCase
{
    public function testOptionsCreation(): void
    {
        $options = new GreyOptions(
            host: 'api.grey.com',
            port: 443,
            useTls: true,
            timeoutMs: 5000,
            metadata: ['X-Custom' => 'value'],
        );

        $this->assertSame('api.grey.com', $options->getHost());
        $this->assertSame(443, $options->getPort());
        $this->assertTrue($options->useTls());
        $this->assertSame(5000, $options->getTimeoutMs());
        $this->assertSame(5.0, $options->getTimeoutSeconds());
        $this->assertSame(['X-Custom' => 'value'], $options->getMetadata());
    }

    public function testOptionsBaseUrl(): void
    {
        $httpOptions = new GreyOptions('localhost', 8080, false);
        $httpsOptions = new GreyOptions('api.grey.com', 443, true);

        $this->assertSame('http://localhost:8080', $httpOptions->getBaseUrl());
        $this->assertSame('https://api.grey.com:443', $httpsOptions->getBaseUrl());
    }

    public function testLocalFactory(): void
    {
        $options = GreyOptions::local();
        
        $this->assertSame('localhost', $options->getHost());
        $this->assertSame(8080, $options->getPort());
        $this->assertFalse($options->useTls());
    }

    public function testLocalFactoryWithPort(): void
    {
        $options = GreyOptions::local(9000);
        
        $this->assertSame(9000, $options->getPort());
    }

    public function testProductionFactory(): void
    {
        $options = GreyOptions::production('api.grey.com');
        
        $this->assertSame('api.grey.com', $options->getHost());
        $this->assertSame(443, $options->getPort());
        $this->assertTrue($options->useTls());
    }

    public function testWithTimeout(): void
    {
        $options = GreyOptions::local()->withTimeout(10000);
        
        $this->assertSame(10000, $options->getTimeoutMs());
        $this->assertSame('localhost', $options->getHost()); // Original values preserved
    }

    public function testWithMetadata(): void
    {
        $options = GreyOptions::local()
            ->withMetadata(['X-First' => 'a'])
            ->withMetadata(['X-Second' => 'b']);
        
        $metadata = $options->getMetadata();
        $this->assertSame('a', $metadata['X-First']);
        $this->assertSame('b', $metadata['X-Second']);
    }

    public function testToArray(): void
    {
        $options = new GreyOptions('localhost', 8080, false, 30000, ['key' => 'value']);
        $array = $options->toArray();

        $this->assertSame('localhost', $array['host']);
        $this->assertSame(8080, $array['port']);
        $this->assertFalse($array['useTls']);
        $this->assertSame(30000, $array['timeoutMs']);
        $this->assertSame(['key' => 'value'], $array['metadata']);
    }
}
