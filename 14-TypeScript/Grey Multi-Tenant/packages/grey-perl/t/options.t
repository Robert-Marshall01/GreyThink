#!/usr/bin/env perl

use strict;
use warnings;
use Test::More tests => 14;

use_ok('Grey::SDK::Options');

# Test constructor with required parameters
my $opts = Grey::SDK::Options->new(host => 'api.example.com');
is($opts->host, 'api.example.com', 'host is set');
is($opts->port, 443, 'default port is 443');
ok($opts->use_tls, 'default use_tls is true');
is($opts->timeout, 30, 'default timeout is 30');
is_deeply($opts->headers, {}, 'default headers is empty');

# Test local factory
my $local = Grey::SDK::Options->local(3000);
is($local->host, 'localhost', 'local host is localhost');
is($local->port, 3000, 'local port is set');
ok(!$local->use_tls, 'local use_tls is false');

# Test production factory
my $prod = Grey::SDK::Options->production('api.grey.com');
is($prod->host, 'api.grey.com', 'production host is set');
is($prod->port, 443, 'production port is 443');
ok($prod->use_tls, 'production use_tls is true');

# Test base_url
is($local->base_url, 'http://localhost:3000', 'local base_url is correct');
is($prod->base_url, 'https://api.grey.com:443', 'production base_url is correct');
