#!/usr/bin/env perl

use strict;
use warnings;
use Test::More tests => 10;

use_ok('Grey::SDK::Error', qw(:all));

# Test error_from_http_status
is(error_from_http_status(401), ERROR_UNAUTHORIZED, '401 -> unauthorized');
is(error_from_http_status(403), ERROR_FORBIDDEN, '403 -> forbidden');
is(error_from_http_status(404), ERROR_NOT_FOUND, '404 -> not_found');
is(error_from_http_status(400), ERROR_VALIDATION, '400 -> validation_error');
is(error_from_http_status(500), ERROR_SERVER, '500 -> server_error');
is(error_from_http_status(418), ERROR_UNKNOWN, '418 -> unknown');

# Test is_valid_error_code
ok(is_valid_error_code(ERROR_UNAUTHORIZED), 'unauthorized is valid');
ok(!is_valid_error_code('invalid_code'), 'invalid_code is not valid');

# Test make_error
my $error = make_error(
    code    => ERROR_FORBIDDEN,
    message => 'Access denied',
    details => { user_id => 123 },
);

is_deeply($error, {
    code    => 'forbidden',
    message => 'Access denied',
    details => { user_id => 123 },
}, 'make_error creates correct structure');
