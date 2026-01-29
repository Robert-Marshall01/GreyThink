#!/usr/bin/env perl

use strict;
use warnings;
use Test::More tests => 18;

use_ok('Grey::SDK::Result');
use_ok('Grey::SDK::Error', qw(make_error ERROR_UNAUTHORIZED ERROR_NOT_FOUND));

# Test ok result
my $ok = Grey::SDK::Result->ok({ id => 1, name => 'Test' });
ok($ok->is_ok, 'ok result is_ok');
ok(!$ok->is_err, 'ok result is not is_err');
is_deeply($ok->data, { id => 1, name => 'Test' }, 'ok result has correct data');
is($ok->error, undef, 'ok result has no error');

# Test err result
my $error = make_error(
    code    => ERROR_UNAUTHORIZED,
    message => 'Not logged in',
);
my $err = Grey::SDK::Result->err($error);
ok(!$err->is_ok, 'err result is not is_ok');
ok($err->is_err, 'err result is_err');
is($err->data, undef, 'err result has no data');
is($err->error->{code}, 'unauthorized', 'err result has correct error code');

# Test unwrap_or
is($ok->unwrap_or('default'), $ok->data, 'unwrap_or returns data for ok');
is($err->unwrap_or('default'), 'default', 'unwrap_or returns default for err');

# Test map
my $mapped = $ok->map(sub { $_[0]->{id} * 2 });
ok($mapped->is_ok, 'map result is ok');
is($mapped->data, 2, 'map transforms data correctly');

my $mapped_err = $err->map(sub { $_[0] * 2 });
ok($mapped_err->is_err, 'map passes through error');

# Test then
my $chained = $ok->then(sub { Grey::SDK::Result->ok($_[0]->{name} . '!') });
ok($chained->is_ok, 'then result is ok');
is($chained->data, 'Test!', 'then chains correctly');

# Test catch
my $caught = $err->catch(sub { Grey::SDK::Result->ok('recovered') });
ok($caught->is_ok, 'catch recovers from error');
