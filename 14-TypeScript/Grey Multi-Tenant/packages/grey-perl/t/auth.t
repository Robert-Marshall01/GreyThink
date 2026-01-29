#!/usr/bin/env perl

use strict;
use warnings;
use Test::More tests => 6;

use Grey::SDK::Domain::Auth;
use Grey::SDK::Http::HttpClient;
use Grey::SDK::Options;

# Create mock HTTP client
my $options = Grey::SDK::Options->local(8080);
my $http = Grey::SDK::Http::HttpClient->new($options);
my $auth = Grey::SDK::Domain::Auth->new($http);

# Test login validation
my $result1 = $auth->login(email => '', password => 'secret');
ok($result1->is_err, 'empty email returns error');
is($result1->error->{code}, 'validation_error', 'error code is validation_error');

my $result2 = $auth->login(email => 'user@example.com', password => '');
ok($result2->is_err, 'empty password returns error');
is($result2->error->{code}, 'validation_error', 'error code is validation_error');

# Test refresh validation
my $result3 = $auth->refresh(refresh_token => '');
ok($result3->is_err, 'empty refresh token returns error');
is($result3->error->{code}, 'validation_error', 'error code is validation_error');
