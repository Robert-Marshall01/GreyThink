#!/usr/bin/env perl

use strict;
use warnings;
use Test::More tests => 8;

use_ok('Grey::SDK::Client');
use_ok('Grey::SDK::Domain::Auth');
use_ok('Grey::SDK::Domain::User');
use_ok('Grey::SDK::Domain::Projects');
use_ok('Grey::SDK::Domain::Query');
use_ok('Grey::SDK::Domain::Mutation');

# Test client creation
my $client = Grey::SDK::Client->local(8080);
isa_ok($client, 'Grey::SDK::Client', 'client is Grey::SDK::Client');

# Test domain clients are initialized
isa_ok($client->auth, 'Grey::SDK::Domain::Auth', 'auth client is initialized');
