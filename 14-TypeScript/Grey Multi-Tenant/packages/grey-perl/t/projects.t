#!/usr/bin/env perl

use strict;
use warnings;
use Test::More tests => 8;

use Grey::SDK::Domain::Projects;
use Grey::SDK::Http::HttpClient;
use Grey::SDK::Options;

# Create mock HTTP client
my $options = Grey::SDK::Options->local(8080);
my $http = Grey::SDK::Http::HttpClient->new($options);
my $projects = Grey::SDK::Domain::Projects->new($http);

# Test create_project validation
my $result1 = $projects->create_project(name => '');
ok($result1->is_err, 'empty name returns error');
is($result1->error->{code}, 'validation_error', 'error code is validation_error');
like($result1->error->{message}, qr/name/i, 'error message mentions name');

# Test get_project validation
my $result2 = $projects->get_project(project_id => '');
ok($result2->is_err, 'empty project_id returns error');
is($result2->error->{code}, 'validation_error', 'error code is validation_error');

# Test update_project validation
my $result3 = $projects->update_project(project_id => '', data => {});
ok($result3->is_err, 'empty project_id returns error');

my $result4 = $projects->update_project(project_id => '123', data => {});
ok($result4->is_err, 'empty data returns error');
is($result4->error->{code}, 'validation_error', 'error code is validation_error');
