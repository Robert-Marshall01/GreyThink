package Grey::SDK;

use strict;
use warnings;
use 5.020;

our $VERSION = '0.1.0';

use Grey::SDK::Error;
use Grey::SDK::Result;
use Grey::SDK::Options;
use Grey::SDK::Http::HttpClient;
use Grey::SDK::Domain::Auth;
use Grey::SDK::Domain::User;
use Grey::SDK::Domain::Projects;
use Grey::SDK::Domain::Query;
use Grey::SDK::Domain::Mutation;
use Grey::SDK::Client;

1;

__END__

=head1 NAME

Grey::SDK - Perl client SDK for the Grey Multi-Tenant API

=head1 VERSION

Version 0.1.0

=head1 SYNOPSIS

    use Grey::SDK::Client;

    my $client = Grey::SDK::Client->local(8080);
    
    my $result = $client->auth->login(
        email    => 'user@example.com',
        password => 'secret'
    );
    
    if ($result->is_ok) {
        print "Logged in!\n";
    }

=head1 DESCRIPTION

Grey::SDK provides a Perl client for the Grey Multi-Tenant API.
It includes authentication, user management, project operations,
and generic query/mutation support.

=head1 AUTHOR

Grey Team

=head1 LICENSE

MIT

=cut
