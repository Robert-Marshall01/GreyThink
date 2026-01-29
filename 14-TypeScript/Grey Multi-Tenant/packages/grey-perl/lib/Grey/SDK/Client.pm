package Grey::SDK::Client;

use strict;
use warnings;
use 5.020;

use Grey::SDK::Options;
use Grey::SDK::Http::HttpClient;
use Grey::SDK::Domain::Auth;
use Grey::SDK::Domain::User;
use Grey::SDK::Domain::Projects;
use Grey::SDK::Domain::Query;
use Grey::SDK::Domain::Mutation;

# Constructor
sub new {
    my ($class, $options) = @_;
    
    my $http_client = Grey::SDK::Http::HttpClient->new($options);
    
    my $self = {
        options     => $options,
        http_client => $http_client,
        auth        => Grey::SDK::Domain::Auth->new($http_client),
        user        => Grey::SDK::Domain::User->new($http_client),
        projects    => Grey::SDK::Domain::Projects->new($http_client),
        query       => Grey::SDK::Domain::Query->new($http_client),
        mutation    => Grey::SDK::Domain::Mutation->new($http_client),
    };
    
    return bless $self, $class;
}

# Factory for local development
sub local {
    my ($class, $port) = @_;
    $class = ref($class) || $class || __PACKAGE__;
    $port //= 8080;
    
    my $options = Grey::SDK::Options->local($port);
    return $class->new($options);
}

# Factory for production
sub production {
    my ($class, $host, $port) = @_;
    $class = ref($class) || $class || __PACKAGE__;
    
    my $options = Grey::SDK::Options->production($host, $port);
    return $class->new($options);
}

# Domain client accessors
sub auth     { $_[0]->{auth} }
sub user     { $_[0]->{user} }
sub projects { $_[0]->{projects} }
sub query    { $_[0]->{query} }
sub mutation { $_[0]->{mutation} }

# Get options
sub options { $_[0]->{options} }

# Check if authenticated
sub is_authenticated {
    my ($self) = @_;
    return defined $self->{http_client}->access_token;
}

# Clear authentication
sub clear_auth {
    my ($self) = @_;
    $self->{http_client}->clear_token();
}

1;

__END__

=head1 NAME

Grey::SDK::Client - Main client facade for Grey SDK

=head1 SYNOPSIS

    use Grey::SDK::Client;
    
    # Local development
    my $client = Grey::SDK::Client->local(8080);
    
    # Production
    my $client = Grey::SDK::Client->production('api.grey.com');
    
    # Custom options
    my $options = Grey::SDK::Options->new(
        host    => 'api.example.com',
        port    => 443,
        use_tls => 1,
    );
    my $client = Grey::SDK::Client->new($options);
    
    # Use domain clients
    my $result = $client->auth->login(
        email    => 'user@example.com',
        password => 'secret'
    );
    
    if ($result->is_ok) {
        my $projects = $client->projects->list_projects();
    }

=head1 DESCRIPTION

Grey::SDK::Client provides the main entry point to the Grey SDK.
It initializes all domain clients and manages shared HTTP state.

=head1 METHODS

=head2 new($options)

Create a new client with the given Grey::SDK::Options.

=head2 local($port)

Factory method for local development. Defaults to port 8080.

=head2 production($host, $port)

Factory method for production. Defaults to port 443 with TLS.

=head2 auth

Returns the Grey::SDK::Domain::Auth client.

=head2 user

Returns the Grey::SDK::Domain::User client.

=head2 projects

Returns the Grey::SDK::Domain::Projects client.

=head2 query

Returns the Grey::SDK::Domain::Query client.

=head2 mutation

Returns the Grey::SDK::Domain::Mutation client.

=head2 is_authenticated

Returns true if an access token is set.

=head2 clear_auth

Clears the current access token.

=cut
