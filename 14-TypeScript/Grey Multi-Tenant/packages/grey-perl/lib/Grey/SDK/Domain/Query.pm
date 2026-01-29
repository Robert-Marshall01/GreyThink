package Grey::SDK::Domain::Query;

use strict;
use warnings;
use 5.020;

use Grey::SDK::Result;
use Grey::SDK::Error qw(make_error ERROR_VALIDATION);

# Constructor
sub new {
    my ($class, $http_client) = @_;
    
    my $self = {
        http => $http_client,
    };
    
    return bless $self, $class;
}

# Execute a query operation
sub query {
    my ($self, %args) = @_;
    
    my $query_name = $args{query_name};
    my $variables  = $args{variables};
    
    return $self->_validation_error('Query name is required')
        unless defined $query_name && length $query_name;
    
    my %body = (query => $query_name);
    $body{variables} = $variables if defined $variables;
    
    return $self->{http}->post('/api/v1/query', \%body);
}

# Execute a batch of queries
sub batch_query {
    my ($self, %args) = @_;
    
    my $queries = $args{queries};
    
    return $self->_validation_error('Queries array is required')
        unless defined $queries && ref($queries) eq 'ARRAY';
    
    return $self->_validation_error('At least one query is required')
        unless @$queries > 0;
    
    my @formatted = map {
        {
            query     => $_->{query_name},
            variables => $_->{variables},
        }
    } @$queries;
    
    return $self->{http}->post('/api/v1/query/batch', { queries => \@formatted });
}

# Execute a paginated query
sub paginated_query {
    my ($self, %args) = @_;
    
    my $query_name = $args{query_name};
    my $variables  = $args{variables} // {};
    my $page       = $args{page} // 1;
    my $per_page   = $args{per_page} // 20;
    
    return $self->_validation_error('Query name is required')
        unless defined $query_name && length $query_name;
    
    my %body = (
        query      => $query_name,
        variables  => $variables,
        pagination => {
            page     => $page,
            per_page => $per_page,
        },
    );
    
    return $self->{http}->post('/api/v1/query', \%body);
}

# Create validation error result
sub _validation_error {
    my ($self, $message) = @_;
    
    return Grey::SDK::Result->err(
        make_error(
            code    => ERROR_VALIDATION,
            message => $message,
        )
    );
}

1;

__END__

=head1 NAME

Grey::SDK::Domain::Query - Query client for Grey SDK

=head1 SYNOPSIS

    my $query = Grey::SDK::Domain::Query->new($http_client);
    
    my $result = $query->query(
        query_name => 'getUsers',
        variables  => { limit => 10 }
    );
    
    my $batch = $query->batch_query(
        queries => [
            { query_name => 'getUser', variables => { id => '1' } },
            { query_name => 'getUser', variables => { id => '2' } },
        ]
    );

=cut
