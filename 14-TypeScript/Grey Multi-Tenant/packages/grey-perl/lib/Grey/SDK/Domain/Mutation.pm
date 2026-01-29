package Grey::SDK::Domain::Mutation;

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

# Execute a mutation operation
sub mutate {
    my ($self, %args) = @_;
    
    my $mutation_name = $args{mutation_name};
    my $input         = $args{input};
    
    return $self->_validation_error('Mutation name is required')
        unless defined $mutation_name && length $mutation_name;
    
    return $self->_validation_error('Input data is required')
        unless defined $input;
    
    my %body = (
        mutation => $mutation_name,
        input    => $input,
    );
    
    return $self->{http}->post('/api/v1/mutate', \%body);
}

# Execute a batch of mutations
sub batch_mutate {
    my ($self, %args) = @_;
    
    my $mutations = $args{mutations};
    
    return $self->_validation_error('Mutations array is required')
        unless defined $mutations && ref($mutations) eq 'ARRAY';
    
    return $self->_validation_error('At least one mutation is required')
        unless @$mutations > 0;
    
    my @formatted = map {
        {
            mutation => $_->{mutation_name},
            input    => $_->{input},
        }
    } @$mutations;
    
    return $self->{http}->post('/api/v1/mutate/batch', { mutations => \@formatted });
}

# Execute a mutation within a transaction
sub transactional_mutate {
    my ($self, %args) = @_;
    
    my $transaction_id = $args{transaction_id};
    my $mutation_name  = $args{mutation_name};
    my $input          = $args{input};
    
    return $self->_validation_error('Transaction ID is required')
        unless defined $transaction_id && length $transaction_id;
    
    return $self->_validation_error('Mutation name is required')
        unless defined $mutation_name && length $mutation_name;
    
    return $self->_validation_error('Input data is required')
        unless defined $input;
    
    my %body = (
        transaction_id => $transaction_id,
        mutation       => $mutation_name,
        input          => $input,
    );
    
    return $self->{http}->post('/api/v1/mutate/transactional', \%body);
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

Grey::SDK::Domain::Mutation - Mutation client for Grey SDK

=head1 SYNOPSIS

    my $mutation = Grey::SDK::Domain::Mutation->new($http_client);
    
    my $result = $mutation->mutate(
        mutation_name => 'updateUser',
        input         => { id => '123', name => 'New Name' }
    );
    
    my $batch = $mutation->batch_mutate(
        mutations => [
            { mutation_name => 'updateUser', input => { id => '1', name => 'A' } },
            { mutation_name => 'updateUser', input => { id => '2', name => 'B' } },
        ]
    );

=cut
