package Grey::SDK::Domain::User;

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

# Get user by ID
sub get_user {
    my ($self, %args) = @_;
    
    my $user_id = $args{user_id};
    
    return $self->_validation_error('User ID is required')
        unless defined $user_id && length $user_id;
    
    return $self->{http}->get("/api/v1/users/$user_id");
}

# Get current authenticated user
sub get_current_user {
    my ($self) = @_;
    
    return $self->{http}->get('/api/v1/users/me');
}

# Update user profile
sub update_user {
    my ($self, %args) = @_;
    
    my $user_id = $args{user_id};
    my $data    = $args{data};
    
    return $self->_validation_error('User ID is required')
        unless defined $user_id && length $user_id;
    
    return $self->_validation_error('Update data is required')
        unless defined $data && ref($data) eq 'HASH' && %$data;
    
    return $self->{http}->patch("/api/v1/users/$user_id", $data);
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

Grey::SDK::Domain::User - User client for Grey SDK

=head1 SYNOPSIS

    my $user = Grey::SDK::Domain::User->new($http_client);
    
    my $result = $user->get_user(user_id => '123');
    my $me = $user->get_current_user();

=cut
