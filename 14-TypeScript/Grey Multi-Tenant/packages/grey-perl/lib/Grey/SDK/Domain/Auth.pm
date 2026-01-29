package Grey::SDK::Domain::Auth;

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

# Authenticate with email and password
sub login {
    my ($self, %args) = @_;
    
    my $email    = $args{email};
    my $password = $args{password};
    
    # Validate required fields
    return $self->_validation_error('Email is required')
        unless defined $email && length $email;
    
    return $self->_validation_error('Password is required')
        unless defined $password && length $password;
    
    my $result = $self->{http}->post('/api/v1/auth/login', {
        email    => $email,
        password => $password,
    });
    
    # Store access token on success
    if ($result->is_ok) {
        my $data = $result->data;
        if ($data->{access_token}) {
            $self->{http}->access_token($data->{access_token});
        }
    }
    
    return $result;
}

# Logout current user
sub logout {
    my ($self) = @_;
    
    my $result = $self->{http}->post('/api/v1/auth/logout');
    
    # Clear token on success
    if ($result->is_ok) {
        $self->{http}->clear_token();
    }
    
    return $result;
}

# Refresh access token
sub refresh {
    my ($self, %args) = @_;
    
    my $refresh_token = $args{refresh_token};
    
    return $self->_validation_error('Refresh token is required')
        unless defined $refresh_token && length $refresh_token;
    
    my $result = $self->{http}->post('/api/v1/auth/refresh', {
        refresh_token => $refresh_token,
    });
    
    # Update access token on success
    if ($result->is_ok) {
        my $data = $result->data;
        if ($data->{access_token}) {
            $self->{http}->access_token($data->{access_token});
        }
    }
    
    return $result;
}

# Manually set access token
sub set_token {
    my ($self, $token) = @_;
    $self->{http}->access_token($token);
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

Grey::SDK::Domain::Auth - Authentication client for Grey SDK

=head1 SYNOPSIS

    my $auth = Grey::SDK::Domain::Auth->new($http_client);
    
    my $result = $auth->login(
        email    => 'user@example.com',
        password => 'secret'
    );
    
    $auth->logout();
    
    $auth->refresh(refresh_token => $token);

=cut
