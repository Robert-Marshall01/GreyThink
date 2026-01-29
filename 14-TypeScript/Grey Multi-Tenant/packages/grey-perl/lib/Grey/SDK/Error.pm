package Grey::SDK::Error;

use strict;
use warnings;
use 5.020;

use Exporter 'import';

our @EXPORT_OK = qw(
    ERROR_UNAUTHORIZED
    ERROR_FORBIDDEN
    ERROR_NOT_FOUND
    ERROR_VALIDATION
    ERROR_NETWORK
    ERROR_TIMEOUT
    ERROR_SERVER
    ERROR_UNKNOWN
    error_from_http_status
    is_valid_error_code
    normalize_error_code
    make_error
);

our %EXPORT_TAGS = (all => \@EXPORT_OK);

# Error code constants
use constant {
    ERROR_UNAUTHORIZED => 'unauthorized',
    ERROR_FORBIDDEN    => 'forbidden',
    ERROR_NOT_FOUND    => 'not_found',
    ERROR_VALIDATION   => 'validation_error',
    ERROR_NETWORK      => 'network_error',
    ERROR_TIMEOUT      => 'timeout',
    ERROR_SERVER       => 'server_error',
    ERROR_UNKNOWN      => 'unknown',
};

# All valid error codes
my %VALID_CODES = map { $_ => 1 } (
    ERROR_UNAUTHORIZED,
    ERROR_FORBIDDEN,
    ERROR_NOT_FOUND,
    ERROR_VALIDATION,
    ERROR_NETWORK,
    ERROR_TIMEOUT,
    ERROR_SERVER,
    ERROR_UNKNOWN,
);

# Convert HTTP status code to error code
sub error_from_http_status {
    my ($status) = @_;
    
    return ERROR_UNAUTHORIZED if $status == 401;
    return ERROR_FORBIDDEN    if $status == 403;
    return ERROR_NOT_FOUND    if $status == 404;
    return ERROR_VALIDATION   if $status == 400 || $status == 422;
    return ERROR_TIMEOUT      if $status == 408 || $status == 504;
    return ERROR_SERVER       if $status >= 500 && $status < 600;
    return ERROR_UNKNOWN;
}

# Check if a code is valid
sub is_valid_error_code {
    my ($code) = @_;
    return exists $VALID_CODES{$code // ''};
}

# Normalize a code to a valid error code
sub normalize_error_code {
    my ($code) = @_;
    return is_valid_error_code($code) ? $code : ERROR_UNKNOWN;
}

# Create an error hashref
sub make_error {
    my (%args) = @_;
    
    my $code    = normalize_error_code($args{code});
    my $message = $args{message} // '';
    my $details = $args{details};
    
    return {
        code    => $code,
        message => $message,
        details => $details,
    };
}

# Factory methods for common errors
sub unauthorized {
    my ($message) = @_;
    return make_error(
        code    => ERROR_UNAUTHORIZED,
        message => $message // 'Authentication required',
    );
}

sub forbidden {
    my ($message) = @_;
    return make_error(
        code    => ERROR_FORBIDDEN,
        message => $message // 'Permission denied',
    );
}

sub not_found {
    my ($message) = @_;
    return make_error(
        code    => ERROR_NOT_FOUND,
        message => $message // 'Resource not found',
    );
}

sub validation {
    my ($message, $details) = @_;
    return make_error(
        code    => ERROR_VALIDATION,
        message => $message // 'Validation error',
        details => $details,
    );
}

sub network {
    my ($message) = @_;
    return make_error(
        code    => ERROR_NETWORK,
        message => $message // 'Network error occurred',
    );
}

sub timeout {
    my ($message) = @_;
    return make_error(
        code    => ERROR_TIMEOUT,
        message => $message // 'Request timed out',
    );
}

sub server {
    my ($message) = @_;
    return make_error(
        code    => ERROR_SERVER,
        message => $message // 'Server error occurred',
    );
}

sub unknown {
    my ($message) = @_;
    return make_error(
        code    => ERROR_UNKNOWN,
        message => $message // 'An unknown error occurred',
    );
}

# Create error from HTTP response
sub from_http_response {
    my ($status, $body) = @_;
    
    my $code = error_from_http_status($status);
    my $message = $body->{message} // $body->{error} // "HTTP error $status";
    my $details = $body->{details};
    
    return make_error(
        code    => $code,
        message => $message,
        details => $details,
    );
}

1;

__END__

=head1 NAME

Grey::SDK::Error - Error handling for Grey SDK

=head1 SYNOPSIS

    use Grey::SDK::Error qw(:all);
    
    my $error = make_error(
        code    => ERROR_UNAUTHORIZED,
        message => 'Invalid credentials'
    );
    
    # Or use factory methods
    my $err = Grey::SDK::Error::unauthorized('Please log in');

=cut
