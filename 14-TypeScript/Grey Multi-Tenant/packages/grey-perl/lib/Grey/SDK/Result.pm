package Grey::SDK::Result;

use strict;
use warnings;
use 5.020;

use Grey::SDK::Error qw(make_error ERROR_UNKNOWN);
use Try::Tiny;

# Constructor - use ok() or err() factory methods instead
sub new {
    my ($class, %args) = @_;
    
    my $self = {
        _ok    => $args{ok} // 0,
        _data  => $args{data},
        _error => $args{error},
    };
    
    return bless $self, $class;
}

# Create a successful result
sub ok {
    my ($class, $data) = @_;
    $class = ref($class) || $class || __PACKAGE__;
    
    return $class->new(
        ok   => 1,
        data => $data,
    );
}

# Create a failed result
sub err {
    my ($class, $error) = @_;
    $class = ref($class) || $class || __PACKAGE__;
    
    # Ensure error is a proper hashref
    unless (ref($error) eq 'HASH' && exists $error->{code}) {
        $error = make_error(
            code    => ERROR_UNKNOWN,
            message => "$error",
        );
    }
    
    return $class->new(
        ok    => 0,
        error => $error,
    );
}

# Check if result is successful
sub is_ok {
    my ($self) = @_;
    return $self->{_ok};
}

# Check if result is an error
sub is_err {
    my ($self) = @_;
    return !$self->{_ok};
}

# Get the data (undef if error)
sub data {
    my ($self) = @_;
    return $self->{_data};
}

# Get the error (undef if success)
sub error {
    my ($self) = @_;
    return $self->{_error};
}

# Unwrap data or die with error message
sub unwrap {
    my ($self) = @_;
    
    if ($self->is_err) {
        my $msg = $self->{_error}{message} // 'Unknown error';
        die "Unwrap failed: $msg\n";
    }
    
    return $self->{_data};
}

# Unwrap data or return default value
sub unwrap_or {
    my ($self, $default) = @_;
    return $self->is_ok ? $self->{_data} : $default;
}

# Map the data if successful
sub map {
    my ($self, $fn) = @_;
    
    return $self if $self->is_err;
    
    my $result;
    try {
        my $mapped = $fn->($self->{_data});
        $result = Grey::SDK::Result->ok($mapped);
    } catch {
        $result = Grey::SDK::Result->err(
            make_error(
                code    => ERROR_UNKNOWN,
                message => "$_",
            )
        );
    };
    
    return $result;
}

# Chain another operation if successful
sub then {
    my ($self, $fn) = @_;
    
    return $self if $self->is_err;
    
    my $result;
    try {
        $result = $fn->($self->{_data});
        # Ensure it returns a Result
        unless (ref($result) && $result->isa('Grey::SDK::Result')) {
            $result = Grey::SDK::Result->ok($result);
        }
    } catch {
        $result = Grey::SDK::Result->err(
            make_error(
                code    => ERROR_UNKNOWN,
                message => "$_",
            )
        );
    };
    
    return $result;
}

# Handle error if failed
sub catch {
    my ($self, $fn) = @_;
    
    return $self if $self->is_ok;
    
    my $result;
    try {
        $result = $fn->($self->{_error});
        unless (ref($result) && $result->isa('Grey::SDK::Result')) {
            $result = Grey::SDK::Result->ok($result);
        }
    } catch {
        $result = Grey::SDK::Result->err(
            make_error(
                code    => ERROR_UNKNOWN,
                message => "$_",
            )
        );
    };
    
    return $result;
}

# Convert to hash
sub to_hash {
    my ($self) = @_;
    
    if ($self->is_ok) {
        return { ok => 1, data => $self->{_data} };
    } else {
        return { ok => 0, error => $self->{_error} };
    }
}

1;

__END__

=head1 NAME

Grey::SDK::Result - Result monad for Grey SDK operations

=head1 SYNOPSIS

    use Grey::SDK::Result;
    
    # Create results
    my $ok = Grey::SDK::Result->ok({ id => 1 });
    my $err = Grey::SDK::Result->err({ code => 'not_found', message => 'Not found' });
    
    # Check and access
    if ($ok->is_ok) {
        my $data = $ok->data;
    }
    
    # Chaining
    my $final = $result
        ->map(sub { $_[0]->{id} })
        ->then(sub { Grey::SDK::Result->ok("ID: $_[0]") });

=cut
