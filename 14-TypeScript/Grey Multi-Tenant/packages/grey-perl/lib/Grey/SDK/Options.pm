package Grey::SDK::Options;

use strict;
use warnings;
use 5.020;

use constant {
    DEFAULT_TIMEOUT => 30,
    DEFAULT_PORT    => 443,
};

# Constructor
sub new {
    my ($class, %args) = @_;
    
    my $self = {
        host    => $args{host} // die("host is required"),
        port    => $args{port} // DEFAULT_PORT,
        use_tls => $args{use_tls} // 1,
        timeout => $args{timeout} // DEFAULT_TIMEOUT,
        headers => $args{headers} // {},
    };
    
    return bless $self, $class;
}

# Factory for local development
sub local {
    my ($class, $port) = @_;
    $class = ref($class) || $class || __PACKAGE__;
    $port //= 8080;
    
    return $class->new(
        host    => 'localhost',
        port    => $port,
        use_tls => 0,
        timeout => DEFAULT_TIMEOUT,
    );
}

# Factory for production
sub production {
    my ($class, $host, $port) = @_;
    $class = ref($class) || $class || __PACKAGE__;
    $port //= 443;
    
    return $class->new(
        host    => $host,
        port    => $port,
        use_tls => 1,
        timeout => DEFAULT_TIMEOUT,
    );
}

# Accessors
sub host    { $_[0]->{host} }
sub port    { $_[0]->{port} }
sub use_tls { $_[0]->{use_tls} }
sub timeout { $_[0]->{timeout} }
sub headers { $_[0]->{headers} }

# Get base URL
sub base_url {
    my ($self) = @_;
    my $scheme = $self->{use_tls} ? 'https' : 'http';
    return "$scheme://$self->{host}:$self->{port}";
}

# Create new options with different timeout
sub with_timeout {
    my ($self, $timeout) = @_;
    
    return Grey::SDK::Options->new(
        host    => $self->{host},
        port    => $self->{port},
        use_tls => $self->{use_tls},
        timeout => $timeout,
        headers => { %{$self->{headers}} },
    );
}

# Create new options with merged headers
sub with_headers {
    my ($self, $new_headers) = @_;
    
    return Grey::SDK::Options->new(
        host    => $self->{host},
        port    => $self->{port},
        use_tls => $self->{use_tls},
        timeout => $self->{timeout},
        headers => { %{$self->{headers}}, %{$new_headers} },
    );
}

# Convert to hash
sub to_hash {
    my ($self) = @_;
    
    return {
        host    => $self->{host},
        port    => $self->{port},
        use_tls => $self->{use_tls},
        timeout => $self->{timeout},
        headers => { %{$self->{headers}} },
    };
}

1;

__END__

=head1 NAME

Grey::SDK::Options - Configuration options for Grey SDK

=head1 SYNOPSIS

    use Grey::SDK::Options;
    
    # Local development
    my $opts = Grey::SDK::Options->local(8080);
    
    # Production
    my $opts = Grey::SDK::Options->production('api.grey.com');
    
    # Custom
    my $opts = Grey::SDK::Options->new(
        host    => 'api.example.com',
        port    => 443,
        use_tls => 1,
        timeout => 60,
    );

=cut
