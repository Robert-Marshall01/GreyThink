package Grey::SDK::Http::HttpClient;

use strict;
use warnings;
use 5.020;

use LWP::UserAgent;
use HTTP::Request;
use URI;
use JSON qw(encode_json decode_json);
use Try::Tiny;

use Grey::SDK::Result;
use Grey::SDK::Error qw(
    make_error
    error_from_http_status
    ERROR_NETWORK
    ERROR_TIMEOUT
    ERROR_VALIDATION
    ERROR_UNKNOWN
);

# Constructor
sub new {
    my ($class, $options) = @_;
    
    my $ua = LWP::UserAgent->new(
        timeout => $options->timeout,
        ssl_opts => { verify_hostname => 1 },
    );
    
    my $self = {
        options      => $options,
        ua           => $ua,
        access_token => undef,
    };
    
    return bless $self, $class;
}

# Set/get access token
sub access_token {
    my ($self, $token) = @_;
    
    if (defined $token) {
        $self->{access_token} = $token;
    }
    
    return $self->{access_token};
}

# Clear access token
sub clear_token {
    my ($self) = @_;
    $self->{access_token} = undef;
}

# GET request
sub get {
    my ($self, $path, $params) = @_;
    $params //= {};
    
    my $uri = $self->_build_uri($path, $params);
    my $request = HTTP::Request->new('GET', $uri);
    
    return $self->_execute($request);
}

# POST request
sub post {
    my ($self, $path, $body) = @_;
    $body //= {};
    
    my $uri = $self->_build_uri($path);
    my $request = HTTP::Request->new('POST', $uri);
    $request->content(encode_json($body));
    
    return $self->_execute($request);
}

# PUT request
sub put {
    my ($self, $path, $body) = @_;
    $body //= {};
    
    my $uri = $self->_build_uri($path);
    my $request = HTTP::Request->new('PUT', $uri);
    $request->content(encode_json($body));
    
    return $self->_execute($request);
}

# PATCH request
sub patch {
    my ($self, $path, $body) = @_;
    $body //= {};
    
    my $uri = $self->_build_uri($path);
    my $request = HTTP::Request->new('PATCH', $uri);
    $request->content(encode_json($body));
    
    return $self->_execute($request);
}

# DELETE request
sub delete {
    my ($self, $path, $params) = @_;
    $params //= {};
    
    my $uri = $self->_build_uri($path, $params);
    my $request = HTTP::Request->new('DELETE', $uri);
    
    return $self->_execute($request);
}

# Build URI from path and optional params
sub _build_uri {
    my ($self, $path, $params) = @_;
    
    my $base_url = $self->{options}->base_url;
    my $uri = URI->new("$base_url$path");
    
    if ($params && %$params) {
        $uri->query_form(%$params);
    }
    
    return $uri;
}

# Apply headers to request
sub _apply_headers {
    my ($self, $request) = @_;
    
    $request->header('Content-Type' => 'application/json');
    $request->header('Accept' => 'application/json');
    
    # Apply custom headers from options
    my $headers = $self->{options}->headers;
    for my $key (keys %$headers) {
        $request->header($key => $headers->{$key});
    }
    
    # Apply authorization header if token exists
    if ($self->{access_token}) {
        $request->header('Authorization' => "Bearer $self->{access_token}");
    }
}

# Execute request and return Result
sub _execute {
    my ($self, $request) = @_;
    
    $self->_apply_headers($request);
    
    my $result;
    try {
        my $response = $self->{ua}->request($request);
        $result = $self->_handle_response($response);
    } catch {
        my $error_msg = "$_";
        
        if ($error_msg =~ /timeout/i) {
            $result = Grey::SDK::Result->err(
                make_error(
                    code    => ERROR_TIMEOUT,
                    message => $error_msg,
                )
            );
        } elsif ($error_msg =~ /connect|connection|network/i) {
            $result = Grey::SDK::Result->err(
                make_error(
                    code    => ERROR_NETWORK,
                    message => $error_msg,
                )
            );
        } else {
            $result = Grey::SDK::Result->err(
                make_error(
                    code    => ERROR_UNKNOWN,
                    message => $error_msg,
                )
            );
        }
    };
    
    return $result;
}

# Handle HTTP response
sub _handle_response {
    my ($self, $response) = @_;
    
    my $status = $response->code;
    my $body = $self->_parse_body($response->decoded_content);
    
    if ($response->is_success) {
        return Grey::SDK::Result->ok($body);
    } else {
        # Check for LWP internal errors
        if ($status == 500 && $response->header('Client-Warning')) {
            my $warning = $response->header('Client-Warning');
            if ($warning =~ /timeout/i) {
                return Grey::SDK::Result->err(
                    make_error(
                        code    => ERROR_TIMEOUT,
                        message => 'Request timed out',
                    )
                );
            } elsif ($warning =~ /connect/i) {
                return Grey::SDK::Result->err(
                    make_error(
                        code    => ERROR_NETWORK,
                        message => 'Connection failed',
                    )
                );
            }
        }
        
        my $error = make_error(
            code    => error_from_http_status($status),
            message => $body->{message} // $body->{error} // "HTTP error $status",
            details => $body->{details},
        );
        
        return Grey::SDK::Result->err($error);
    }
}

# Parse response body as JSON
sub _parse_body {
    my ($self, $content) = @_;
    
    return {} unless defined $content && length $content;
    
    my $body;
    try {
        $body = decode_json($content);
    } catch {
        $body = { raw => $content };
    };
    
    return $body;
}

1;

__END__

=head1 NAME

Grey::SDK::Http::HttpClient - HTTP client for Grey SDK

=head1 SYNOPSIS

    use Grey::SDK::Http::HttpClient;
    use Grey::SDK::Options;
    
    my $options = Grey::SDK::Options->local(8080);
    my $client = Grey::SDK::Http::HttpClient->new($options);
    
    my $result = $client->get('/api/v1/users');
    my $result = $client->post('/api/v1/auth/login', { email => $email, password => $pass });

=cut
