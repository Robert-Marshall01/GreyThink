# Grey::SDK

Perl client SDK for the Grey Multi-Tenant API.

## Requirements

- Perl 5.20+
- LWP::UserAgent
- JSON
- Try::Tiny

## Installation

```bash
# Install dependencies
cpanm LWP::UserAgent JSON Try::Tiny

# Install from source
perl Makefile.PL
make
make test
make install
```

## Usage

```perl
use Grey::SDK::Client;

# Create client for local development
my $client = Grey::SDK::Client->local(8080);

# Or for production
my $client = Grey::SDK::Client->production('api.grey.com');

# Authenticate
my $result = $client->auth->login(
    email    => 'user@example.com',
    password => 'secret'
);

if ($result->is_ok) {
    my $tokens = $result->data;
    print "Access token: $tokens->{access_token}\n";
} else {
    my $error = $result->error;
    print "Error: $error->{message}\n";
}

# List projects
my $projects_result = $client->projects->list_projects();

if ($projects_result->is_ok) {
    for my $project (@{$projects_result->data->{projects}}) {
        print "Project: $project->{name}\n";
    }
}

# Create a project
my $create_result = $client->projects->create_project(
    name        => 'New Project',
    description => 'A description'
);

# Execute a query
my $query_result = $client->query->query(
    query_name => 'getUsers',
    variables  => { limit => 10 }
);

# Execute a mutation
my $mutation_result = $client->mutation->mutate(
    mutation_name => 'updateUser',
    input         => { id => '123', name => 'New Name' }
);

# Logout
$client->auth->logout();
```

## Error Handling

All methods return a `Grey::SDK::Result` object:

```perl
my $result = $client->auth->login(email => $email, password => $password);

if ($result->is_ok) {
    my $data = $result->data;
    # Handle success
} else {
    my $error = $result->error;
    # error is a hashref with: code, message, details
    print "Error code: $error->{code}\n";
    print "Message: $error->{message}\n";
}

# Chaining with map/then
my $final = $result
    ->map(sub { $_[0]->{access_token} })
    ->then(sub { Grey::SDK::Result->ok("Token: $_[0]") });
```

## Error Codes

- `unauthorized` - Authentication required (401)
- `forbidden` - Permission denied (403)
- `not_found` - Resource not found (404)
- `validation_error` - Invalid input (400, 422)
- `network_error` - Connection failed
- `timeout` - Request timed out
- `server_error` - Server error (5xx)
- `unknown` - Unknown error

## License

MIT
