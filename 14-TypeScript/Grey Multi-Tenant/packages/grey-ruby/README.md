# Grey Ruby SDK

Ruby client for the Grey Multi-Tenant API using HTTP JSON transport.

## Requirements

- Ruby 3.0+

## Installation

Add to your Gemfile:

```ruby
gem 'grey_sdk'
```

Then run:

```bash
bundle install
```

Or install directly:

```bash
gem install grey_sdk
```

## Quick Start

```ruby
require 'grey_sdk'

# Create client
options = GreySdk::Config::Options.local(8080)
# Or for production:
# options = GreySdk::Config::Options.production('api.grey.com')

client = GreySdk::Client.new(options)

# Login
result = client.auth.login('user@example.com', 'password')
if result.ok?
  puts "Access token: #{result.data[:access_token]}"
else
  puts "Login failed: #{result.error.message}"
end

# Get current user
result = client.user.get_user
if result.ok?
  puts "Hello, #{result.data[:name]}"
end

# List projects
result = client.projects.list_projects(page: 1, page_size: 10)
if result.ok?
  result.data[:projects].each do |project|
    puts "Project: #{project[:name]}"
  end
end

# Create project
result = client.projects.create_project('My Project', description: 'A new project')

# Query
result = client.query.query('/api/data', params: { filter: 'active' })

# Mutate
result = client.mutation.mutate('/api/update', method: :put, body: { name: 'Updated' })

# Logout
client.auth.logout
```

## Error Handling

All operations return a `Result` object:

```ruby
result = client.auth.login(email, password)

if result.ok?
  data = result.data
  # Success
else
  error = result.error
  puts error.code     # e.g., :unauthorized
  puts error.message  # Human-readable message
  puts error.details  # Additional details (hash or nil)
end
```

Error codes:
- `:unauthorized` - Authentication required
- `:forbidden` - Permission denied
- `:not_found` - Resource not found
- `:validation_error` - Input validation failed
- `:network_error` - Network failure
- `:timeout` - Request timeout
- `:server_error` - Server-side error
- `:unknown` - Unclassified error

## Configuration

```ruby
# Local development
options = GreySdk::Config::Options.local           # localhost:8080
options = GreySdk::Config::Options.local(9000)     # localhost:9000

# Production with HTTPS
options = GreySdk::Config::Options.production('api.grey.com')

# Full control
options = GreySdk::Config::Options.new(
  host: 'api.grey.com',
  port: 443,
  use_tls: true,
  timeout: 30,
  headers: { 'X-Custom-Header' => 'value' }
)
```

## Testing

```bash
bundle exec rspec
```

## License

MIT
