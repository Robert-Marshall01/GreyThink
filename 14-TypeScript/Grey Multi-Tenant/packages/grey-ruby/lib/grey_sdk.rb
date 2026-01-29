# frozen_string_literal: true

require_relative "grey_sdk/version"
require_relative "grey_sdk/error/error_code"
require_relative "grey_sdk/error/grey_error"
require_relative "grey_sdk/error/result"
require_relative "grey_sdk/config/options"
require_relative "grey_sdk/http/http_client"
require_relative "grey_sdk/domain/auth_client"
require_relative "grey_sdk/domain/user_client"
require_relative "grey_sdk/domain/projects_client"
require_relative "grey_sdk/domain/query_client"
require_relative "grey_sdk/domain/mutation_client"
require_relative "grey_sdk/client"

# Grey SDK - Ruby client for the Grey Multi-Tenant API
module GreySdk
  class Error < StandardError; end
end
