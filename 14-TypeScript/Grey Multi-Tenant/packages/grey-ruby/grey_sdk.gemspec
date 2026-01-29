# frozen_string_literal: true

Gem::Specification.new do |spec|
  spec.name          = "grey_sdk"
  spec.version       = "0.1.0"
  spec.authors       = ["Grey Team"]
  spec.email         = ["team@grey.com"]

  spec.summary       = "Grey Multi-Tenant SDK for Ruby"
  spec.description   = "Ruby client for the Grey Multi-Tenant API using HTTP JSON transport"
  spec.homepage      = "https://github.com/grey/grey-ruby"
  spec.license       = "MIT"
  spec.required_ruby_version = ">= 3.0.0"

  spec.metadata["homepage_uri"] = spec.homepage
  spec.metadata["source_code_uri"] = spec.homepage
  spec.metadata["changelog_uri"] = "#{spec.homepage}/blob/main/CHANGELOG.md"

  spec.files = Dir.chdir(__dir__) do
    `git ls-files -z`.split("\x0").reject do |f|
      (f == __FILE__) || f.match(%r{\A(?:(?:bin|test|spec|features)/|\.(?:git|circleci)|appveyor)})
    end
  end

  spec.bindir = "exe"
  spec.executables = spec.files.grep(%r{\Aexe/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]

  spec.add_dependency "json", "~> 2.6"

  spec.add_development_dependency "bundler", "~> 2.4"
  spec.add_development_dependency "rake", "~> 13.0"
  spec.add_development_dependency "rspec", "~> 3.12"
  spec.add_development_dependency "rubocop", "~> 1.57"
  spec.add_development_dependency "webmock", "~> 3.19"
end
