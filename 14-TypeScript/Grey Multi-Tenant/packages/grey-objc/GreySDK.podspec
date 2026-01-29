Pod::Spec.new do |s|
  s.name             = 'GreySDK'
  s.version          = '1.0.0'
  s.summary          = 'Grey Multi-Tenant SDK for Objective-C'
  s.description      = <<-DESC
    The Grey Multi-Tenant SDK provides a complete client for interacting with
    Grey services. It includes authentication, user management, project operations,
    and generic query/mutation support using gRPC transport.
  DESC
  s.homepage         = 'https://github.com/grey-systems/grey-objc'
  s.license          = { :type => 'MIT', :file => 'LICENSE' }
  s.author           = { 'Grey Systems' => 'sdk@grey.io' }
  s.source           = { :git => 'https://github.com/grey-systems/grey-objc.git', :tag => s.version.to_s }

  s.ios.deployment_target = '12.0'
  s.osx.deployment_target = '10.14'
  s.tvos.deployment_target = '12.0'
  s.watchos.deployment_target = '5.0'

  s.source_files = 'GreySDK/**/*.{h,m}'
  s.public_header_files = 'GreySDK/**/*.h'

  # gRPC dependencies (when using actual gRPC implementation)
  # Uncomment these when integrating with real gRPC-ObjC
  # s.dependency 'gRPC-ProtoRPC', '~> 1.0'
  # s.dependency 'Protobuf', '~> 3.0'

  s.frameworks = 'Foundation'

  s.pod_target_xcconfig = {
    'CLANG_ENABLE_MODULES' => 'YES',
    'DEFINES_MODULE' => 'YES'
  }

  s.test_spec 'Tests' do |test_spec|
    test_spec.source_files = 'Tests/**/*.m'
    test_spec.frameworks = 'XCTest'
  end
end
