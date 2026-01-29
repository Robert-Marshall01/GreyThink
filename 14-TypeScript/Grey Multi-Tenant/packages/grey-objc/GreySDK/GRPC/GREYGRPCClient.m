//
//  GREYGRPCClient.m
//  GreySDK
//
//  Base gRPC client for the Grey SDK.
//

#import "GREYGRPCClient.h"

@interface GREYGRPCClient ()
@property (nonatomic, strong) GREYOptions *options;
@property (nonatomic, copy, nullable) NSString *authToken;
@end

@implementation GREYGRPCClient

- (instancetype)initWithOptions:(GREYOptions *)options {
    self = [super init];
    if (self) {
        _options = options;
        _authToken = options.authToken;
    }
    return self;
}

- (void)setAuthToken:(NSString *)token {
    _authToken = [token copy];
}

- (void)clearAuthToken {
    _authToken = nil;
}

- (NSDictionary<NSString *, NSString *> *)metadata {
    NSMutableDictionary *meta = [NSMutableDictionary dictionary];
    
    // Add custom headers
    [meta addEntriesFromDictionary:_options.customHeaders];
    
    // Add authorization header if token is set
    if (_authToken) {
        meta[@"authorization"] = [NSString stringWithFormat:@"Bearer %@", _authToken];
    }
    
    return [meta copy];
}

@end
