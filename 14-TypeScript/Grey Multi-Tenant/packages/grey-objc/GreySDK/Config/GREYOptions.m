//
//  GREYOptions.m
//  GreySDK
//
//  Configuration options for the Grey SDK client.
//

#import "GREYOptions.h"

@implementation GREYOptions

- (instancetype)init {
    self = [super init];
    if (self) {
        _host = @"localhost";
        _port = 50051;
        _useTLS = NO;
        _timeoutSeconds = 30.0;
        _authToken = nil;
        _customHeaders = @{};
    }
    return self;
}

+ (instancetype)localOptions {
    return [[self alloc] init];
}

+ (instancetype)localOptionsWithPort:(NSInteger)port {
    GREYOptions *options = [[self alloc] init];
    options.port = port;
    return options;
}

+ (instancetype)productionOptionsWithHost:(NSString *)host {
    return [self productionOptionsWithHost:host port:443];
}

+ (instancetype)productionOptionsWithHost:(NSString *)host port:(NSInteger)port {
    GREYOptions *options = [[self alloc] init];
    options.host = host;
    options.port = port;
    options.useTLS = YES;
    return options;
}

- (NSString *)endpoint {
    return [NSString stringWithFormat:@"%@:%ld", _host, (long)_port];
}

- (instancetype)withAuthToken:(NSString *)token {
    GREYOptions *copy = [self copy];
    copy.authToken = token;
    return copy;
}

- (instancetype)withTimeout:(NSTimeInterval)seconds {
    GREYOptions *copy = [self copy];
    copy.timeoutSeconds = seconds;
    return copy;
}

- (instancetype)withHeader:(NSString *)name value:(NSString *)value {
    GREYOptions *copy = [self copy];
    NSMutableDictionary *headers = [copy.customHeaders mutableCopy];
    headers[name] = value;
    copy.customHeaders = headers;
    return copy;
}

#pragma mark - NSCopying

- (instancetype)copyWithZone:(NSZone *)zone {
    GREYOptions *copy = [[GREYOptions alloc] init];
    copy.host = self.host;
    copy.port = self.port;
    copy.useTLS = self.useTLS;
    copy.timeoutSeconds = self.timeoutSeconds;
    copy.authToken = self.authToken;
    copy.customHeaders = [self.customHeaders copy];
    return copy;
}

@end
