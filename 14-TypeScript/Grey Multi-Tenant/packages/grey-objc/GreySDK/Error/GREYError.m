//
//  GREYError.m
//  GreySDK
//
//  Normalized error type for the Grey SDK.
//

#import "GREYError.h"

@implementation GREYError

- (instancetype)initWithCode:(GREYErrorCode)code
                     message:(NSString *)message
                     details:(nullable NSString *)details {
    self = [super init];
    if (self) {
        _code = code;
        _message = [message copy];
        _details = [details copy];
    }
    return self;
}

- (instancetype)initWithCode:(GREYErrorCode)code {
    return [self initWithCode:code
                      message:[GREYErrorCodes messageForCode:code]
                      details:nil];
}

- (NSString *)codeString {
    return [GREYErrorCodes stringForCode:_code];
}

- (BOOL)isRetryable {
    return [GREYErrorCodes isRetryable:_code];
}

#pragma mark - Factory Methods

+ (instancetype)unauthorized {
    return [[self alloc] initWithCode:GREYErrorCodeUnauthorized];
}

+ (instancetype)unauthorizedWithDetails:(nullable NSString *)details {
    return [[self alloc] initWithCode:GREYErrorCodeUnauthorized
                              message:[GREYErrorCodes messageForCode:GREYErrorCodeUnauthorized]
                              details:details];
}

+ (instancetype)forbidden {
    return [[self alloc] initWithCode:GREYErrorCodeForbidden];
}

+ (instancetype)forbiddenWithDetails:(nullable NSString *)details {
    return [[self alloc] initWithCode:GREYErrorCodeForbidden
                              message:[GREYErrorCodes messageForCode:GREYErrorCodeForbidden]
                              details:details];
}

+ (instancetype)notFound {
    return [[self alloc] initWithCode:GREYErrorCodeNotFound];
}

+ (instancetype)notFoundWithDetails:(nullable NSString *)details {
    return [[self alloc] initWithCode:GREYErrorCodeNotFound
                              message:[GREYErrorCodes messageForCode:GREYErrorCodeNotFound]
                              details:details];
}

+ (instancetype)validationErrorWithMessage:(NSString *)message {
    return [[self alloc] initWithCode:GREYErrorCodeValidationError
                              message:message
                              details:nil];
}

+ (instancetype)validationErrorWithMessage:(NSString *)message
                                   details:(nullable NSString *)details {
    return [[self alloc] initWithCode:GREYErrorCodeValidationError
                              message:message
                              details:details];
}

+ (instancetype)networkError {
    return [[self alloc] initWithCode:GREYErrorCodeNetworkError];
}

+ (instancetype)networkErrorWithDetails:(nullable NSString *)details {
    return [[self alloc] initWithCode:GREYErrorCodeNetworkError
                              message:[GREYErrorCodes messageForCode:GREYErrorCodeNetworkError]
                              details:details];
}

+ (instancetype)timeout {
    return [[self alloc] initWithCode:GREYErrorCodeTimeout];
}

+ (instancetype)timeoutWithDetails:(nullable NSString *)details {
    return [[self alloc] initWithCode:GREYErrorCodeTimeout
                              message:[GREYErrorCodes messageForCode:GREYErrorCodeTimeout]
                              details:details];
}

+ (instancetype)serverError {
    return [[self alloc] initWithCode:GREYErrorCodeServerError];
}

+ (instancetype)serverErrorWithDetails:(nullable NSString *)details {
    return [[self alloc] initWithCode:GREYErrorCodeServerError
                              message:[GREYErrorCodes messageForCode:GREYErrorCodeServerError]
                              details:details];
}

+ (instancetype)fromHTTPStatus:(NSInteger)statusCode
                       message:(nullable NSString *)message
                       details:(nullable NSString *)details {
    GREYErrorCode code = [GREYErrorCodes fromHTTPStatus:statusCode];
    NSString *msg = message ?: [GREYErrorCodes messageForCode:code];
    return [[self alloc] initWithCode:code message:msg details:details];
}

+ (instancetype)fromGRPCStatus:(NSInteger)statusCode
                       message:(nullable NSString *)message
                       details:(nullable NSString *)details {
    GREYErrorCode code = [GREYErrorCodes fromGRPCStatus:statusCode];
    NSString *msg = message ?: [GREYErrorCodes messageForCode:code];
    return [[self alloc] initWithCode:code message:msg details:details];
}

+ (instancetype)fromNSError:(NSError *)error {
    if ([error.domain isEqualToString:GREYErrorDomain]) {
        return [[self alloc] initWithCode:(GREYErrorCode)error.code
                                  message:error.localizedDescription
                                  details:error.userInfo[GREYErrorDetailsKey]];
    }
    return [[self alloc] initWithCode:GREYErrorCodeUnknown
                              message:error.localizedDescription
                              details:error.localizedFailureReason];
}

- (NSError *)toNSError {
    NSMutableDictionary *userInfo = [NSMutableDictionary dictionary];
    userInfo[NSLocalizedDescriptionKey] = _message;
    if (_details) {
        userInfo[GREYErrorDetailsKey] = _details;
        userInfo[NSLocalizedFailureReasonErrorKey] = _details;
    }
    return [NSError errorWithDomain:GREYErrorDomain
                               code:_code
                           userInfo:userInfo];
}

- (NSString *)description {
    if (_details) {
        return [NSString stringWithFormat:@"GREYError [%@]: %@ - %@",
                self.codeString, _message, _details];
    }
    return [NSString stringWithFormat:@"GREYError [%@]: %@",
            self.codeString, _message];
}

@end
