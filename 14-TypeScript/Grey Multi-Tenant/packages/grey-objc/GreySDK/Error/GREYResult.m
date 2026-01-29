//
//  GREYResult.m
//  GreySDK
//
//  Result type representing success or failure.
//

#import "GREYResult.h"

@interface GREYResult ()
@property (nonatomic, readwrite, nullable) id value;
@property (nonatomic, readwrite, nullable) GREYError *error;
@end

@implementation GREYResult

- (instancetype)initWithValue:(id)value error:(GREYError *)error {
    self = [super init];
    if (self) {
        _value = value;
        _error = error;
    }
    return self;
}

- (BOOL)isSuccess {
    return _error == nil;
}

- (BOOL)isFailure {
    return _error != nil;
}

+ (instancetype)ok:(id)value {
    return [[self alloc] initWithValue:value error:nil];
}

+ (instancetype)fail:(GREYError *)error {
    return [[self alloc] initWithValue:nil error:error];
}

+ (instancetype)failWithCode:(GREYErrorCode)code
                     message:(NSString *)message
                     details:(nullable NSString *)details {
    GREYError *error = [[GREYError alloc] initWithCode:code
                                               message:message
                                               details:details];
    return [[self alloc] initWithValue:nil error:error];
}

- (id)getOr:(id)defaultValue {
    return self.isSuccess ? _value : defaultValue;
}

- (GREYResult *)map:(id (^)(id value))mapper {
    if (self.isSuccess) {
        return [GREYResult ok:mapper(_value)];
    }
    return [GREYResult fail:_error];
}

- (GREYResult *)flatMap:(GREYResult * (^)(id value))mapper {
    if (self.isSuccess) {
        return mapper(_value);
    }
    return [GREYResult fail:_error];
}

- (GREYResult *)onSuccess:(void (^)(id value))block {
    if (self.isSuccess && block) {
        block(_value);
    }
    return self;
}

- (GREYResult *)onFailure:(void (^)(GREYError *error))block {
    if (self.isFailure && block) {
        block(_error);
    }
    return self;
}

- (id)matchSuccess:(id (^)(id value))onSuccess
           failure:(id (^)(GREYError *error))onFailure {
    if (self.isSuccess) {
        return onSuccess(_value);
    }
    return onFailure(_error);
}

- (NSString *)description {
    if (self.isSuccess) {
        return [NSString stringWithFormat:@"Ok(%@)", _value];
    }
    return [NSString stringWithFormat:@"Fail(%@)", _error];
}

@end
