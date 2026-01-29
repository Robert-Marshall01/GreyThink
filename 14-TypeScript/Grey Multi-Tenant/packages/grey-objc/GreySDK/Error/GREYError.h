//
//  GREYError.h
//  GreySDK
//
//  Normalized error type for the Grey SDK.
//

#import <Foundation/Foundation.h>
#import "GREYErrorCodes.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * Normalized error type for the Grey SDK.
 * Contains code, message, and optional details.
 */
@interface GREYError : NSObject

/// The error code
@property (nonatomic, readonly) GREYErrorCode code;

/// Human-readable error message
@property (nonatomic, readonly, copy) NSString *message;

/// Optional additional error details
@property (nonatomic, readonly, copy, nullable) NSString *details;

/// String representation of the error code
@property (nonatomic, readonly) NSString *codeString;

/// Whether this error is retryable
@property (nonatomic, readonly) BOOL isRetryable;

/**
 * Create a new error with code, message, and optional details.
 */
- (instancetype)initWithCode:(GREYErrorCode)code
                     message:(NSString *)message
                     details:(nullable NSString *)details;

/**
 * Create a new error with code and default message.
 */
- (instancetype)initWithCode:(GREYErrorCode)code;

// Convenience factory methods

+ (instancetype)unauthorized;
+ (instancetype)unauthorizedWithDetails:(nullable NSString *)details;

+ (instancetype)forbidden;
+ (instancetype)forbiddenWithDetails:(nullable NSString *)details;

+ (instancetype)notFound;
+ (instancetype)notFoundWithDetails:(nullable NSString *)details;

+ (instancetype)validationErrorWithMessage:(NSString *)message;
+ (instancetype)validationErrorWithMessage:(NSString *)message
                                   details:(nullable NSString *)details;

+ (instancetype)networkError;
+ (instancetype)networkErrorWithDetails:(nullable NSString *)details;

+ (instancetype)timeout;
+ (instancetype)timeoutWithDetails:(nullable NSString *)details;

+ (instancetype)serverError;
+ (instancetype)serverErrorWithDetails:(nullable NSString *)details;

+ (instancetype)fromHTTPStatus:(NSInteger)statusCode
                       message:(nullable NSString *)message
                       details:(nullable NSString *)details;

+ (instancetype)fromGRPCStatus:(NSInteger)statusCode
                       message:(nullable NSString *)message
                       details:(nullable NSString *)details;

+ (instancetype)fromNSError:(NSError *)error;

/**
 * Convert to NSError for compatibility.
 */
- (NSError *)toNSError;

@end

NS_ASSUME_NONNULL_END
