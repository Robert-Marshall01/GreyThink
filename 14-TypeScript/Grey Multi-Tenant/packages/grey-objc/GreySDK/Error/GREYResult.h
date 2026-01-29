//
//  GREYResult.h
//  GreySDK
//
//  Result type representing success or failure.
//

#import <Foundation/Foundation.h>
#import "GREYError.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * Result type representing success or failure.
 * Provides a type-safe way to handle operation results.
 */
@interface GREYResult<__covariant T> : NSObject

/// Whether the result is a success
@property (nonatomic, readonly) BOOL isSuccess;

/// Whether the result is a failure
@property (nonatomic, readonly) BOOL isFailure;

/// The success value (nil if failure)
@property (nonatomic, readonly, nullable) T value;

/// The error (nil if success)
@property (nonatomic, readonly, nullable) GREYError *error;

/**
 * Create a successful result with a value.
 */
+ (instancetype)ok:(T)value;

/**
 * Create a failed result with an error.
 */
+ (instancetype)fail:(GREYError *)error;

/**
 * Create a failed result with error components.
 */
+ (instancetype)failWithCode:(GREYErrorCode)code
                     message:(NSString *)message
                     details:(nullable NSString *)details;

/**
 * Get the value or a default if failed.
 */
- (T)getOr:(T)defaultValue;

/**
 * Map the success value to a new result.
 */
- (GREYResult *)map:(id (^)(T value))mapper;

/**
 * Flat map the success value to a new result.
 */
- (GREYResult *)flatMap:(GREYResult * (^)(T value))mapper;

/**
 * Execute a block on success.
 */
- (GREYResult<T> *)onSuccess:(void (^)(T value))block;

/**
 * Execute a block on failure.
 */
- (GREYResult<T> *)onFailure:(void (^)(GREYError *error))block;

/**
 * Pattern match on the result.
 */
- (id)matchSuccess:(id (^)(T value))onSuccess
           failure:(id (^)(GREYError *error))onFailure;

@end

NS_ASSUME_NONNULL_END
