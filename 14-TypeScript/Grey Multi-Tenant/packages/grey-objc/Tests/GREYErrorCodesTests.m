//
//  GREYErrorCodesTests.m
//  GreySDKTests
//
//  Unit tests for GREYErrorCodes.
//

#import <XCTest/XCTest.h>
#import "GREYErrorCodes.h"

@interface GREYErrorCodesTests : XCTestCase
@end

@implementation GREYErrorCodesTests

#pragma mark - HTTP Status Conversion Tests

- (void)testFromHTTPStatus401ReturnsUnauthorized {
    GREYErrorCode code = [GREYErrorCodes fromHTTPStatus:401];
    XCTAssertEqual(code, GREYErrorCodeUnauthorized);
}

- (void)testFromHTTPStatus403ReturnsForbidden {
    GREYErrorCode code = [GREYErrorCodes fromHTTPStatus:403];
    XCTAssertEqual(code, GREYErrorCodeForbidden);
}

- (void)testFromHTTPStatus404ReturnsNotFound {
    GREYErrorCode code = [GREYErrorCodes fromHTTPStatus:404];
    XCTAssertEqual(code, GREYErrorCodeNotFound);
}

- (void)testFromHTTPStatus422ReturnsValidationError {
    GREYErrorCode code = [GREYErrorCodes fromHTTPStatus:422];
    XCTAssertEqual(code, GREYErrorCodeValidationError);
}

- (void)testFromHTTPStatus500ReturnsServerError {
    GREYErrorCode code = [GREYErrorCodes fromHTTPStatus:500];
    XCTAssertEqual(code, GREYErrorCodeServerError);
}

- (void)testFromHTTPStatus503ReturnsServerError {
    GREYErrorCode code = [GREYErrorCodes fromHTTPStatus:503];
    XCTAssertEqual(code, GREYErrorCodeServerError);
}

- (void)testFromHTTPStatus999ReturnsUnknown {
    GREYErrorCode code = [GREYErrorCodes fromHTTPStatus:999];
    XCTAssertEqual(code, GREYErrorCodeUnknown);
}

#pragma mark - gRPC Status Conversion Tests

- (void)testFromGRPCStatus16ReturnsUnauthorized {
    GREYErrorCode code = [GREYErrorCodes fromGRPCStatus:16];
    XCTAssertEqual(code, GREYErrorCodeUnauthorized);
}

- (void)testFromGRPCStatus7ReturnsForbidden {
    GREYErrorCode code = [GREYErrorCodes fromGRPCStatus:7];
    XCTAssertEqual(code, GREYErrorCodeForbidden);
}

- (void)testFromGRPCStatus5ReturnsNotFound {
    GREYErrorCode code = [GREYErrorCodes fromGRPCStatus:5];
    XCTAssertEqual(code, GREYErrorCodeNotFound);
}

- (void)testFromGRPCStatus3ReturnsValidationError {
    GREYErrorCode code = [GREYErrorCodes fromGRPCStatus:3];
    XCTAssertEqual(code, GREYErrorCodeValidationError);
}

- (void)testFromGRPCStatus4ReturnsTimeout {
    GREYErrorCode code = [GREYErrorCodes fromGRPCStatus:4];
    XCTAssertEqual(code, GREYErrorCodeTimeout);
}

- (void)testFromGRPCStatus14ReturnsNetworkError {
    GREYErrorCode code = [GREYErrorCodes fromGRPCStatus:14];
    XCTAssertEqual(code, GREYErrorCodeNetworkError);
}

#pragma mark - Retryable Tests

- (void)testNetworkErrorIsRetryable {
    XCTAssertTrue([GREYErrorCodes isRetryable:GREYErrorCodeNetworkError]);
}

- (void)testTimeoutIsRetryable {
    XCTAssertTrue([GREYErrorCodes isRetryable:GREYErrorCodeTimeout]);
}

- (void)testServerErrorIsRetryable {
    XCTAssertTrue([GREYErrorCodes isRetryable:GREYErrorCodeServerError]);
}

- (void)testUnauthorizedIsNotRetryable {
    XCTAssertFalse([GREYErrorCodes isRetryable:GREYErrorCodeUnauthorized]);
}

- (void)testForbiddenIsNotRetryable {
    XCTAssertFalse([GREYErrorCodes isRetryable:GREYErrorCodeForbidden]);
}

- (void)testNotFoundIsNotRetryable {
    XCTAssertFalse([GREYErrorCodes isRetryable:GREYErrorCodeNotFound]);
}

- (void)testValidationErrorIsNotRetryable {
    XCTAssertFalse([GREYErrorCodes isRetryable:GREYErrorCodeValidationError]);
}

#pragma mark - String Conversion Tests

- (void)testStringForUnauthorized {
    NSString *str = [GREYErrorCodes stringForCode:GREYErrorCodeUnauthorized];
    XCTAssertEqualObjects(str, @"unauthorized");
}

- (void)testStringForNetworkError {
    NSString *str = [GREYErrorCodes stringForCode:GREYErrorCodeNetworkError];
    XCTAssertEqualObjects(str, @"network_error");
}

- (void)testMessageForUnauthorized {
    NSString *msg = [GREYErrorCodes messageForCode:GREYErrorCodeUnauthorized];
    XCTAssertTrue([msg containsString:@"Authentication"]);
}

@end
