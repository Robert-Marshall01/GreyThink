//
//  GREYOptionsTests.m
//  GreySDKTests
//
//  Unit tests for GREYOptions.
//

#import <XCTest/XCTest.h>
#import "GREYOptions.h"

@interface GREYOptionsTests : XCTestCase
@end

@implementation GREYOptionsTests

#pragma mark - Default Values Tests

- (void)testDefaultValuesAreCorrect {
    GREYOptions *options = [[GREYOptions alloc] init];
    
    XCTAssertEqualObjects(options.host, @"localhost");
    XCTAssertEqual(options.port, 50051);
    XCTAssertFalse(options.useTLS);
    XCTAssertEqual(options.timeoutSeconds, 30.0);
    XCTAssertNil(options.authToken);
}

#pragma mark - Factory Methods Tests

- (void)testLocalOptionsCreatesCorrectConfig {
    GREYOptions *options = [GREYOptions localOptions];
    
    XCTAssertEqualObjects(options.host, @"localhost");
    XCTAssertEqual(options.port, 50051);
    XCTAssertFalse(options.useTLS);
}

- (void)testLocalOptionsWithPortCreatesCorrectConfig {
    GREYOptions *options = [GREYOptions localOptionsWithPort:8080];
    
    XCTAssertEqualObjects(options.host, @"localhost");
    XCTAssertEqual(options.port, 8080);
    XCTAssertFalse(options.useTLS);
}

- (void)testProductionOptionsCreatesCorrectConfig {
    GREYOptions *options = [GREYOptions productionOptionsWithHost:@"api.example.com"];
    
    XCTAssertEqualObjects(options.host, @"api.example.com");
    XCTAssertEqual(options.port, 443);
    XCTAssertTrue(options.useTLS);
}

- (void)testProductionOptionsWithPortCreatesCorrectConfig {
    GREYOptions *options = [GREYOptions productionOptionsWithHost:@"api.example.com" port:8443];
    
    XCTAssertEqualObjects(options.host, @"api.example.com");
    XCTAssertEqual(options.port, 8443);
    XCTAssertTrue(options.useTLS);
}

#pragma mark - Endpoint Tests

- (void)testEndpointFormatsCorrectly {
    GREYOptions *options = [GREYOptions localOptions];
    
    NSString *endpoint = [options endpoint];
    XCTAssertEqualObjects(endpoint, @"localhost:50051");
}

- (void)testEndpointWithCustomPort {
    GREYOptions *options = [GREYOptions productionOptionsWithHost:@"api.grey.io" port:9000];
    
    NSString *endpoint = [options endpoint];
    XCTAssertEqualObjects(endpoint, @"api.grey.io:9000");
}

#pragma mark - Builder Methods Tests

- (void)testWithAuthTokenReturnsNewInstance {
    GREYOptions *original = [GREYOptions localOptions];
    GREYOptions *withToken = [original withAuthToken:@"test-token"];
    
    XCTAssertNotEqual(original, withToken);
    XCTAssertNil(original.authToken);
    XCTAssertEqualObjects(withToken.authToken, @"test-token");
}

- (void)testWithTimeoutReturnsNewInstance {
    GREYOptions *original = [GREYOptions localOptions];
    GREYOptions *withTimeout = [original withTimeout:60.0];
    
    XCTAssertNotEqual(original, withTimeout);
    XCTAssertEqual(original.timeoutSeconds, 30.0);
    XCTAssertEqual(withTimeout.timeoutSeconds, 60.0);
}

- (void)testWithHeaderReturnsNewInstance {
    GREYOptions *original = [GREYOptions localOptions];
    GREYOptions *withHeader = [original withHeader:@"X-Custom" value:@"test"];
    
    XCTAssertNotEqual(original, withHeader);
    XCTAssertEqual(original.customHeaders.count, 0);
    XCTAssertEqualObjects(withHeader.customHeaders[@"X-Custom"], @"test");
}

- (void)testWithHeaderPreservesExistingHeaders {
    GREYOptions *original = [[GREYOptions localOptions] withHeader:@"X-First" value:@"1"];
    GREYOptions *withSecond = [original withHeader:@"X-Second" value:@"2"];
    
    XCTAssertEqual(withSecond.customHeaders.count, 2);
    XCTAssertEqualObjects(withSecond.customHeaders[@"X-First"], @"1");
    XCTAssertEqualObjects(withSecond.customHeaders[@"X-Second"], @"2");
}

#pragma mark - Copying Tests

- (void)testCopyCreatesIndependentInstance {
    GREYOptions *original = [GREYOptions productionOptionsWithHost:@"api.grey.io"];
    original.authToken = @"token123";
    
    GREYOptions *copy = [original copy];
    copy.authToken = @"different";
    copy.host = @"other.grey.io";
    
    XCTAssertEqualObjects(original.authToken, @"token123");
    XCTAssertEqualObjects(original.host, @"api.grey.io");
    XCTAssertEqualObjects(copy.authToken, @"different");
    XCTAssertEqualObjects(copy.host, @"other.grey.io");
}

@end
