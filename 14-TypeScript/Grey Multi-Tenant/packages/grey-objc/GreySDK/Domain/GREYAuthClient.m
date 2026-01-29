//
//  GREYAuthClient.m
//  GreySDK
//
//  Domain client for authentication operations.
//

#import "GREYAuthClient.h"

@implementation GREYAuthTokens
@end

@interface GREYAuthClient ()
@property (nonatomic, strong) GREYAuthService *service;
@end

@implementation GREYAuthClient

- (instancetype)initWithService:(GREYAuthService *)service {
    self = [super init];
    if (self) {
        _service = service;
    }
    return self;
}

#pragma mark - Validation

- (GREYError *)validateEmail:(NSString *)email {
    if (!email || email.length == 0) {
        return [GREYError validationErrorWithMessage:@"Email is required"];
    }
    
    // Basic email format validation
    NSString *emailPattern = @"^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$";
    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:emailPattern
                                                                           options:NSRegularExpressionCaseInsensitive
                                                                             error:nil];
    NSRange range = NSMakeRange(0, email.length);
    NSUInteger matches = [regex numberOfMatchesInString:email options:0 range:range];
    
    if (matches == 0) {
        return [GREYError validationErrorWithMessage:@"Invalid email format"];
    }
    
    return nil;
}

- (GREYError *)validatePassword:(NSString *)password {
    if (!password || password.length == 0) {
        return [GREYError validationErrorWithMessage:@"Password is required"];
    }
    
    if (password.length < 8) {
        return [GREYError validationErrorWithMessage:@"Password must be at least 8 characters"];
    }
    
    return nil;
}

- (GREYError *)validateRefreshToken:(NSString *)token {
    if (!token || token.length == 0) {
        return [GREYError validationErrorWithMessage:@"Refresh token is required"];
    }
    
    return nil;
}

#pragma mark - Public Methods

- (void)loginWithEmail:(NSString *)email
              password:(NSString *)password
              tenantId:(nullable NSString *)tenantId
            completion:(GREYAuthCompletion)completion {
    // Validate inputs
    GREYError *emailError = [self validateEmail:email];
    if (emailError) {
        completion([GREYResult fail:emailError]);
        return;
    }
    
    GREYError *passwordError = [self validatePassword:password];
    if (passwordError) {
        completion([GREYResult fail:passwordError]);
        return;
    }
    
    // Create request
    GREYLoginRequest *request = [GREYLoginRequest requestWithEmail:email
                                                          password:password
                                                          tenantId:tenantId];
    
    // Execute login
    [_service login:request completion:^(GREYResult *result) {
        if (result.isSuccess) {
            GREYLoginResponse *response = result.value;
            GREYAuthTokens *tokens = [[GREYAuthTokens alloc] init];
            tokens.accessToken = response.accessToken;
            tokens.refreshToken = response.refreshToken;
            tokens.expiresIn = response.expiresIn;
            completion([GREYResult ok:tokens]);
        } else {
            completion([GREYResult fail:result.error]);
        }
    }];
}

- (void)logoutWithCompletion:(GREYLogoutCompletion)completion {
    [_service logoutWithCompletion:^(GREYResult *result) {
        if (result.isSuccess) {
            completion([GREYResult ok:@YES]);
        } else {
            completion([GREYResult fail:result.error]);
        }
    }];
}

- (void)refreshWithToken:(NSString *)refreshToken
              completion:(GREYAuthCompletion)completion {
    // Validate input
    GREYError *tokenError = [self validateRefreshToken:refreshToken];
    if (tokenError) {
        completion([GREYResult fail:tokenError]);
        return;
    }
    
    // Create request
    GREYRefreshRequest *request = [GREYRefreshRequest requestWithRefreshToken:refreshToken];
    
    // Execute refresh
    [_service refresh:request completion:^(GREYResult *result) {
        if (result.isSuccess) {
            GREYLoginResponse *response = result.value;
            GREYAuthTokens *tokens = [[GREYAuthTokens alloc] init];
            tokens.accessToken = response.accessToken;
            tokens.refreshToken = response.refreshToken;
            tokens.expiresIn = response.expiresIn;
            completion([GREYResult ok:tokens]);
        } else {
            completion([GREYResult fail:result.error]);
        }
    }];
}

@end
