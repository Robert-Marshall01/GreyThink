//
//  GREYClient.m
//  GreySDK
//
//  Main client facade for the Grey Multi-Tenant SDK.
//

#import "GREYClient.h"
#import "GREYAuthService.h"
#import "GREYUserService.h"
#import "GREYProjectsService.h"
#import "GREYQueryService.h"
#import "GREYMutationService.h"

@interface GREYClient ()

@property (nonatomic, strong) GREYOptions *options;
@property (nonatomic, strong) GREYAuthService *authService;
@property (nonatomic, strong) GREYUserService *userService;
@property (nonatomic, strong) GREYProjectsService *projectsService;
@property (nonatomic, strong) GREYQueryService *queryService;
@property (nonatomic, strong) GREYMutationService *mutationService;

@property (nonatomic, strong) GREYAuthClient *auth;
@property (nonatomic, strong) GREYUserClient *user;
@property (nonatomic, strong) GREYProjectsClient *projects;
@property (nonatomic, strong) GREYQueryClient *query;
@property (nonatomic, strong) GREYMutationClient *mutation;

@end

@implementation GREYClient

- (instancetype)initWithOptions:(GREYOptions *)options {
    self = [super init];
    if (self) {
        _options = options;
        
        // Initialize gRPC services
        _authService = [[GREYAuthService alloc] initWithOptions:options];
        _userService = [[GREYUserService alloc] initWithOptions:options];
        _projectsService = [[GREYProjectsService alloc] initWithOptions:options];
        _queryService = [[GREYQueryService alloc] initWithOptions:options];
        _mutationService = [[GREYMutationService alloc] initWithOptions:options];
        
        // Initialize domain clients
        _auth = [[GREYAuthClient alloc] initWithService:_authService];
        _user = [[GREYUserClient alloc] initWithService:_userService];
        _projects = [[GREYProjectsClient alloc] initWithService:_projectsService];
        _query = [[GREYQueryClient alloc] initWithService:_queryService];
        _mutation = [[GREYMutationClient alloc] initWithService:_mutationService];
    }
    return self;
}

#pragma mark - Factory Methods

+ (instancetype)localClient {
    return [[self alloc] initWithOptions:[GREYOptions localOptions]];
}

+ (instancetype)localClientWithPort:(NSInteger)port {
    return [[self alloc] initWithOptions:[GREYOptions localOptionsWithPort:port]];
}

+ (instancetype)productionClientWithHost:(NSString *)host {
    return [[self alloc] initWithOptions:[GREYOptions productionOptionsWithHost:host]];
}

+ (instancetype)productionClientWithHost:(NSString *)host port:(NSInteger)port {
    return [[self alloc] initWithOptions:[GREYOptions productionOptionsWithHost:host port:port]];
}

#pragma mark - Token Management

- (void)setAuthToken:(NSString *)token {
    [_authService setAuthToken:token];
    [_userService setAuthToken:token];
    [_projectsService setAuthToken:token];
    [_queryService setAuthToken:token];
    [_mutationService setAuthToken:token];
}

- (void)clearAuthToken {
    [_authService clearAuthToken];
    [_userService clearAuthToken];
    [_projectsService clearAuthToken];
    [_queryService clearAuthToken];
    [_mutationService clearAuthToken];
}

@end
