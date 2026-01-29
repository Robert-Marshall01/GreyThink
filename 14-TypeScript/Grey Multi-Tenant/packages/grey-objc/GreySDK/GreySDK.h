//
//  GreySDK.h
//  GreySDK
//
//  Umbrella header for the Grey Multi-Tenant SDK.
//
//  Import this header to access all SDK functionality:
//  #import <GreySDK/GreySDK.h>
//

#ifndef GreySDK_h
#define GreySDK_h

// Version
#define GREY_SDK_VERSION @"1.0.0"
#define GREY_SDK_VERSION_MAJOR 1
#define GREY_SDK_VERSION_MINOR 0
#define GREY_SDK_VERSION_PATCH 0

// Error handling
#import "Error/GREYErrorCodes.h"
#import "Error/GREYError.h"
#import "Error/GREYResult.h"

// Configuration
#import "Config/GREYOptions.h"

// gRPC Services
#import "GRPC/GREYGRPCClient.h"
#import "GRPC/GREYAuthService.h"
#import "GRPC/GREYUserService.h"
#import "GRPC/GREYProjectsService.h"
#import "GRPC/GREYQueryService.h"
#import "GRPC/GREYMutationService.h"

// Domain Clients
#import "Domain/GREYAuthClient.h"
#import "Domain/GREYUserClient.h"
#import "Domain/GREYProjectsClient.h"
#import "Domain/GREYQueryClient.h"
#import "Domain/GREYMutationClient.h"

// Main Client
#import "GREYClient.h"

#endif /* GreySDK_h */
