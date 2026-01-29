/**
 * Grey Integration Layer - Auth gRPC Service Implementation
 *
 * Implements the AuthService gRPC service.
 * Calls adapter-core directly, no business logic.
 */

import type { AdapterCore, LoginRequest, LoginResponse } from '../../adapter_core';
import { createGrpcError, GrpcStatus, type GrpcError } from '../grpc_error';

// Proto message types (would be generated from proto files)
export interface ProtoLoginRequest {
  email: string;
  password: string;
  tenantId?: string;
}

export interface ProtoLoginResponse {
  accessToken: string;
  refreshToken: string;
  expiresIn: number;
}

export interface ProtoLogoutRequest {
  accessToken?: string;
}

export interface ProtoRefreshRequest {
  refreshToken: string;
}

export interface ProtoEmpty {}

// Service call context
export interface ServiceContext {
  metadata?: Map<string, string>;
  authToken?: string;
  tenantId?: string;
}

/**
 * Auth service implementation.
 */
export class AuthServiceImpl {
  constructor(private readonly core: AdapterCore) {}

  /**
   * Login with email and password.
   */
  async login(request: ProtoLoginRequest, context: ServiceContext): Promise<ProtoLoginResponse> {
    try {
      const response = await this.core.login({
        email: request.email,
        password: request.password,
        tenantId: request.tenantId,
      });

      return {
        accessToken: response.accessToken,
        refreshToken: response.refreshToken,
        expiresIn: response.expiresIn,
      };
    } catch (error) {
      throw createGrpcError(error);
    }
  }

  /**
   * Logout current session.
   */
  async logout(request: ProtoLogoutRequest, context: ServiceContext): Promise<ProtoEmpty> {
    try {
      await this.core.logout({
        accessToken: request.accessToken || context.authToken,
      });

      return {};
    } catch (error) {
      throw createGrpcError(error);
    }
  }

  /**
   * Refresh authentication token.
   */
  async refresh(request: ProtoRefreshRequest, context: ServiceContext): Promise<ProtoLoginResponse> {
    try {
      const response = await this.core.refresh({
        refreshToken: request.refreshToken,
      });

      return {
        accessToken: response.accessToken,
        refreshToken: response.refreshToken,
        expiresIn: response.expiresIn,
      };
    } catch (error) {
      throw createGrpcError(error);
    }
  }
}

/**
 * Create Auth service handlers for gRPC server registration.
 */
export function createAuthServiceHandlers(core: AdapterCore) {
  const service = new AuthServiceImpl(core);

  return {
    login: service.login.bind(service),
    logout: service.logout.bind(service),
    refresh: service.refresh.bind(service),
  };
}
