/**
 * Grey Integration Layer - User gRPC Service Implementation
 *
 * Implements the UserService gRPC service.
 * Calls adapter-core directly, no business logic.
 */

import type { AdapterCore, User } from '../../adapter_core';
import { createGrpcError, type GrpcError } from '../grpc_error';

// Proto message types
export interface ProtoEmpty {}

export interface ProtoGetUserRequest {
  userId: string;
}

export interface ProtoUser {
  userId: string;
  email: string;
  displayName?: string;
  tenantId?: string;
}

// Service call context
export interface ServiceContext {
  metadata?: Map<string, string>;
  authToken?: string;
  tenantId?: string;
}

/**
 * User service implementation.
 */
export class UserServiceImpl {
  constructor(private readonly core: AdapterCore) {}

  /**
   * Get the current authenticated user.
   */
  async getCurrentUser(request: ProtoEmpty, context: ServiceContext): Promise<ProtoUser> {
    try {
      const user = await this.core.getCurrentUser();

      return {
        userId: user.userId,
        email: user.email,
        displayName: user.displayName,
        tenantId: user.tenantId,
      };
    } catch (error) {
      throw createGrpcError(error);
    }
  }

  /**
   * Get a user by ID.
   */
  async getUser(request: ProtoGetUserRequest, context: ServiceContext): Promise<ProtoUser> {
    try {
      const user = await this.core.getUser({
        userId: request.userId,
      });

      return {
        userId: user.userId,
        email: user.email,
        displayName: user.displayName,
        tenantId: user.tenantId,
      };
    } catch (error) {
      throw createGrpcError(error);
    }
  }
}

/**
 * Create User service handlers for gRPC server registration.
 */
export function createUserServiceHandlers(core: AdapterCore) {
  const service = new UserServiceImpl(core);

  return {
    getCurrentUser: service.getCurrentUser.bind(service),
    getUser: service.getUser.bind(service),
  };
}
