/**
 * Grey Integration Layer - Mutation gRPC Service Implementation
 *
 * Implements the MutationService gRPC service.
 * Calls adapter-core directly, no business logic.
 */

import type { AdapterCore, MutationResponse } from '../../adapter_core';
import { createGrpcError, type GrpcError } from '../grpc_error';

// Proto message types
export interface ProtoMutationRequest {
  mutationName: string;
  parameters: Record<string, unknown>;
  tenantId?: string;
}

export interface ProtoMutationResponse {
  success: boolean;
  message?: string;
  data?: Record<string, unknown>;
  metadata?: Record<string, unknown>;
}

// Service call context
export interface ServiceContext {
  metadata?: Map<string, string>;
  authToken?: string;
  tenantId?: string;
}

/**
 * Mutation service implementation.
 */
export class MutationServiceImpl {
  constructor(private readonly core: AdapterCore) {}

  /**
   * Execute a mutation.
   */
  async mutate(
    request: ProtoMutationRequest,
    context: ServiceContext
  ): Promise<ProtoMutationResponse> {
    try {
      const response = await this.core.mutate({
        mutationName: request.mutationName,
        parameters: request.parameters,
        tenantId: request.tenantId || context.tenantId,
      });

      return {
        success: response.success,
        message: response.message,
        data: response.data as Record<string, unknown> | undefined,
        metadata: response.metadata,
      };
    } catch (error) {
      throw createGrpcError(error);
    }
  }
}

/**
 * Create Mutation service handlers for gRPC server registration.
 */
export function createMutationServiceHandlers(core: AdapterCore) {
  const service = new MutationServiceImpl(core);

  return {
    mutate: service.mutate.bind(service),
  };
}
