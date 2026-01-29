/**
 * Grey Integration Layer - Query gRPC Service Implementation
 *
 * Implements the QueryService gRPC service.
 * Calls adapter-core directly, no business logic.
 */

import type { AdapterCore, QueryResponse } from '../../adapter_core';
import { createGrpcError, type GrpcError } from '../grpc_error';

// Proto message types
export interface ProtoQueryRequest {
  queryName: string;
  parameters: Record<string, unknown>;
  tenantId?: string;
}

export interface ProtoQueryResponse {
  data: Record<string, unknown>;
  metadata?: Record<string, unknown>;
}

// Service call context
export interface ServiceContext {
  metadata?: Map<string, string>;
  authToken?: string;
  tenantId?: string;
}

/**
 * Query service implementation.
 */
export class QueryServiceImpl {
  constructor(private readonly core: AdapterCore) {}

  /**
   * Execute a query.
   */
  async query(request: ProtoQueryRequest, context: ServiceContext): Promise<ProtoQueryResponse> {
    try {
      const response = await this.core.query({
        queryName: request.queryName,
        parameters: request.parameters,
        tenantId: request.tenantId || context.tenantId,
      });

      return {
        data: response.data as Record<string, unknown>,
        metadata: response.metadata,
      };
    } catch (error) {
      throw createGrpcError(error);
    }
  }
}

/**
 * Create Query service handlers for gRPC server registration.
 */
export function createQueryServiceHandlers(core: AdapterCore) {
  const service = new QueryServiceImpl(core);

  return {
    query: service.query.bind(service),
  };
}
