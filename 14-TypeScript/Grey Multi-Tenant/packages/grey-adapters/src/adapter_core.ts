/**
 * Grey Integration Layer - Adapter Core Interface
 *
 * Defines the adapter-core contract that all integrations call.
 * This is the single source of truth for domain logic.
 */

// ============================================================================
// Auth Domain
// ============================================================================

export interface LoginRequest {
  email: string;
  password: string;
  tenantId?: string;
}

export interface LoginResponse {
  accessToken: string;
  refreshToken: string;
  expiresIn: number;
}

export interface RefreshRequest {
  refreshToken: string;
}

export interface LogoutRequest {
  accessToken?: string;
}

// ============================================================================
// User Domain
// ============================================================================

export interface GetUserRequest {
  userId: string;
}

export interface User {
  userId: string;
  email: string;
  displayName?: string;
  tenantId?: string;
}

// ============================================================================
// Projects Domain
// ============================================================================

export interface ListProjectsRequest {
  limit?: number;
  offset?: number;
  tenantId?: string;
}

export interface Project {
  projectId: string;
  name: string;
  description?: string;
  tenantId?: string;
  createdAt?: string;
  updatedAt?: string;
}

export interface ListProjectsResponse {
  projects: Project[];
  total: number;
}

export interface CreateProjectRequest {
  name: string;
  description?: string;
  tenantId?: string;
}

// ============================================================================
// Query Domain
// ============================================================================

export interface QueryRequest {
  queryName: string;
  parameters: Record<string, unknown>;
  tenantId?: string;
}

export interface QueryResponse {
  data: unknown;
  metadata?: Record<string, unknown>;
}

// ============================================================================
// Mutation Domain
// ============================================================================

export interface MutationRequest {
  mutationName: string;
  parameters: Record<string, unknown>;
  tenantId?: string;
}

export interface MutationResponse {
  success: boolean;
  message?: string;
  data?: unknown;
  metadata?: Record<string, unknown>;
}

// ============================================================================
// Adapter Core Interface
// ============================================================================

export interface AdapterCore {
  // Auth
  login(request: LoginRequest): Promise<LoginResponse>;
  logout(request: LogoutRequest): Promise<void>;
  refresh(request: RefreshRequest): Promise<LoginResponse>;

  // User
  getUser(request: GetUserRequest): Promise<User>;
  getCurrentUser(): Promise<User>;

  // Projects
  listProjects(request: ListProjectsRequest): Promise<ListProjectsResponse>;
  createProject(request: CreateProjectRequest): Promise<Project>;

  // Query
  query(request: QueryRequest): Promise<QueryResponse>;

  // Mutation
  mutate(request: MutationRequest): Promise<MutationResponse>;
}

// ============================================================================
// Context
// ============================================================================

export interface RequestContext {
  authToken?: string;
  tenantId?: string;
  correlationId?: string;
  metadata?: Record<string, string>;
}
