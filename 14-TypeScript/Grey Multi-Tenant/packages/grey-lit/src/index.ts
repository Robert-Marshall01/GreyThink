/**
 * @grey/lit - Lit bindings for Grey Multi-Tenant
 *
 * Barrel exports for all controllers and provider.
 * No logic, only exports.
 */

// =============================================================================
// Controllers
// =============================================================================

export { AuthController, type GreyError } from './controllers/auth-controller.js';
export { UserController } from './controllers/user-controller.js';
export { ProjectsController } from './controllers/projects-controller.js';
export { QueryController } from './controllers/query-controller.js';
export { MutationController } from './controllers/mutation-controller.js';

// =============================================================================
// Provider
// =============================================================================

export { GreyProvider, greyContext, type GreyContextValue } from './provider/grey-provider.js';
