/**
 * Grey Provider - Angular DI provider configuration
 *
 * Provides all Grey services using Angular's dependency injection system.
 * SSR-safe, no UI logic.
 */

import { makeEnvironmentProviders, InjectionToken, type EnvironmentProviders } from '@angular/core';
import { AuthService, type AuthServiceConfig } from '../services/auth.service';
import { UserService } from '../services/user.service';
import { ProjectsService } from '../services/projects.service';
import { QueryService } from '../services/query.service';
import { MutationService } from '../services/mutation.service';

// =============================================================================
// Configuration Token
// =============================================================================

export const GREY_CONFIG = new InjectionToken<GreyConfig>('GreyConfig');

// =============================================================================
// Configuration Interface
// =============================================================================

export interface GreyConfig {
  /** Base URL for the Grey API */
  baseUrl: string;
  /** Callback when authentication state changes */
  onAuthChange?: (user: unknown) => void;
  /** Callback when user logs out */
  onLogout?: () => void;
}

// =============================================================================
// Provider Function
// =============================================================================

/**
 * Provide all Grey services for Angular applications.
 *
 * Use this function in your application configuration to register
 * all Grey Multi-Tenant services with Angular's dependency injection.
 *
 * @example Standalone Components (Angular 17+)
 * ```typescript
 * // app.config.ts
 * import { provideGrey } from '@grey/angular';
 *
 * export const appConfig: ApplicationConfig = {
 *   providers: [
 *     provideGrey({
 *       baseUrl: 'http://localhost:8080/api/v1',
 *       onLogout: () => router.navigate(['/login']),
 *     }),
 *   ],
 * };
 * ```
 *
 * @example NgModule-based (Angular 16 and earlier)
 * ```typescript
 * // app.module.ts
 * import { provideGrey } from '@grey/angular';
 *
 * @NgModule({
 *   providers: [
 *     provideGrey({
 *       baseUrl: environment.greyApiUrl,
 *     }),
 *   ],
 * })
 * export class AppModule {}
 * ```
 */
export function provideGrey(config: GreyConfig): EnvironmentProviders {
  return makeEnvironmentProviders([
    { provide: GREY_CONFIG, useValue: config },
    AuthService,
    UserService,
    ProjectsService,
    QueryService,
    MutationService,
  ]);
}

// =============================================================================
// Legacy Array Provider (for compatibility)
// =============================================================================

/**
 * Get an array of Grey service providers.
 *
 * Use this for scenarios where you need individual provider entries
 * rather than EnvironmentProviders.
 *
 * @param config - Grey configuration options
 * @returns Array of provider entries
 *
 * @example
 * ```typescript
 * const providers = getGreyProviders({
 *   baseUrl: 'http://localhost:8080/api/v1',
 * });
 * ```
 */
export function getGreyProviders(config: GreyConfig) {
  return [
    { provide: GREY_CONFIG, useValue: config },
    AuthService,
    UserService,
    ProjectsService,
    QueryService,
    MutationService,
  ];
}
