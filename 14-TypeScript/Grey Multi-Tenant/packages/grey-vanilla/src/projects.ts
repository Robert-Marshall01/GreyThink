/**
 * Grey Vanilla - Projects Module
 * 
 * Vanilla JS state container for projects management.
 * Wraps the @grey/adapters ProjectsController with a simple listener pattern.
 * 
 * @example
 * ```js
 * import { createProjectsContainer } from '@grey/vanilla';
 * 
 * const projects = createProjectsContainer({ client: greyClient });
 * 
 * // Subscribe to state changes
 * const unsubscribe = projects.subscribe((state) => {
 *   console.log('Projects:', state.projects);
 *   console.log('Pagination:', state.pagination);
 * });
 * 
 * // Fetch projects list
 * await projects.listProjects();
 * 
 * // Create a new project
 * const project = await projects.createProject({
 *   name: 'New Project',
 *   description: 'Project description',
 * });
 * 
 * // Cleanup
 * unsubscribe();
 * ```
 */

import {
  ProjectsController as CoreProjectsController,
  type ProjectsState,
  type ProjectState,
  type CreateProjectInput,
} from '@grey/adapters';
import type { GreyClient, Project, Pagination } from '@grey/core-client';
import { isBrowser, type Listener, type Unsubscribe } from './types.js';

/**
 * Options for creating a projects container
 */
export interface ProjectsContainerOptions {
  /** The Grey client instance (from AuthContainer) */
  client: GreyClient;
  /** Whether to auto-fetch projects list on creation (default: false) */
  autoFetch?: boolean;
  /** Default page size for list pagination */
  pageSize?: number;
}

/**
 * Projects container interface
 * Provides reactive projects state and actions
 */
export interface ProjectsContainer {
  // ============================================================
  // State Access
  // ============================================================
  
  /** Get the current projects list state snapshot */
  getState(): ProjectsState;
  
  /** Subscribe to projects list state changes */
  subscribe(listener: Listener<ProjectsState>): Unsubscribe;
  
  /**
   * Get detail state for a specific project
   * @param id - Project ID
   */
  getProjectState(id: string): ProjectState;
  
  /**
   * Subscribe to project detail state changes
   * @param id - Project ID
   * @param listener - State change listener
   */
  subscribeToProject(id: string, listener: Listener<ProjectState>): Unsubscribe;
  
  // ============================================================
  // State Properties (convenience getters)
  // ============================================================
  
  /** The projects list */
  readonly projects: Project[];
  
  /** Pagination info for the projects list */
  readonly pagination: Pagination | null;
  
  /** Whether projects are being loaded */
  readonly loading: boolean;
  
  /** Current error message (if any) */
  readonly error: string | null;
  
  // ============================================================
  // Actions
  // ============================================================
  
  /**
   * Load projects list
   * @param page - Page number (default: 1)
   * @param pageSize - Items per page (default: from options or 20)
   */
  listProjects(page?: number, pageSize?: number): Promise<void>;
  
  /** Refetch projects list (alias for listProjects) */
  refetch(): Promise<void>;
  
  /**
   * Load a single project by ID
   * @param id - Project ID
   * @returns Promise resolving to Project or null
   */
  loadProject(id: string): Promise<Project | null>;
  
  /**
   * Create a new project
   * @param input - Project creation data
   * @returns Promise resolving to the created Project or null
   */
  createProject(input: CreateProjectInput): Promise<Project | null>;
  
  /** Clear list error */
  clearListError(): void;
  
  /**
   * Clear detail error for a specific project
   * @param id - Project ID
   */
  clearDetailError(id: string): void;
}

/**
 * Create a projects container
 * 
 * Provides a simple observable state container for projects data
 * that wraps the @grey/adapters ProjectsController.
 * 
 * @param options - Configuration options
 * @returns ProjectsContainer instance
 * 
 * @example
 * ```js
 * const projects = createProjectsContainer({
 *   client: auth.getClient(),
 *   autoFetch: true,
 *   pageSize: 10,
 * });
 * 
 * // Subscribe to changes
 * projects.subscribe((state) => {
 *   renderProjectsList(state.projects);
 * });
 * 
 * // Create a project
 * const newProject = await projects.createProject({
 *   name: 'My Project',
 * });
 * ```
 */
export function createProjectsContainer(options: ProjectsContainerOptions): ProjectsContainer {
  // Internal state
  let _listState: ProjectsState;
  const listListeners = new Set<Listener<ProjectsState>>();
  const detailListeners = new Map<string, Set<Listener<ProjectState>>>();
  
  // Initialize core controller
  const coreController = new CoreProjectsController(options.client);
  _listState = coreController.getListState();
  
  // Subscribe to core controller list state changes
  coreController.subscribeToList((state: ProjectsState) => {
    _listState = state;
    notifyListListeners();
  });
  
  /**
   * Notify all list listeners of state change
   */
  function notifyListListeners(): void {
    listListeners.forEach((listener) => listener(_listState));
  }
  
  // Auto-fetch if enabled and in browser
  if (options.autoFetch && isBrowser()) {
    coreController.loadList(1, options.pageSize ?? 20).catch(() => {
      // Fetch failed
    });
  }
  
  // Return the container interface
  const container: ProjectsContainer = {
    // State access
    getState() {
      return { ..._listState };
    },
    
    subscribe(listener: Listener<ProjectsState>): Unsubscribe {
      listListeners.add(listener);
      // Immediately call with current state
      listener(_listState);
      return () => listListeners.delete(listener);
    },
    
    getProjectState(id: string): ProjectState {
      return coreController.getDetailState(id);
    },
    
    subscribeToProject(id: string, listener: Listener<ProjectState>): Unsubscribe {
      // Track detail listener internally
      if (!detailListeners.has(id)) {
        detailListeners.set(id, new Set());
      }
      detailListeners.get(id)!.add(listener);
      
      // Subscribe to core controller
      const unsubscribe = coreController.subscribeToDetail(id, listener);
      
      return () => {
        detailListeners.get(id)?.delete(listener);
        unsubscribe();
      };
    },
    
    // Convenience getters
    get projects() {
      return _listState.projects;
    },
    
    get pagination() {
      return _listState.pagination;
    },
    
    get loading() {
      return _listState.isLoading;
    },
    
    get error() {
      return _listState.error;
    },
    
    // Actions
    listProjects(page = 1, pageSize?: number): Promise<void> {
      return coreController.loadList(page, pageSize ?? options.pageSize ?? 20);
    },
    
    refetch(): Promise<void> {
      return coreController.loadList(1, options.pageSize ?? 20);
    },
    
    loadProject(id: string): Promise<Project | null> {
      return coreController.loadProject(id);
    },
    
    createProject(input: CreateProjectInput): Promise<Project | null> {
      return coreController.createProject(input);
    },
    
    clearListError(): void {
      coreController.clearListError();
    },
    
    clearDetailError(id: string): void {
      coreController.clearDetailError(id);
    },
  };
  
  return container;
}

// Re-export related types
export type { ProjectsState, ProjectState, CreateProjectInput, UpdateProjectInput } from '@grey/adapters';
export { initialProjectsState, initialProjectState, createProjectsController } from '@grey/adapters';
