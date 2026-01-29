/**
 * Grey Solid - Projects Signal
 *
 * Provides projects state and actions as SolidJS signals.
 * Must be used within a GreyProvider.
 *
 * @example
 * ```tsx
 * import { createProjectsSignal } from '@grey/solid';
 *
 * function ProjectsList() {
 *   const { projects, isLoading, load, create } = createProjectsSignal();
 *
 *   onMount(() => {
 *     load();
 *   });
 *
 *   return (
 *     <div>
 *       <button onClick={() => create({ name: 'New Project' })}>
 *         Create Project
 *       </button>
 *       <Show when={!isLoading()} fallback={<Spinner />}>
 *         <For each={projects()}>
 *           {(project) => <ProjectCard project={project} />}
 *         </For>
 *       </Show>
 *     </div>
 *   );
 * }
 * ```
 */

import { createSignal, onCleanup } from 'solid-js';
import type { ProjectsState as CoreProjectsState, ProjectState as CoreProjectState } from '@grey/adapters';
import type { Project, Pagination } from '@grey/core-client';
import { useGreyContext } from '../context/index.js';
import type { ProjectsSignal, ProjectSignal } from '../types/index.js';

/**
 * Create reactive projects list signal
 *
 * Wraps the ProjectsController from @grey/adapters and exposes
 * SolidJS signals for reactive state access.
 *
 * @returns Projects signal with state accessors and actions
 *
 * @example
 * ```tsx
 * function ProjectsPage() {
 *   const { projects, pagination, load } = createProjectsSignal();
 *
 *   onMount(() => load());
 *
 *   return (
 *     <div>
 *       <For each={projects()}>
 *         {(project) => <div>{project.name}</div>}
 *       </For>
 *       <Show when={pagination()}>
 *         <Pagination
 *           current={pagination()!.page}
 *           total={pagination()!.total_pages}
 *           onPageChange={(page) => load(page)}
 *         />
 *       </Show>
 *     </div>
 *   );
 * }
 * ```
 */
export function createProjectsSignal(): ProjectsSignal {
  const { projectsController } = useGreyContext();

  // Create signal with initial state from controller
  const [state, setState] = createSignal<CoreProjectsState>(projectsController.getListState());

  // Subscribe to controller list state changes
  const unsubscribe = projectsController.subscribeToList((newState: CoreProjectsState) => {
    setState(newState);
  });

  // Cleanup subscription on unmount
  onCleanup(() => {
    unsubscribe();
  });

  // Derived accessors
  const projects = () => state().projects;
  const pagination = () => state().pagination;
  const isLoading = () => state().isLoading;
  const error = () => state().error;

  /**
   * Load projects list with optional pagination
   */
  async function load(page?: number, pageSize?: number): Promise<void> {
    await projectsController.loadList(page, pageSize);
  }

  /**
   * Create a new project
   */
  async function create(input: { name: string; description?: string }): Promise<Project | null> {
    return projectsController.createProject(input);
  }

  /**
   * Clear current error
   */
  function clearError(): void {
    projectsController.clearListError();
  }

  return {
    state,
    projects,
    pagination,
    isLoading,
    error,
    load,
    create,
    clearError,
  };
}

/**
 * Create reactive single project signal
 *
 * @param projectId - The ID of the project to track
 * @returns Project signal with state accessors and actions
 *
 * @example
 * ```tsx
 * function ProjectDetail(props: { id: string }) {
 *   const { project, isLoading, load } = createProjectSignal(props.id);
 *
 *   onMount(() => load());
 *
 *   return (
 *     <Show when={!isLoading() && project()}>
 *       <h1>{project()!.name}</h1>
 *       <p>{project()!.description}</p>
 *     </Show>
 *   );
 * }
 * ```
 */
export function createProjectSignal(projectId: string): ProjectSignal {
  const { projectsController } = useGreyContext();

  // Create signal with initial state from controller
  const [state, setState] = createSignal<CoreProjectState>(
    projectsController.getDetailState(projectId)
  );

  // Subscribe to controller detail state changes
  const unsubscribe = projectsController.subscribeToDetail(projectId, (newState: CoreProjectState) => {
    setState(newState);
  });

  // Cleanup subscription on unmount
  onCleanup(() => {
    unsubscribe();
  });

  // Derived accessors
  const project = () => state().project;
  const isLoading = () => state().isLoading;
  const error = () => state().error;

  /**
   * Load/reload project
   */
  async function load(): Promise<Project | null> {
    return projectsController.loadProject(projectId);
  }

  /**
   * Clear current error
   */
  function clearError(): void {
    projectsController.clearDetailError(projectId);
  }

  return {
    state,
    project,
    isLoading,
    error,
    load,
    clearError,
  };
}

/**
 * Hook to access projects list from context
 */
export function useProjects(): ProjectsSignal {
  return createProjectsSignal();
}

/**
 * Hook to access a single project from context
 */
export function useProject(projectId: string): ProjectSignal {
  return createProjectSignal(projectId);
}
