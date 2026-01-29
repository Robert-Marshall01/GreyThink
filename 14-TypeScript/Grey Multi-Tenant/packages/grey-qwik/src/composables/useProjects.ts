/**
 * Grey Qwik - useProjects Composable
 *
 * Provides projects state and actions using Qwik's reactive primitives.
 * Must be used within a GreyProvider.
 *
 * @example
 * ```tsx
 * import { component$ } from '@builder.io/qwik';
 * import { useProjects } from '@grey/qwik';
 *
 * export default component$(() => {
 *   const { projects, isLoading, load$, create$ } = useProjects();
 *
 *   // Load projects on mount
 *   useVisibleTask$(async () => {
 *     await load$();
 *   });
 *
 *   return (
 *     <div>
 *       <button onClick$={() => create$({ name: 'New Project' })}>
 *         Create Project
 *       </button>
 *       {isLoading.value ? (
 *         <p>Loading...</p>
 *       ) : (
 *         <ul>
 *           {projects.value.map((project) => (
 *             <li key={project.id}>{project.name}</li>
 *           ))}
 *         </ul>
 *       )}
 *     </div>
 *   );
 * });
 * ```
 */

import { useSignal, useVisibleTask$, $ } from '@builder.io/qwik';
import type { ProjectsState as CoreProjectsState, ProjectState as CoreProjectState } from '@grey/adapters';
import type { Project, Pagination } from '@grey/core-client';
import { useGreyContext } from '../context/index.js';
import type { ProjectsComposable, ProjectComposable } from '../types/index.js';

/**
 * useProjects Composable
 *
 * Wraps the ProjectsController from @grey/adapters and exposes
 * Qwik signals for reactive state access.
 *
 * @returns Projects composable with state signals and actions
 *
 * @example
 * ```tsx
 * export default component$(() => {
 *   const { projects, pagination, load$ } = useProjects();
 *
 *   useVisibleTask$(async () => {
 *     await load$();
 *   });
 *
 *   return (
 *     <div>
 *       {projects.value.map((project) => (
 *         <ProjectCard key={project.id} project={project} />
 *       ))}
 *       {pagination.value && (
 *         <Pagination
 *           current={pagination.value.page}
 *           total={pagination.value.total_pages}
 *           onPageChange$={(page) => load$(page)}
 *         />
 *       )}
 *     </div>
 *   );
 * });
 * ```
 */
export function useProjects(): ProjectsComposable {
  const { projectsController } = useGreyContext();

  // Create signals with initial state from controller
  const state = useSignal<CoreProjectsState>(projectsController.getListState());
  const projects = useSignal<Project[]>(state.value.projects);
  const pagination = useSignal<Pagination | null>(state.value.pagination);
  const isLoading = useSignal(state.value.isLoading);
  const error = useSignal<string | null>(state.value.error);

  // Subscribe to controller list state changes (client-side only)
  useVisibleTask$(({ cleanup }: { cleanup: (fn: () => void) => void }) => {
    const unsubscribe = projectsController.subscribeToList((newState: CoreProjectsState) => {
      state.value = newState;
      projects.value = newState.projects;
      pagination.value = newState.pagination;
      isLoading.value = newState.isLoading;
      error.value = newState.error;
    });

    cleanup(() => {
      unsubscribe();
    });
  });

  /**
   * Load projects list with optional pagination
   */
  const load$ = $(async (page?: number, pageSize?: number): Promise<void> => {
    await projectsController.loadList(page, pageSize);
  });

  /**
   * Create a new project
   */
  const create$ = $(async (input: { name: string; description?: string }): Promise<Project | null> => {
    return projectsController.createProject(input);
  });

  /**
   * Clear current error
   */
  const clearError$ = $(() => {
    projectsController.clearListError();
  });

  return {
    state,
    projects,
    pagination,
    isLoading,
    error,
    load$,
    create$,
    clearError$,
  };
}

/**
 * useProject Composable
 *
 * Provides single project state using Qwik's reactive primitives.
 *
 * @param projectId - The ID of the project to track
 * @returns Project composable with state signals and actions
 *
 * @example
 * ```tsx
 * export default component$(({ id }: { id: string }) => {
 *   const { project, isLoading, load$ } = useProject(id);
 *
 *   useVisibleTask$(async () => {
 *     await load$();
 *   });
 *
 *   return (
 *     <div>
 *       {isLoading.value ? (
 *         <Spinner />
 *       ) : project.value ? (
 *         <div>
 *           <h1>{project.value.name}</h1>
 *           <p>{project.value.description}</p>
 *         </div>
 *       ) : null}
 *     </div>
 *   );
 * });
 * ```
 */
export function useProject(projectId: string): ProjectComposable {
  const { projectsController } = useGreyContext();

  // Create signals with initial state from controller
  const state = useSignal<CoreProjectState>(projectsController.getDetailState(projectId));
  const project = useSignal<Project | null>(state.value.project);
  const isLoading = useSignal(state.value.isLoading);
  const error = useSignal<string | null>(state.value.error);

  // Subscribe to controller detail state changes (client-side only)
  useVisibleTask$(({ cleanup }: { cleanup: (fn: () => void) => void }) => {
    const unsubscribe = projectsController.subscribeToDetail(projectId, (newState: CoreProjectState) => {
      state.value = newState;
      project.value = newState.project;
      isLoading.value = newState.isLoading;
      error.value = newState.error;
    });

    cleanup(() => {
      unsubscribe();
    });
  });

  /**
   * Load/reload project
   */
  const load$ = $(async (): Promise<Project | null> => {
    return projectsController.loadProject(projectId);
  });

  /**
   * Clear current error
   */
  const clearError$ = $(() => {
    projectsController.clearDetailError(projectId);
  });

  return {
    state,
    project,
    isLoading,
    error,
    load$,
    clearError$,
  };
}
