# Grey Angular

Angular services for the Grey Multi-Tenant platform.

## Installation

```bash
npm install @grey/angular @grey/core-client
```

## Quick Start

```typescript
// app.component.ts
import { Component, OnInit } from '@angular/core';
import { GreyService } from '@grey/angular';

@Component({
  selector: 'app-root',
  template: `<router-outlet></router-outlet>`,
})
export class AppComponent implements OnInit {
  constructor(private grey: GreyService) {}

  ngOnInit() {
    this.grey.init({ apiBaseUrl: 'http://localhost:8080/api/v1' });
    this.grey.restoreSession();
  }
}
```

```typescript
// dashboard.component.ts
import { Component, OnInit } from '@angular/core';
import { GreyService } from '@grey/angular';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

@Component({
  selector: 'app-dashboard',
  template: `
    <div *ngIf="!(isAuthenticated$ | async)">
      <app-login></app-login>
    </div>
    <div *ngIf="isAuthenticated$ | async">
      <h1>Welcome, {{ (user$ | async)?.name }}</h1>
      <button (click)="logout()">Logout</button>
      <app-project-list></app-project-list>
    </div>
  `,
})
export class DashboardComponent implements OnInit {
  isAuthenticated$: Observable<boolean>;
  user$: Observable<any>;

  constructor(private grey: GreyService) {
    this.isAuthenticated$ = this.grey.auth$.pipe(map(s => s.isAuthenticated));
    this.user$ = this.grey.auth$.pipe(map(s => s.user));
  }

  ngOnInit() {
    this.grey.loadProjects();
  }

  logout() {
    this.grey.logout();
  }
}
```

## Services

### GreyService

Main service providing auth, user, and projects functionality.

```typescript
@Injectable({ providedIn: 'root' })
class GreyService {
  // Observables
  auth$: Observable<AuthState>;
  user$: Observable<UserState>;
  projects$: Observable<ProjectsState>;

  // Initialization
  init(config: AuthConfig): void;

  // Auth methods
  login(email: string, password: string): Promise<boolean>;
  logout(): void;
  restoreSession(): Promise<boolean>;
  clearAuthError(): void;

  // User methods
  refreshUser(): Promise<User | null>;
  clearUserError(): void;

  // Projects methods
  loadProjects(page?: number, pageSize?: number): Promise<void>;
  createProject(input: CreateProjectInput): Promise<Project | null>;
  loadProject(id: string): Promise<Project | null>;
  getProjectState$(id: string): Observable<ProjectState>;
  clearProjectsError(): void;
  clearProjectError(id: string): void;

  // Raw client
  client: GreyClient;
}
```

### GreyQueryService

Service for creating custom query and mutation observables.

```typescript
@Injectable({ providedIn: 'root' })
class GreyQueryService {
  createQuery<T>(options: QueryOptions<T>): {
    state$: Observable<QueryState<T>>;
    refetch: () => Promise<T | null>;
  };

  createMutation<TData, TVariables>(options: MutationOptions<TData, TVariables>): {
    state$: Observable<MutationState<TData>>;
    mutate: (variables: TVariables) => Promise<TData | null>;
    reset: () => void;
  };
}
```

## Usage Examples

### Authentication

```typescript
import { GreyService } from '@grey/angular';

@Component({...})
export class LoginComponent {
  email = '';
  password = '';
  error$ = this.grey.auth$.pipe(map(s => s.error));
  isLoading$ = this.grey.auth$.pipe(map(s => s.isLoading));

  constructor(private grey: GreyService, private router: Router) {}

  async login() {
    const success = await this.grey.login(this.email, this.password);
    if (success) {
      this.router.navigate(['/dashboard']);
    }
  }
}
```

### Projects List

```typescript
import { GreyService } from '@grey/angular';

@Component({
  template: `
    <div *ngIf="isLoading$ | async">Loading...</div>
    <ul>
      <li *ngFor="let project of projects$ | async">
        {{ project.name }}
      </li>
    </ul>
    <button (click)="loadMore()">Load More</button>
  `,
})
export class ProjectListComponent implements OnInit {
  projects$ = this.grey.projects$.pipe(map(s => s.projects));
  isLoading$ = this.grey.projects$.pipe(map(s => s.isLoading));
  pagination$ = this.grey.projects$.pipe(map(s => s.pagination));
  
  private currentPage = 1;

  constructor(private grey: GreyService) {}

  ngOnInit() {
    this.grey.loadProjects(1, 20);
  }

  loadMore() {
    this.currentPage++;
    this.grey.loadProjects(this.currentPage, 20);
  }
}
```

### Custom Query

```typescript
import { GreyService, GreyQueryService } from '@grey/angular';

@Component({...})
export class OrganizationComponent implements OnInit {
  orgState$!: Observable<QueryState<Organization>>;
  
  constructor(
    private grey: GreyService,
    private queryService: GreyQueryService
  ) {}

  ngOnInit() {
    const query = this.queryService.createQuery({
      queryFn: () => this.grey.client.organizations.get('org-id'),
    });
    this.orgState$ = query.state$;
  }
}
```

### Mutation

```typescript
import { GreyService, GreyQueryService, CreateProjectInput } from '@grey/angular';

@Component({...})
export class CreateProjectComponent {
  private createMutation = this.queryService.createMutation({
    mutationFn: (data: CreateProjectInput) => this.grey.client.projects.create(data),
    onSuccess: () => {
      this.grey.loadProjects(); // Refresh list
      this.router.navigate(['/projects']);
    },
  });

  isLoading$ = this.createMutation.state$.pipe(map(s => s.isLoading));
  error$ = this.createMutation.state$.pipe(map(s => s.error));

  constructor(
    private grey: GreyService,
    private queryService: GreyQueryService,
    private router: Router
  ) {}

  submit(data: CreateProjectInput) {
    this.createMutation.mutate(data);
  }
}
```

## Configuration

```typescript
import { GreyService, BrowserTokenStorage } from '@grey/angular';

// In your app initialization
grey.init({
  apiBaseUrl: 'http://localhost:8080/api/v1',
  storage: new BrowserTokenStorage(), // Uses localStorage
  onAuthChange: (state) => console.log('Auth changed:', state),
  onLogout: () => router.navigate(['/login']),
});
```

## License

MIT
