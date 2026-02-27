# Grey Legacy Modernization Roadmap

## Enterprise Insurance Claims System — Migration Strategy

**Author:** Engineering Architecture Team  
**Version:** 1.0  
**Date:** February 2026  
**Status:** Proposed  

---

## Executive Summary

This document outlines a phased modernization strategy for the Grey Legacy insurance claims processing system. The approach follows the **Strangler Fig Pattern** — incrementally replacing legacy components while maintaining full system availability. No "big bang" rewrite. Every phase delivers measurable value.

The modernization targets six areas:
1. Web framework migration (Struts → Spring Boot)
2. API modernization (SOAP → REST)
3. ORM configuration migration (XML → annotations)
4. Monolith decomposition (modular → microservices)
5. Testing and quality (untested → tested codebase)
6. DevOps and observability (manual → CI/CD + monitoring)

**Estimated timeline:** 18-24 months for full migration  
**Risk level:** Low (incremental, reversible changes)  
**Business continuity:** 100% maintained throughout migration

---

## Phase 1: Foundation (Months 1-3)

### 1.1 Migrate Struts → Spring Boot MVC

**Current state:**
- Struts 1.3 ActionServlet with `struts-config.xml` routing
- Form beans extending `ActionForm`
- Actions retrieving Spring beans via `WebApplicationContextUtils`
- JSPs with Struts taglibs (`html:form`, `html:text`, `logic:messagesPresent`)

**Target state:**
- Spring Boot with embedded Tomcat
- `@Controller` / `@RestController` classes
- `@ModelAttribute` binding (replacing form beans)
- Thymeleaf templates (replacing JSPs) or keep JSPs initially

**Migration steps:**

```
Step 1: Create Spring Boot application class
├── Add spring-boot-starter-web dependency
├── Create @SpringBootApplication entry point
├── Port web.xml filter chain to FilterRegistrationBean
└── Keep Spring XML contexts initially (import via @ImportResource)

Step 2: Convert Actions to Controllers (one at a time)
├── FnolAction → FnolController
│   ├── @Controller + @RequestMapping("/claims/fnol")
│   ├── @GetMapping for form display
│   ├── @PostMapping for form submission
│   ├── @ModelAttribute for FnolForm binding
│   └── @Autowired ClaimService (direct DI, no more WebApplicationContext lookup)
├── ClaimSearchAction → ClaimSearchController
├── ClaimDetailAction → ClaimDetailController
├── AdjusterReviewAction → AdjusterReviewController
└── ClaimDashboardAction → DashboardController

Step 3: Migrate validation
├── Replace ActionErrors with JSR-303 Bean Validation
│   ├── @NotBlank, @Size, @Pattern on form fields
│   ├── @Valid on @ModelAttribute parameters
│   └── BindingResult for error handling
└── Remove ActionForm.validate() methods

Step 4: Port servlet filters
├── AuditLoggingFilter → FilterRegistrationBean or @Component
├── SessionTimeoutFilter → Spring Security filter chain
└── OpenSessionInViewFilter → spring.jpa.open-in-view=true
```

**Example conversion:**

```java
// BEFORE (Struts)
public class FnolAction extends Action {
    public ActionForward execute(ActionMapping mapping, ActionForm form,
                                  HttpServletRequest request, 
                                  HttpServletResponse response) {
        ClaimService svc = (ClaimService) WebApplicationContextUtils
            .getRequiredWebApplicationContext(request.getServletContext())
            .getBean("claimService");
        // ...
        return mapping.findForward("success");
    }
}

// AFTER (Spring Boot)
@Controller
@RequestMapping("/claims/fnol")
public class FnolController {
    
    @Autowired
    private ClaimService claimService;
    
    @GetMapping
    public String showForm(Model model) {
        model.addAttribute("fnolForm", new FnolForm());
        return "fnol";
    }
    
    @PostMapping
    public String submitFnol(@Valid @ModelAttribute FnolForm form,
                              BindingResult result, Model model) {
        if (result.hasErrors()) return "fnol";
        Claim claim = claimService.submitFnol(convertToRequest(form));
        model.addAttribute("claim", claim);
        return "fnolSuccess";
    }
}
```

**Rollback plan:** Both Struts and Spring MVC endpoints can coexist via URL prefixes.

---

### 1.2 Introduce Spring Boot Configuration

**Replace XML configuration with Spring Boot auto-configuration:**

```
applicationContext.xml         → @SpringBootApplication + @Configuration classes
applicationContext-dao.xml     → DataSourceAutoConfiguration + JpaAutoConfiguration
applicationContext-service.xml → @ComponentScan (already using @Service annotations)
database.properties            → application.yml
hibernate.cfg.xml              → spring.jpa.* properties
log4j.properties               → logback-spring.xml
web.xml                        → embedded Tomcat (eliminated entirely)
struts-config.xml              → @RequestMapping annotations (eliminated entirely)
```

**application.yml (target):**
```yaml
spring:
  datasource:
    url: jdbc:postgresql://localhost:5432/greylegacy
    username: ${DB_USERNAME}
    password: ${DB_PASSWORD}
    hikari:
      minimum-idle: 5
      maximum-pool-size: 20
  jpa:
    hibernate:
      ddl-auto: validate
    show-sql: false
    properties:
      hibernate:
        jdbc.batch_size: 25
        order_inserts: true
        order_updates: true
```

---

## Phase 2: API Modernization (Months 3-6)

### 2.1 Replace SOAP with REST

**Current state:**
- CXF-based SOAP endpoints with WSDL/XSD
- JAX-WS annotations on service interfaces
- JAXB for XML serialization
- SOAP faults for error handling

**Target state:**
- Spring Boot REST controllers
- JSON request/response (Jackson)
- Standard HTTP status codes for errors
- OpenAPI/Swagger documentation

**Migration strategy:**

```
Step 1: Create REST endpoints alongside SOAP (dual-stack)
├── POST /api/v1/claims          (replaces submitClaim SOAP op)
├── GET  /api/v1/claims/{number} (replaces getClaimDetails)
├── GET  /api/v1/claims/{number}/status
├── GET  /api/v1/policies/{number}
├── POST /api/v1/policies/{number}/validate-coverage
└── Use same service layer — REST controllers are thin wrappers

Step 2: Standardize error responses
├── @ControllerAdvice for global exception handling
├── ProblemDetail (RFC 7807) for error responses
└── Replace ClaimServiceFault/SOAP faults with HTTP status codes

Step 3: Migrate consumers (coordinate with partners)
├── Provide API migration guide to consumers
├── Set SOAP deprecation timeline (6 months)
├── Offer both endpoints during transition
└── Monitor SOAP usage — decommission when traffic drops to zero

Step 4: Add API documentation
├── springdoc-openapi for Swagger UI
├── API versioning via URL path (/api/v1/, /api/v2/)
└── Rate limiting via Spring Cloud Gateway (future)
```

**Example REST controller:**

```java
@RestController
@RequestMapping("/api/v1/claims")
public class ClaimRestController {
    
    @Autowired
    private ClaimService claimService;
    
    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public ClaimResponse submitClaim(@Valid @RequestBody FnolRequest request) {
        Claim claim = claimService.submitFnol(request);
        return ClaimMapper.toResponse(claim);
    }
    
    @GetMapping("/{claimNumber}")
    public ClaimDetailResponse getClaimDetails(@PathVariable String claimNumber) {
        Claim claim = claimService.findByClaimNumber(claimNumber);
        if (claim == null) throw new ResourceNotFoundException("Claim not found");
        return ClaimMapper.toDetailResponse(claim);
    }
}
```

---

### 2.2 Keep SOAP for Regulatory Endpoints

Some insurance integrations (state regulatory reporting, ACORD standards) still require SOAP/XML. Plan to maintain these as a small, isolated SOAP module while migrating internal APIs to REST.

---

## Phase 3: ORM Migration (Months 4-6)

### 3.1 Migrate Hibernate XML → Annotations

**Current state:**
- Dual mapping: JPA annotations AND `.hbm.xml` files
- `hibernate.cfg.xml` with explicit mapping declarations
- Spring XML `LocalSessionFactoryBean` with `mappingLocations`

**Target state:**
- JPA annotations only
- Spring Boot auto-configuration for JPA
- `EntityManagerFactory` instead of `SessionFactory`

**Migration steps:**

```
Step 1: Verify annotation coverage
├── All entities already have @Entity, @Table, @Column annotations
├── Verify all relationships have proper @ManyToOne/@OneToMany
├── Confirm @NamedQuery definitions match HQL in .hbm.xml
└── Run integration tests to validate

Step 2: Remove XML mappings
├── Delete Policy.hbm.xml, Claim.hbm.xml, ClaimPayment.hbm.xml
├── Remove <mapping resource> entries from hibernate.cfg.xml
├── Remove mappingLocations from applicationContext-dao.xml
└── Test everything — annotation mappings take over

Step 3: Migrate SessionFactory → EntityManager
├── Replace SessionFactory injection with @PersistenceContext EntityManager
├── Replace getCurrentSession() calls with EntityManager methods
├── Replace Criteria API (deprecated) with JPA CriteriaBuilder
├── Replace HQL named queries with Spring Data JPA @Query
└── Gradual — one DAO at a time

Step 4: Introduce Spring Data JPA (optional, incremental)
├── Create JpaRepository interfaces alongside existing DAOs
├── Route new code to Spring Data JPA
├── Gradually migrate existing DAOs
└── Delete custom DAO implementations when fully migrated
```

**Example Spring Data JPA repository:**

```java
@Repository
public interface ClaimRepository extends JpaRepository<Claim, Long> {
    
    Optional<Claim> findByClaimNumber(String claimNumber);
    
    List<Claim> findByStatus(ClaimStatus status);
    
    @Query("SELECT c FROM Claim c LEFT JOIN FETCH c.payments WHERE c.id = :id")
    Optional<Claim> findByIdWithPayments(@Param("id") Long id);
    
    @Query("SELECT c FROM Claim c WHERE c.fraudScore IS NULL " +
           "AND c.status NOT IN ('CLOSED', 'DENIED')")
    List<Claim> findUnscored();
    
    @Modifying
    @Query("UPDATE Claim c SET c.fraudScore = :score, c.fraudRiskLevel = :level " +
           "WHERE c.id IN :ids")
    int bulkUpdateFraudScores(@Param("score") int score, 
                               @Param("level") FraudRiskLevel level,
                               @Param("ids") List<Long> ids);
}
```

---

## Phase 4: Testing (Months 2-8, continuous)

### 4.1 Introduce Tests into an Untested Codebase

**Strategy: Test the perimeter first, then work inward.**

```
Priority 1: Integration tests for critical paths
├── FNOL submission end-to-end
├── Adjudication workflow
├── Payment scheduling and processing
├── Fraud scoring
└── Use @SpringBootTest + H2 in-memory database

Priority 2: Service layer unit tests  
├── ClaimServiceImpl — mock DAOs with Mockito
├── UnderwritingServiceImpl — mock DAOs
├── Business rule validation tests
└── Edge cases: null policies, expired coverage, etc.

Priority 3: DAO layer integration tests
├── Test named queries with @DataJpaTest
├── Verify N+1 avoidance with fetch joins
├── Test batch operations
└── Test cascade and orphan removal behavior

Priority 4: Web layer tests
├── MockMvc tests for controllers
├── Form validation tests
├── Error handling tests
└── Security tests (after Spring Security is added)
```

**Test infrastructure:**

```java
@RunWith(SpringRunner.class)
@SpringBootTest
@Transactional
@ActiveProfiles("test")
public class ClaimServiceIntegrationTest {

    @Autowired private ClaimService claimService;
    @Autowired private PolicyDao policyDao;

    @Test
    public void submitFnol_withValidPolicy_shouldCreateClaim() {
        // Given
        FnolRequest request = buildValidFnolRequest("POL-2025-001");
        
        // When
        Claim claim = claimService.submitFnol(request);
        
        // Then
        assertNotNull(claim.getId());
        assertNotNull(claim.getClaimNumber());
        assertEquals(ClaimStatus.FNOL_RECEIVED, claim.getStatus());
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void submitFnol_withInvalidPolicy_shouldThrowException() {
        FnolRequest request = buildValidFnolRequest("NONEXISTENT");
        claimService.submitFnol(request);
    }
}
```

**Code coverage targets:**
- Service layer: 80%+
- DAO layer: 70%+
- Web layer: 60%+
- Overall: 70%+ within 6 months

---

## Phase 5: Monolith Decomposition (Months 8-18)

### 5.1 Modularize the Monolith

**Before extracting microservices, modularize within the monolith.**

```
Current module structure (good start):
├── domain/    → shared domain model
├── dao/       → data access
├── service/   → business logic
├── web/       → presentation
├── batch/     → scheduled jobs
└── integration/ → SOAP endpoints

Target modular structure:
├── claims-domain/        → Claim, ClaimPayment, ClaimAuditEntry entities
├── policy-domain/        → Policy, PolicyEndorsement entities
├── claims-service/       → ClaimService + ClaimDao
├── policy-service/       → UnderwritingService + PolicyDao
├── fraud-service/        → FraudScoringService (extracted from batch)
├── payment-service/      → PaymentService (extracted from ClaimService)
├── notification-service/ → Email/SMS notifications (new)
├── api-gateway/          → REST controllers, authentication
└── batch-scheduler/      → Quartz jobs as standalone service
```

### 5.2 Gradually Extract Services

**Order of extraction (lowest risk first):**

```
1. Fraud Scoring Service
   ├── Already batch-oriented (loosely coupled)
   ├── Minimal domain dependencies
   ├── Communicate via events/messages
   └── Natural service boundary

2. Notification Service (new)
   ├── No existing coupling to break
   ├── React to domain events
   └── Email/SMS providers are external

3. Payment Service
   ├── Clear aggregate boundary
   ├── Payment processing is independent
   └── Transaction boundary already isolated (REQUIRES_NEW)

4. Policy Service
   ├── PolicyDao + UnderwritingService
   ├── Claim service calls policy service via REST/gRPC
   └── Most impactful — requires careful API design

5. Claims Service (last)
   ├── Core domain — extract once all dependencies are services
   ├── Owns Claim aggregate
   └── Orchestrates workflows via events
```

### 5.3 Introduce Event-Driven Architecture

**Replace direct service calls with domain events:**

```java
// Instead of:
claimService.submitFnol() → directly calls policyDao.find()

// Use events:
claimService.submitFnol()
  → publishes ClaimSubmittedEvent
  → FraudScoringService listens → scores claim
  → NotificationService listens → sends acknowledgment
  → AuditService listens → records audit entry
```

**Technology options:**
- Phase 1: Spring `ApplicationEventPublisher` (in-process)
- Phase 2: Apache Kafka or RabbitMQ (cross-service)

---

## Phase 6: CI/CD (Months 1-4, parallel)

### 6.1 Introduce Continuous Integration

```yaml
# .github/workflows/ci.yml
name: Grey Legacy CI
on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-java@v4
        with:
          distribution: 'temurin'
          java-version: '11'  # Upgrade from Java 8
      - name: Build and Test
        run: mvn clean verify -P ci
      - name: Code Coverage
        run: mvn jacoco:report
      - name: Upload Coverage
        uses: codecov/codecov-action@v3
```

### 6.2 Continuous Delivery Pipeline

```
Environments:
├── dev     → auto-deploy on merge to develop
├── staging → auto-deploy on merge to main
├── prod    → manual approval gate
└── DR      → replicated from prod

Pipeline stages:
1. Compile + Unit Tests
2. Integration Tests (H2)
3. Static Analysis (SonarQube)
4. Build WAR/Docker image
5. Deploy to staging
6. Smoke tests
7. Manual approval
8. Deploy to production
9. Post-deploy health check
```

### 6.3 Containerization

```dockerfile
# Dockerfile
FROM tomcat:9-jdk11-temurin
COPY web/target/grey-legacy.war /usr/local/tomcat/webapps/
COPY config/production/ /usr/local/tomcat/conf/grey-legacy/
EXPOSE 8080
CMD ["catalina.sh", "run"]
```

**Future:** Spring Boot with embedded Tomcat → single executable JAR → simpler Docker builds.

---

## Phase 7: Observability (Months 4-8)

### 7.1 Structured Logging

```
Replace log4j 1.x with:
├── SLF4J + Logback (Spring Boot default)
├── Structured JSON logging (ELK stack compatible)
├── Correlation IDs (already implemented in AuditLoggingFilter)
├── MDC context propagation across async boundaries
└── Log aggregation via ELK or Datadog
```

### 7.2 Metrics

```
Spring Boot Actuator endpoints:
├── /actuator/health     → system health
├── /actuator/metrics    → JVM, DB pool, HTTP metrics
├── /actuator/info       → build info
├── /actuator/prometheus → Prometheus scraping

Custom metrics:
├── claims.submitted (counter)
├── claims.adjudication.duration (timer)
├── claims.payout.total (gauge)
├── fraud.scores.calculated (counter)
├── batch.job.duration (timer)
└── db.pool.active (gauge)
```

### 7.3 Distributed Tracing

```
When services are extracted:
├── Spring Cloud Sleuth → Micrometer Tracing
├── Zipkin or Jaeger for trace visualization
├── Trace propagation via HTTP headers
└── Span creation for DB queries, SOAP calls, batch jobs
```

### 7.4 Alerting

```
Critical alerts:
├── Batch job failures
├── DB connection pool exhaustion
├── SOAP endpoint errors > threshold
├── Fraud score = CRITICAL
├── Payment processing failures
└── SLA breach (claim aging > 120 days)
```

---

## Technology Migration Summary

| Component           | Current                    | Target                        | Phase |
|---------------------|----------------------------|-------------------------------|-------|
| Web framework       | Struts 1.3                 | Spring Boot MVC               | 1     |
| View layer          | JSPs + Struts taglibs      | Thymeleaf or React SPA        | 1     |
| DI configuration    | Spring XML                 | Spring Boot auto-config       | 1     |
| API style           | SOAP/WSDL                  | REST + OpenAPI                | 2     |
| Serialization       | JAXB/XML                   | Jackson/JSON                  | 2     |
| ORM configuration   | Hibernate XML + annotations| JPA annotations only          | 3     |
| Data access         | Custom DAO + Criteria API  | Spring Data JPA               | 3     |
| Database            | H2 (dev)                   | PostgreSQL + Flyway migrations| 3     |
| Connection pool     | C3P0                       | HikariCP (Spring Boot default)| 3     |
| Batch processing    | Quartz + custom            | Spring Batch or Quartz        | 5     |
| Build               | Maven WAR                  | Maven/Gradle + Docker         | 6     |
| Deployment          | Tomcat WAR deploy          | Docker + Kubernetes           | 6     |
| Logging             | Log4j 1.x                  | Logback + structured JSON     | 7     |
| Monitoring          | None                       | Actuator + Prometheus + Grafana| 7    |
| Java version        | 1.8                        | 17 LTS (or 21 LTS)           | 1     |
| Testing             | None                       | JUnit 5 + Mockito + Testcontainers | 4 |

---

## Risk Mitigation

| Risk | Probability | Impact | Mitigation |
|------|:-----------:|:------:|-----------|
| Data loss during migration | Low | Critical | Feature flags, dual-write patterns, comprehensive data migration tests |
| Performance regression | Medium | High | Baseline benchmarks before migration, load testing at each phase |
| Team skill gaps | High | Medium | Training sprints on Spring Boot, Docker, REST API design; pair programming |
| Business disruption | Low | Critical | Strangler Fig Pattern — old and new coexist, gradual traffic shift |
| Scope creep | Medium | Medium | Strict phase boundaries, quarterly reviews, backlog grooming |
| Dependency conflicts | Medium | Low | Maven BOM management, dependency convergence enforcer |
| Regulatory compliance gap | Low | Critical | Engage compliance team early; maintain audit trail continuity |
| Key person departure | Medium | High | Cross-training, documented Architecture Decision Records (ADRs) |
| Integration partner refusal to migrate | Medium | Medium | Maintain SOAP adapter layer indefinitely for laggard partners |
| Production incident during migration | Medium | High | Canary deployments, instant rollback capability, war room protocol |

---

## Technical Debt Inventory

Before modernization begins, the team must catalog the existing technical debt that the migration will address. This inventory drives prioritization and helps leadership understand the "why" behind each phase.

| Debt Item | Severity | Phase Addressed | Business Impact |
|-----------|:--------:|:---------------:|-----------------|
| Struts 1.3 EOL (no security patches since 2013) | Critical | Phase 1 | Security vulnerability exposure |
| Log4j 1.x EOL (CVE-susceptible) | Critical | Phase 7 | Security vulnerability exposure |
| No automated test suite | High | Phase 4 | Regression risk on every deployment |
| SOAP-only API (no REST) | High | Phase 2 | Partner onboarding takes weeks |
| Manual deployment process | High | Phase 6 | 4-hour deployment windows, human error |
| No structured monitoring/alerting | High | Phase 7 | Incident detection takes hours |
| Hibernate XML mappings (dual config) | Medium | Phase 3 | Maintenance confusion, onboarding friction |
| Java 8 EOL (extended support ends 2030) | Medium | Phase 1 | Vendor support, security compliance |
| Spring XML-only configuration | Medium | Phase 1 | 9 XML files to understand context wiring |
| Tight coupling between service and DAO layers | Medium | Phase 5 | Cannot test or deploy independently |
| C3P0 connection pool (unmaintained) | Low | Phase 3 | HikariCP is 10x faster, better diagnostics |
| Ant build as secondary build tool | Low | Phase 6 | Dual build systems cause confusion |

---

## Migration Dependency Map

Phases are not strictly sequential. Some can run in parallel; others have hard dependencies.

```
Phase 1 (Foundation)
   ├──→ Phase 2 (API Modernization)
   │       └──→ Phase 5 (Decomposition) ──→ Phase 7.3 (Distributed Tracing)
   ├──→ Phase 3 (ORM Migration)
   │       └──→ Phase 5 (Decomposition)
   └──→ Phase 6.1 (CI) ←── can start Day 1, no dependencies
   
Phase 4 (Testing) ←── runs continuously from Month 2
   Feeds into every other phase (tests validate each migration step)

Phase 6.2-6.3 (CD + Containers) ←── depends on Phase 1 completion
Phase 7.1-7.2 (Logging + Metrics) ←── depends on Phase 1 (Spring Boot)
Phase 7.3-7.4 (Tracing + Alerting) ←── depends on Phase 5 (services exist)
```

**Critical path:** Phase 1 → Phase 2 → Phase 5 → Phase 7.3

**Quick wins (start immediately, no dependencies):**
- Phase 4: Write tests for existing code (no code changes needed)
- Phase 6.1: Set up CI pipeline (just add pipeline config)
- Phase 7.1: Replace Log4j 1.x (security-critical, standalone change)

---

## Staffing and Budget Model

| Role | Headcount | Phase Involvement | Notes |
|------|:---------:|:-----------------:|-------|
| Tech Lead / Architect | 1 | All phases | Owns ADRs, reviews all migration PRs |
| Senior Java Developer | 2 | Phases 1-3, 5 | Lead migration coding |
| Mid-level Developer | 2 | Phases 1-4 | Controller conversion, test writing |
| DevOps Engineer | 1 | Phases 6-7 | CI/CD pipelines, Docker, monitoring |
| QA Engineer | 1 | Phase 4, all phases | Test strategy, integration test suites |
| DBA | 0.5 (shared) | Phases 3, 5 | Schema migration, Flyway scripts |
| Business Analyst | 0.5 (shared) | Phase 2 | API contract negotiation with partners |

**Total:** 7-8 FTEs for 18 months

**Infrastructure costs:**
- CI/CD tooling (GitHub Actions / Jenkins agents): ~$500/month
- Staging environment (mirrors prod): ~$2,000/month
- Monitoring stack (Datadog or self-hosted ELK): ~$1,000/month
- Training budget: ~$5,000 per developer for Spring Boot / Docker certifications

---

## Stakeholder Communication Plan

| Audience | Frequency | Format | Content |
|----------|-----------|--------|---------|
| Engineering team | Weekly | Stand-up / Slack | Sprint progress, blockers, ADR reviews |
| Engineering management | Bi-weekly | Status report | Phase progress, risk updates, staffing |
| VP Engineering / CTO | Monthly | Slide deck | Milestone completion, budget, risk matrix |
| Product / Business stakeholders | Monthly | Demo | New capabilities enabled by migration |
| Integration partners | Per-phase | Email + docs | API deprecation timelines, migration guides |
| Compliance / Legal | Per-phase | Review meeting | Audit trail continuity, data handling changes |
| Operations / Support | Per-phase | Runbook updates | New deployment procedures, monitoring dashboards |

---

## Architecture Decision Records (ADRs)

Each significant technical decision during modernization is recorded as an ADR for future reference.

### ADR-001: Strangler Fig over Big Bang Rewrite
- **Decision:** Incremental migration using Strangler Fig Pattern
- **Rationale:** 100% uptime requirement; insufficient team size for parallel rewrite; incremental value delivery reduces stakeholder risk
- **Consequences:** Longer total timeline; temporary dual-stack complexity; requires careful feature flag management

### ADR-002: Spring Boot over Jakarta EE / Quarkus
- **Decision:** Migrate to Spring Boot (not Jakarta EE or Quarkus)
- **Rationale:** Existing codebase already uses Spring Framework extensively; Boot is the natural evolution; largest ecosystem and hiring pool; team already has Spring expertise
- **Consequences:** Locked into Spring ecosystem; vendor-neutral alternatives exist but would require more retraining

### ADR-003: REST (JSON) over gRPC for Service-to-Service
- **Decision:** Use REST/JSON for inter-service communication initially
- **Rationale:** Simpler tooling, easier debugging, team familiarity; gRPC can be adopted later for high-throughput internal paths
- **Consequences:** Higher serialization overhead than gRPC; compensated by simplicity

### ADR-004: PostgreSQL as Single Production Database
- **Decision:** Consolidate on PostgreSQL; no polyglot persistence initially
- **Rationale:** Team expertise; current schema is RDBMS-native; no clear use case for NoSQL yet; PostgreSQL JSONB covers semi-structured needs
- **Consequences:** Future extraction of services may warrant dedicated databases (database-per-service); PostgreSQL replication handles read scaling for now

### ADR-005: Java 17 LTS as Target Runtime
- **Decision:** Upgrade from Java 8 to Java 17 LTS (not 21)
- **Rationale:** Java 17 is the most widely adopted LTS; all Spring Boot 3.x features require Java 17+; records, sealed classes, text blocks improve code quality; Java 21 can be adopted later
- **Consequences:** Must audit all dependencies for Java 17 compatibility; some legacy libraries may need replacement

---

## Success Metrics

- **Week 1:** CI pipeline running, first unit tests passing
- **Month 3:** Struts eliminated, Spring Boot serving traffic
- **Month 6:** REST API live, SOAP deprecated, 50%+ test coverage
- **Month 12:** First service extracted, Docker deployment in staging
- **Month 18:** Full microservices architecture, 70%+ test coverage
- **Month 24:** Cloud-native deployment, full observability, SOAP decommissioned

---

## Conclusion

This modernization roadmap transforms Grey Legacy from a monolithic Struts/Hibernate/SOAP system into a modern, cloud-native microservices architecture — **without disrupting business operations**. Each phase delivers standalone value. The existing codebase demonstrates deep enterprise Java expertise; the roadmap demonstrates the architectural leadership to evolve it.

The key insight: **modernization is not replacement — it's evolution.** Every legacy pattern in this codebase exists for a reason. Understanding those reasons is what makes a senior engineer capable of leading the migration.
