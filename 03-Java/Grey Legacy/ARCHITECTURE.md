# Grey Legacy Architecture Guide

## Enterprise Insurance Claims System — Architectural Patterns & Design Decisions

**Author:** Platform Architecture Team  
**Version:** 1.0  
**Last Updated:** 2026-02  

---

## Table of Contents

1. [Layered Architecture](#1-layered-architecture)
2. [Transaction Boundaries](#2-transaction-boundaries)
3. [Enterprise Integration Patterns (EIP)](#3-enterprise-integration-patterns-eip)
4. [Data Access Patterns](#4-data-access-patterns)
5. [Security Architecture](#5-security-architecture)
6. [Deployment Architecture](#6-deployment-architecture)
7. [Error Handling Strategy](#7-error-handling-strategy)
8. [Configuration Architecture](#8-configuration-architecture)
9. [Compensating Transactions](#9-compensating-transactions)

---

## 1. Layered Architecture

### Overview

Grey Legacy follows a strict **layered (n-tier) architecture** — the dominant enterprise Java pattern from 2002–2015. Each layer has a single responsibility and communicates only with its adjacent layer.

```
┌──────────────────────────────────────────────────────────────────────┐
│                       PRESENTATION LAYER                           │
│   Struts Actions │ Spring MVC Controllers │ JSPs │ CXF SOAP       │
│   *.do mappings  │ /api/* REST endpoints  │ JSTL │ /services/*    │
├──────────────────────────────────────────────────────────────────────┤
│                        SERVICE LAYER                               │
│   ClaimService │ UnderwritingService │ ClaimServiceSessionBean     │
│   @Transactional boundaries │ Business rule orchestration          │
│   EJB-style façade (Spring-managed) │ Feature flags                │
├──────────────────────────────────────────────────────────────────────┤
│                     DATA ACCESS LAYER                              │
│   Hibernate DAOs │ MyBatis Mappers │ JDBC DAOs │ StoredProcedureDao│
│   Named queries │ Criteria API │ ResultSet mapping │ CallableStmt  │
├──────────────────────────────────────────────────────────────────────┤
│                       DOMAIN LAYER                                 │
│   Policy │ Claim │ ClaimPayment │ Adjuster │ ClaimAuditEntry      │
│   JPA annotations + HBM XML │ @Version │ Enums │ Value objects     │
├──────────────────────────────────────────────────────────────────────┤
│                    INFRASTRUCTURE LAYER                            │
│   PostgreSQL │ ActiveMQ │ SMTP │ SFTP │ External SOAP services    │
│   File system │ JNDI │ Quartz scheduler │ Thread pools              │
└──────────────────────────────────────────────────────────────────────┘
```

### Why This Pattern Exists

Layered architecture was standard for enterprise Java because:

1. **Team structure mapping** — Large organizations had separate teams for each layer (UI team, service team, DBA team). The layer boundaries matched organizational boundaries (Conway's Law).

2. **Technology isolation** — Each layer could change its implementation without affecting others. Swapping Struts for Spring MVC only touched the presentation layer; the service layer was unaffected.

3. **Transaction clarity** — The service layer owns all transaction boundaries. DAOs never start transactions; controllers never commit transactions. This prevents the "distributed transaction through the web tier" anti-pattern.

4. **Testability** — Each layer can be tested independently by mocking the layer below it. Service tests mock DAOs; controller tests mock services.

### Layer Rules (enforced by convention, not compiler)

| Rule | Rationale |
|------|-----------|
| Presentation → Service only (never directly to DAO) | Controllers calling DAOs bypass business rules and transaction management |
| Service → DAO only (never to Presentation) | Services must be presentation-agnostic; same service serves Struts, REST, SOAP, and Batch |
| DAO → Domain only | DAOs never import presentation or service classes |
| Domain has NO dependencies on other layers | Entities are POJOs; they don't know about Hibernate, Spring, or Struts |
| Cross-cutting concerns (logging, security, transactions) use AOP/filters | Avoids scattering infrastructure code across business logic |

### Module Mapping

| Maven Module | Layer | Spring Context |
|-------------|-------|----------------|
| `domain/` | Domain | — (no Spring beans, just POJOs) |
| `dao/` | Data Access | `applicationContext-dao.xml` |
| `service/` | Service | `applicationContext-service.xml` |
| `web/` | Presentation | `spring-mvc-servlet.xml`, `struts-config.xml` |
| `batch/` | Cross-cutting (scheduled) | `applicationContext-batch.xml`, `applicationContext-spring-batch.xml` |
| `integration/` | Cross-cutting (external) | `applicationContext-integration.xml`, `applicationContext-jms.xml` |

---

## 2. Transaction Boundaries

### The Service Layer Owns Transactions

Every write operation in Grey Legacy goes through the service layer, which is the **sole owner of transaction boundaries**. This is enforced by Spring's `@Transactional` annotation and the declarative TX advice in `applicationContext.xml`.

```
          Controller              Service                   DAO
              │                      │                       │
              │  submitFnol(req)     │                       │
              ├─────────────────────>│                       │
              │                      │ ── TX BEGIN ────────  │
              │                      │  findByPolicyNumber() │
              │                      ├──────────────────────>│
              │                      │<──────────────────────┤
              │                      │  save(claim)          │
              │                      ├──────────────────────>│
              │                      │<──────────────────────┤
              │                      │  save(auditEntry)     │
              │                      ├──────────────────────>│
              │                      │<──────────────────────┤
              │                      │ ── TX COMMIT ──────── │
              │<─────────────────────┤                       │
```

### Propagation Patterns Used

| Propagation | Where Used | Why |
|-------------|-----------|-----|
| `REQUIRED` (default) | All service methods | Joins existing TX or creates new one. Standard for business operations. |
| `REQUIRES_NEW` | `AuditService.logAudit()` | Audit must persist even if the business operation fails. Independent TX prevents rollback cascade. |
| `SUPPORTS` | Read-only service methods | Participates in existing TX if present, runs without TX otherwise. Avoids unnecessary TX overhead for reads. |
| `NOT_SUPPORTED` | `LegacyConfigurationManager.reload()` | Configuration refresh must not participate in any business TX. |

### Declarative TX via AOP (applicationContext.xml)

```xml
<!-- Before Spring annotations were common, TX was declared via AOP advice -->
<tx:advice id="txAdvice" transaction-manager="transactionManager">
    <tx:attributes>
        <tx:method name="get*" read-only="true" propagation="SUPPORTS"/>
        <tx:method name="find*" read-only="true" propagation="SUPPORTS"/>
        <tx:method name="search*" read-only="true" propagation="SUPPORTS"/>
        <tx:method name="*" propagation="REQUIRED" rollback-for="Exception"/>
    </tx:attributes>
</tx:advice>

<aop:config>
    <aop:pointcut id="serviceOperation"
                  expression="execution(* com.greylegacy.service.*.*(..))"/>
    <aop:advisor advice-ref="txAdvice" pointcut-ref="serviceOperation"/>
</aop:config>
```

This pattern means **every method in the service package gets transaction management automatically**, even without `@Transactional` annotations. The annotations on `ClaimServiceSessionBean` are belt-and-suspenders — they work alongside the AOP advice.

### Self-Invocation Trap

The most common transaction bug in Spring applications:

```java
@Service
public class ClaimService {
    
    public void processBatch(List<Long> ids) {
        for (Long id : ids) {
            processOneClaim(id);  // DIRECT CALL — bypasses proxy!
        }
    }
    
    @Transactional
    public void processOneClaim(Long id) {
        // This @Transactional is IGNORED when called from processBatch()
        // because the call doesn't go through the Spring AOP proxy.
    }
}
```

**Grey Legacy's mitigation:** The AOP advice in `applicationContext.xml` covers all service methods regardless of self-invocation, because the pointcut applies to the proxy-level entry point. However, `@Transactional` annotations on self-invoked methods are still ineffective — the AOP advice from the XML config handles those cases.

---

## 3. Enterprise Integration Patterns (EIP)

Grey Legacy implements several patterns from the *Enterprise Integration Patterns* book (Hohpe & Woolf, 2003), primarily through Apache Camel and Spring JMS.

### 3a. Message-Driven Processing (Event Listener)

```
  ClaimService                    JMS (ActiveMQ)                 ClaimEventListener
       │                              │                               │
       │  publishClaimEvent()         │                               │
       ├─────────────────────────────>│                               │
       │                              │  onMessage()                  │
       │                              ├──────────────────────────────>│
       │                              │              (idempotent      │
       │                              │               dedup check)    │
       │                              │                               │
       │                              │              fraud scoring    │
       │                              │              notification     │
       │                              │              audit trail      │
```

- **Idempotent Consumer**: `IdempotentMessageConsumer` uses an LRU cache of JMS message IDs to prevent duplicate processing. At-least-once delivery semantics require application-level deduplication.
- **Dead Letter Queue (DLQ)**: Messages that fail after max delivery attempts (5) are routed to `DLQ.GreyLegacyClaimEvents`. `DeadLetterQueueProcessor` periodically scans the DLQ for manual review.

### 3b. Content-Based Router (Apache Camel)

```java
from("file:{{batch.export.directory}}/incoming?noop=true")
    .choice()
        .when(header("CamelFileName").endsWith(".csv"))
            .to("direct:processCsvImport")
        .when(header("CamelFileName").endsWith(".xml"))
            .to("direct:processXmlImport")
        .otherwise()
            .to("direct:unsupportedFormat")
    .end();
```

Files arriving in the incoming directory are routed to different processing pipelines based on file extension.

### 3c. Wire Tap (Audit Copy)

```java
from("direct:processClaimUpdate")
    .wireTap("jms:queue:ClaimAuditQueue")  // Non-blocking copy
    .to("bean:claimService?method=updateClaim");
```

A copy of every claim update message is sent to an audit queue without affecting the main processing flow.

### 3d. Splitter (Batch File Processing)

```java
from("direct:processCsvImport")
    .split(body().tokenize("\n"))   // Split CSV into individual lines
        .streaming()                 // Memory-efficient streaming
        .to("bean:csvClaimProcessor?method=processLine")
    .end();
```

### 3e. Request-Reply (SOAP Client)

```
  ClaimService          FraudScoringServiceClient          External SOAP
       │                         │                              │
       │  scoreClaim(claim)      │                              │
       ├────────────────────────>│                              │
       │                         │  SOAP Request                │
       │                         ├─────────────────────────────>│
       │                         │  (retry: 3 attempts,        │
       │                         │   backoff: 1s, 2s, 4s)      │
       │                         │<─────────────────────────────┤
       │  FraudScore             │  SOAP Response               │
       │<────────────────────────┤                              │
```

SOAP clients implement **retry with exponential backoff** and **circuit breaker** patterns to handle partner service unreliability.

---

## 4. Data Access Patterns

### 4a. The Three DAO Strategies

Grey Legacy demonstrates three concurrent data access strategies — a realistic scenario in systems that evolved over 10+ years:

| Strategy | When Introduced | Best For | Example |
|----------|----------------|----------|---------|
| **Hibernate (HBM XML + JPA)** | Day 1 (2008) | CRUD, entity graphs, lazy loading | `ClaimHibernateDao` |
| **MyBatis (XML mappers)** | Phase 2 (2012) | Complex reports, read-only queries, tuned SQL | `ClaimMyBatisDao` |
| **Raw JDBC** | Day 1 (legacy) | Stored procedures, bulk operations, maximum control | `ClaimJdbcDao`, `StoredProcedureDao` |

### 4b. Open Session in View (OSIV)

Grey Legacy uses the `OpenSessionInViewFilter` to keep the Hibernate Session open through the entire HTTP request lifecycle. This prevents `LazyInitializationException` when JSPs access lazily-loaded collections.

```
  HTTP Request          Filter              Service              DAO
       │                  │                    │                   │
       │  GET /claim/42   │                    │                   │
       ├─────────────────>│                    │                   │
       │                  │ ── Open Session ── │                   │
       │                  │  doFilter()        │                   │
       │                  ├───────────────────>│                   │
       │                  │                    │  findById(42)     │
       │                  │                    ├──────────────────>│
       │                  │                    │<─────────────────-┤
       │                  │<───────────────────┤                   │
       │                  │                    │                   │
       │  JSP renders     │                    │                   │
       │  claim.payments  │ ← lazy load OK, Session still open    │
       │                  │                    │                   │
       │                  │ ── Close Session ──│                   │
       │<─────────────────┤                    │                   │
```

**Trade-off:** OSIV is considered an anti-pattern by some because it can mask N+1 query problems and create unexpected database access during view rendering. However, it is ubiquitous in legacy Spring/Hibernate applications.

### 4c. Pessimistic vs. Optimistic Locking

| Pattern | Implementation | Use Case |
|---------|---------------|----------|
| **Optimistic** | `@Version` column on all entities | Web-facing reads (low contention). Hibernate checks version on flush; throws `StaleObjectStateException` on conflict. |
| **Pessimistic** | `SP_LOCK_CLAIM_FOR_PROCESSING` (`SELECT...FOR UPDATE`) | Batch payment processing (must guarantee exclusive access during transaction). |

### 4d. SCD Type 2 (Slowly Changing Dimension)

The `POLICY_HISTORY` table implements SCD Type 2 for tracking policy changes over time:

```
  POLICY_HISTORY
  ┌──────────┬──────────┬────────────┬────────────┬────────────┐
  │HISTORY_ID│POLICY_ID │ STATUS     │EFF_FROM    │EFF_TO      │IS_CURRENT│
  ├──────────┼──────────┼────────────┼────────────┼────────────┤
  │  1       │  100     │ ACTIVE     │2024-01-01  │2024-06-15  │ FALSE    │
  │  2       │  100     │ ACTIVE     │2024-06-15  │2025-01-01  │ FALSE    │
  │  3       │  100     │ RENEWED    │2025-01-01  │ NULL       │ TRUE     │
  └──────────┴──────────┴────────────┴────────────┴────────────┘
```

The trigger `TRG_POLICY_PREMIUM_CHANGE` automatically captures snapshots when premium amounts change, calling `SP_POLICY_SCD_SNAPSHOT` to expire the current row and insert a new one.

---

## 5. Security Architecture

### Defense in Depth

Grey Legacy implements multiple security layers, reflecting the "belt, suspenders, and a parachute" approach common in financial services:

```
┌───────────────────────────────────────────────────────────────┐
│ Layer 1: Network (firewall, VPN, DMZ)                        │ ← Ops team
├───────────────────────────────────────────────────────────────┤
│ Layer 2: Transport (TLS 1.2, cipher suite restriction)       │ ← server.xml
├───────────────────────────────────────────────────────────────┤
│ Layer 3: Container (security-constraints, JAAS, FORM login)  │ ← web.xml
├───────────────────────────────────────────────────────────────┤
│ Layer 4: Application (AuditLoggingFilter, SessionTimeout)    │ ← Servlet filters
├───────────────────────────────────────────────────────────────┤
│ Layer 5: Message (WS-Security, UsernameToken, signatures)    │ ← CXF interceptors
├───────────────────────────────────────────────────────────────┤
│ Layer 6: Data (column encryption, PII masking, audit trail)  │ ← DAO/DB layer
└───────────────────────────────────────────────────────────────┘
```

### Authentication Flow

```
  Browser                 Tomcat/JBoss            JAAS LoginModule         Database
     │                        │                         │                      │
     │  GET /claims/search    │                         │                      │
     ├───────────────────────>│                         │                      │
     │                        │ security-constraint     │                      │
     │                        │ requires CLAIMS_ADJUSTER│                      │
     │  302 → /login.jsp      │                         │                      │
     │<───────────────────────┤                         │                      │
     │                        │                         │                      │
     │  POST j_security_check │                         │                      │
     │  (j_username, j_password)                        │                      │
     ├───────────────────────>│                         │                      │
     │                        │  login()                │                      │
     │                        ├────────────────────────>│                      │
     │                        │                         │  SELECT PASSWORD...  │
     │                        │                         ├─────────────────────>│
     │                        │                         │<─────────────────────┤
     │                        │                         │  hash & compare      │
     │                        │                         │  SELECT ROLE_NAME... │
     │                        │                         ├─────────────────────>│
     │                        │                         │<─────────────────────┤
     │                        │  commit() → Principals  │                      │
     │                        │<────────────────────────┤                      │
     │  302 → /claims/search  │                         │                      │
     │  Set-Cookie: JSESSIONID│                         │                      │
     │<───────────────────────┤                         │                      │
```

### Role Hierarchy

```
        ADMIN
          │
    CLAIMS_MANAGER
          │
    CLAIMS_ADJUSTER
          │
       READONLY
```

Each higher role inherits the permissions of all lower roles. This is enforced by assigning multiple roles per user in `APP_USER_ROLE` (e.g., a CLAIMS_MANAGER also has CLAIMS_ADJUSTER and READONLY roles).

---

## 6. Deployment Architecture

### Production Topology

```
                    ┌──────────────────┐
                    │   Load Balancer  │
                    │   (F5 / HAProxy) │
                    └────────┬─────────┘
                             │
              ┌──────────────┼──────────────┐
              │              │              │
     ┌────────▼──────┐  ┌───▼──────────┐  ┌▼──────────────┐
     │  Tomcat Node 1│  │ Tomcat Node 2│  │  Tomcat Node 3│
     │  (jvmRoute=1) │  │ (jvmRoute=2) │  │  (jvmRoute=3) │
     │  grey-legacy  │  │ grey-legacy  │  │  grey-legacy   │
     │  .war         │  │ .war         │  │  .war          │
     └───────┬───────┘  └──────┬───────┘  └───────┬────────┘
             │                 │                   │
             └─────────────────┼───────────────────┘
                               │
              ┌────────────────┼────────────────┐
              │                │                │
     ┌────────▼──────┐  ┌─────▼──────┐  ┌──────▼──────┐
     │  PostgreSQL   │  │  ActiveMQ  │  │   SMTP      │
     │  Primary      │  │  Broker    │  │   Relay     │
     │  + Standby    │  │  + Standby │  │             │
     └───────────────┘  └────────────┘  └─────────────┘
```

### WAR Deployment (Tomcat)

```bash
# Standard WAR deployment
cp web/target/grey-legacy.war $CATALINA_HOME/webapps/

# Exploded deployment (faster, allows JSP editing in dev)
unzip web/target/grey-legacy.war -d $CATALINA_HOME/webapps/grey-legacy/
```

### EAR Deployment (JBoss/WebLogic)

```bash
# Hot deployment to JBoss
cp ear/target/grey-legacy.ear $JBOSS_HOME/standalone/deployments/

# WebLogic deployment via WLST
java weblogic.Deployer -adminurl t3://admin:7001 \
    -username weblogic -password <password> \
    -deploy ear/target/grey-legacy.ear -targets AdminServer
```

### Container-Specific Descriptors

| Container | Descriptor | Location | Purpose |
|-----------|-----------|----------|---------|
| All | `web.xml` | `WEB-INF/` | Standard Java EE deployment descriptor |
| Tomcat | `context.xml` | `META-INF/` | JNDI resource links, session persistence |
| Tomcat | `server.xml` | `$CATALINA_HOME/conf/` | Connectors, realms, global JNDI |
| JBoss | `jboss-app.xml` | `META-INF/` (EAR) | Classloader isolation |
| JBoss | `standalone.xml` | `$JBOSS_HOME/standalone/configuration/` | Full server config |
| WebSphere | `ibm-web-bnd.xml` | `WEB-INF/` | Security role → LDAP group mapping |
| WebLogic | `weblogic.xml` | `WEB-INF/` | Classloading, sessions, security |

---

## 7. Error Handling Strategy

### Layered Exception Handling

```
  Presentation Layer              Service Layer              DAO Layer
  ┌──────────────────┐            ┌────────────────┐         ┌──────────────┐
  │ ActionForward    │            │ Business       │         │ SQLException │
  │ → error.jsp      │←───────── │ Exceptions     │←─────── │ Hibernate    │
  │                  │  unchecked │ (checked/      │ wrapped │ Exceptions   │
  │ @ExceptionHandler│            │  unchecked)    │         │              │
  │ → error response │            │                │         │ DataAccess   │
  │                  │            │ Translate DB   │         │ Exception    │
  │ SOAP Fault       │            │ exceptions to  │         │              │
  │ → fault taxonomy │            │ business terms │         │              │
  └──────────────────┘            └────────────────┘         └──────────────┘
```

### Exception Categories

| Category | Example | Handling |
|----------|---------|----------|
| **Validation** | Invalid policy number, missing required field | Return 400 / SOAP VALIDATION_ERROR fault. Do not log as error. |
| **Business Rule** | Expired policy, claim exceeds coverage limit | Return 422 / SOAP BUSINESS_RULE fault. Log as WARN. |
| **Authorization** | User lacks role for operation | Return 403 / SOAP AUTHORIZATION fault. Log + audit trail. |
| **System** | Database down, JMS broker unreachable | Return 500 / SOAP SYSTEM_ERROR fault. Log as ERROR. Page on-call. |
| **Concurrency** | `StaleObjectStateException` (optimistic lock) | Retry once, then return 409 Conflict. Log as WARN. |

### SOAP Fault Taxonomy

Grey Legacy defines a typed fault code system for SOAP services:

```xml
<soap:Fault>
    <faultcode>soap:Server</faultcode>
    <faultstring>Claim CLM-2025-00042 cannot be approved: policy POL-001 is expired</faultstring>
    <detail>
        <ns:ClaimServiceFault>
            <faultCode>BUSINESS_RULE</faultCode>
            <correlationId>abc-123-def</correlationId>
            <timestamp>2025-03-15T14:30:00Z</timestamp>
        </ns:ClaimServiceFault>
    </detail>
</soap:Fault>
```

---

## 8. Configuration Architecture

### The XML Configuration Web

Grey Legacy uses Spring's XML-based configuration, which was standard before Spring Boot's auto-configuration. The configuration flows through a web of imported XML files:

```
web.xml
  └─ContextLoaderListener loads:
      ├─ applicationContext.xml (WEB-INF) ← Root context
      │   ├─ import: applicationContext-dao.xml
      │   ├─ import: applicationContext-service.xml
      │   ├─ import: applicationContext-integration.xml
      │   ├─ import: applicationContext-ws-security.xml
      │   ├─ import: applicationContext-jms.xml
      │   ├─ import: applicationContext-batch.xml
      │   ├─ import: applicationContext-spring-batch.xml
      │   └─ import: applicationContext-jmx.xml
      │
      ├─ database.properties ← loaded by PropertyPlaceholderConfigurer
      ├─ feature-flags.properties
      └─ application.properties
```

### Property Resolution Order

The `LegacyConfigurationManager` implements a layered property resolution strategy common in pre-Boot applications:

```
Priority (highest → lowest):
  1. System properties (-Dproperty=value)
  2. Environment variables (DB_HOST, DB_PORT)
  3. JNDI entries (java:comp/env/app/environment)
  4. feature-flags.properties (hot-reloadable)
  5. application.properties (classpath)
  6. database.properties (classpath)
  7. Hardcoded defaults in Java code
```

### Feature Flags

Grey Legacy uses a simple properties-file-based feature flag system that predates modern feature flag services:

```properties
# feature-flags.properties
feature.fraud.scoring.enabled=true
feature.fraud.scoring.auto.deny.threshold=85
feature.batch.export.ssn.masking=true
feature.soap.wss.timestamp.validation=true
feature.claims.auto.assign.adjuster=true
feature.modernization.rest.api.enabled=false
```

Feature flags are checked at runtime via `LegacyConfigurationManager.isFeatureEnabled("fraud.scoring")` and can be toggled without application restart via JMX or by editing the properties file and calling the reload endpoint.

---

## 9. Compensating Transactions

### The Problem

Insurance claim processing involves multiple steps that span different systems and transactions:

1. Update claim status in the database
2. Schedule a payment via the payment gateway
3. Send notification via JMS
4. Update the reinsurance system via SOAP
5. Generate audit entries

If step 3 succeeds but step 4 fails, the system is in an inconsistent state. True distributed (XA) transactions are impractical across SOAP endpoints and external systems. Instead, Grey Legacy uses **compensating transactions** — reverse operations that undo the effects of previously completed steps.

### Pattern: Compensation Chain

```
Forward execution:
  Step 1: reserveFunds()     → success → record compensation: releaseFunds()
  Step 2: schedulePayment()  → success → record compensation: cancelPayment()
  Step 3: notifyClaimant()   → success → record compensation: sendCorrectionNotice()
  Step 4: updateReinsurer()  → FAILURE
  
Compensation (reverse order):
  Undo Step 3: sendCorrectionNotice()
  Undo Step 2: cancelPayment()
  Undo Step 1: releaseFunds()
```

### Implementation: ClaimPayoutCoordinator

The `ClaimPayoutCoordinator` in the service module implements this pattern. Each step in the payout workflow registers a compensating action. If any step fails, the coordinator executes compensations in reverse order (LIFO).

```java
/**
 * Coordinates multi-step claim payout across systems with
 * compensating transaction support.
 *
 * This is NOT a distributed transaction (XA/2PC). Each step executes
 * in its own local transaction. Compensation is best-effort — if a
 * compensation itself fails, it is logged to the COMPENSATION_FAILURE
 * audit table for manual intervention.
 *
 * Pattern: Saga (orchestration variant)
 * See: Enterprise Integration Patterns, Chapter 10
 */
public class ClaimPayoutCoordinator {

    private final CompensationChain compensationChain = new CompensationChain();

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public PayoutResult executePayout(Long claimId, BigDecimal amount) {
        Claim claim = claimDao.findById(claimId);

        try {
            // Step 1: Reserve funds against the claim's coverage
            reserveService.reserveFunds(claim, amount);
            compensationChain.register(() ->
                reserveService.releaseFunds(claim, amount));

            // Step 2: Schedule payment via payment gateway
            PaymentConfirmation confirmation =
                paymentGateway.schedulePayment(claim, amount);
            compensationChain.register(() ->
                paymentGateway.cancelPayment(confirmation.getTransactionId()));

            // Step 3: Send claimant notification via JMS
            notificationSender.sendPayoutNotification(claim, amount);
            compensationChain.register(() ->
                notificationSender.sendCorrectionNotice(claim,
                    "Payout of " + amount + " has been reversed"));

            // Step 4: Update reinsurance system via SOAP
            reinsurerClient.reportPayout(claim.getClaimNumber(), amount);
            // No compensation needed — reinsurer accepts correction reports

            // All steps succeeded — clear the compensation chain
            compensationChain.clear();
            return PayoutResult.success(confirmation);

        } catch (Exception e) {
            log.error("Payout failed at step {}, initiating compensation",
                      compensationChain.getStepsCompleted(), e);

            // Execute compensations in reverse order
            List<String> failures = compensationChain.compensate();

            if (!failures.isEmpty()) {
                // Compensation itself failed — requires manual intervention
                auditService.recordCompensationFailure(
                    claimId, failures, e.getMessage());
            }

            return PayoutResult.failed(e.getMessage(), failures);
        }
    }
}
```

### CompensationChain

```java
/**
 * Maintains a stack of compensating actions (Runnable lambdas) and
 * executes them in LIFO order on failure.
 *
 * Design: Each compensation is independent and best-effort.
 * If a compensation throws, it is caught, logged, and the chain
 * continues executing remaining compensations.
 */
public class CompensationChain {
    private final Deque<Runnable> compensations = new ArrayDeque<>();

    public void register(Runnable compensation) {
        compensations.push(compensation);  // Stack: LIFO
    }

    public int getStepsCompleted() {
        return compensations.size();
    }

    public List<String> compensate() {
        List<String> failures = new ArrayList<>();
        while (!compensations.isEmpty()) {
            Runnable action = compensations.pop();
            try {
                action.run();
            } catch (Exception e) {
                log.error("Compensation failed: {}", e.getMessage());
                failures.add(e.getMessage());
                // Continue — do not abort remaining compensations
            }
        }
        return failures;
    }

    public void clear() {
        compensations.clear();
    }
}
```

### When to Use Compensating Transactions

| Scenario | Use Compensating TX? | Alternative |
|----------|:-------------------:|-------------|
| Multi-system writes (DB + SOAP + JMS) | **Yes** | XA/2PC (complex, slow) |
| Single-database multi-table updates | No | Local ACID transaction |
| Batch job with partial failures | **Yes** | Spring Batch skip/retry |
| Idempotent operations | No | Retry with deduplication |
| Long-running business process (multi-day) | **Yes** | State machine + compensation |

### Key Design Decisions

1. **LIFO execution:** Compensations run in reverse order to unwind state changes cleanly
2. **Best-effort:** A failing compensation does not abort remaining compensations
3. **Audit trail:** All compensation attempts (success and failure) are recorded in `COMPENSATION_AUDIT`
4. **Manual fallback:** If automated compensation fails, an alert is raised for operations team intervention
5. **Idempotent compensations:** Each compensation action is designed to be safely re-executable

---

## Design Decision Log

| Decision | Rationale | Trade-off | Date |
|----------|-----------|-----------|------|
| Layered architecture | Matched team structure, enforced separation of concerns | Vertical feature changes require touching all layers | 2008 |
| Spring XML over annotations | Java 5 adoption was incomplete; XML config was auditable and diff-able | Verbose; changes require deployment | 2008 |
| Hibernate HBM XML + JPA annotations | Started with XML (Hibernate 3); added JPA annotations when migrating to Hibernate 5 | Dual mapping is confusing; HBM XML takes precedence | 2008/2016 |
| MyBatis for reporting | Complex reporting queries were awkward with HQL/Criteria API | Two ORM frameworks to maintain | 2012 |
| Open Session in View | Prevented LazyInitializationException in JSPs without refactoring all service methods | Masks N+1 problems; unexpected DB access during rendering | 2009 |
| SOAP over REST | Industry standard for B2B insurance integrations (ACORD); WS-Security for message-level auth | Verbose; schema coupling; tooling complexity | 2008 |
| JMS for async events | Decouples claim processing from notification/audit; handles load spikes | Additional infrastructure (ActiveMQ); operational complexity | 2010 |
| Quartz for scheduling | Pre-dates Spring Batch scheduler; deeply integrated with JDBC job store | Overlap with Spring Batch job launching | 2008 |
| Ant build alongside Maven | Legacy CI/CD scripts depended on Ant targets; migration risk with no benefit | Two build systems to maintain | 2008/2014 |
| EAR packaging | Required by JBoss/WebLogic for shared library classloading and EJB modules | Extra build complexity; not needed for Tomcat-only deployment | 2010 |
| Container-managed JAAS | Insurance regulatory requirement for centralized authentication audit trail | Complex configuration across container vendors | 2009 |

---

*This document describes the architecture as-built. For the planned modernization path, see [MODERNIZATION_ROADMAP.md](MODERNIZATION_ROADMAP.md).*
