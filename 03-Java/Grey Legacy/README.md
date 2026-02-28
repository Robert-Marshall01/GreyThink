# Grey Legacy

**Enterprise Insurance Claims Processing System**

A multi-module Java monolith that mirrors real insurance claims systems built with legacy enterprise technologies. Demonstrates deep expertise in the enterprise Java stack that powers the insurance industry.

May be unstable, verify stability before deploying in a production environment.
---

## Architecture

```
grey-legacy/
├── domain/           — JPA entities, Hibernate XML mappings, enums
├── dao/              — Hibernate DAOs, MyBatis mappers, JDBC/CallableStatement, stored procedures
├── service/          — Business logic, EJB-style session bean facade, configuration manager
├── web/              — Struts actions, JSPs, Spring MVC controllers, servlet filters, JAAS security
├── batch/            — Quartz nightly jobs, Spring Batch chunk processing
├── integration/      — SOAP/WSDL, WS-Security, JMS, Apache Camel routes, file import
├── modernization/    — Spring Boot 2.7, MapStruct, Flyway, Micrometer (migration bridge)
├── ear/              — EAR packaging for JBoss/WebLogic deployment
├── config/           — App server configs (Tomcat, JBoss, JAAS)
├── scripts/          — Deployment, backup, health-check, and ETL scripts
├── ARCHITECTURE.md   — Layered architecture, EIP patterns, transaction boundaries
├── MODERNIZATION_ROADMAP.md — Phase-by-phase migration strategy
└── TROUBLESHOOTING_LEGACY_SYSTEMS.md — 14-section debugging guide
```

## Full-Spectrum Legacy Framework Coverage

This project demonstrates fluency across **all 10 categories** of the enterprise Java legacy framework landscape, covering every layer from presentation to build tooling.

### Category 1: Web Frameworks
| Framework | Module | Key Files |
|-----------|--------|-----------|
| **Apache Struts 1.3** | web | `FnolAction`, `ClaimSearchAction`, `FnolForm`, `ClaimSearchForm`, `struts-config.xml`, `validation.xml` |
| **Spring MVC 4.3** | web | `ClaimApiController`, `PolicyApiController`, `spring-mvc-servlet.xml`, `ContentNegotiatingViewResolver` |
| **JSP + JSTL + Taglibs** | web | `dashboard.jsp`, `claimSearch.jsp`, `fnol.jsp`, `header.jsp`, `footer.jsp`, Struts taglibs |

### Category 2: ORM / Persistence
| Framework | Module | Key Files |
|-----------|--------|-----------|
| **Hibernate 5.2** (HBM XML) | domain/dao | `Policy.hbm.xml`, `Claim.hbm.xml`, `ClaimPayment.hbm.xml`, `hibernate.cfg.xml` |
| **JPA 2.1** (Annotations) | domain | `@Entity`, `@NamedQueries`, `@Version`, `@ManyToOne`, `@OneToMany` on all entities |
| **MyBatis 3.5** (Hybrid) | dao | `mybatis-config.xml`, `ClaimMapper.xml`, `PolicyMapper.xml`, `ClaimMyBatisDao`, `PolicyMyBatisDao` |

### Category 3: Dependency Injection & Configuration
| Framework | Module | Key Files |
|-----------|--------|-----------|
| **Spring XML** (pre-Boot) | all | `applicationContext-dao.xml`, `applicationContext-service.xml`, `applicationContext-integration.xml`, `applicationContext-jms.xml`, `applicationContext-batch.xml`, `applicationContext-camel.xml`, `applicationContext-ws-security.xml` |
| **web.xml** | web | 3 servlet mappings, filter chains, `ContextLoaderListener`, init-params |
| **Commons Configuration** pattern | service | `LegacyConfigurationManager` — layered properties, env vars, system props, feature flags |

### Category 4: Integration Middleware
| Framework | Module | Key Files |
|-----------|--------|-----------|
| **Apache CXF** (SOAP/WSDL) | integration | `ClaimWebServiceImpl` (submitClaim, getClaimStatus, getClaimDetails, updateClaimStatus with state-machine validation), `PolicyLookupWebServiceImpl`, fault taxonomy (`FaultCodeType` enum: VALIDATION/BUSINESS/AUTH/SYSTEM), `ClaimService.wsdl`, SOAP clients: `FraudScoringServiceClient` (WS-Security, retry), `VinLookupServiceClient` (circuit breaker) |
| **JAXB** (XML binding) | integration | `ClaimSubmissionRequest`, `ValidateCoverageRequest`, XSD schemas |
| **JMS / ActiveMQ** | integration | `ClaimEventListener` (MDP with idempotent consumer, poison-message DLQ routing), `ClaimNotificationSender` (JmsTemplate), `JmsErrorHandler` (classified error handling), `IdempotentMessageConsumer` (LRU dedup), `DeadLetterQueueProcessor`, durable topic subscriptions, exponential backoff redelivery policy |
| **Apache Camel 2.25** | integration | `ClaimIntegrationRouteBuilder` — file polling, content-based router, splitter, dead letter channel, wire tap, idempotent consumer |
| **WS-Security** (WSS4J) | integration | `applicationContext-ws-security.xml`, `ServerPasswordCallback`, UsernameToken auth, timestamp validation |
| **JSch SFTP** | integration | `FtpPartnerExchange` — secure file transfer with partner systems |

### Category 5: Batch Processing
| Framework | Module | Key Files |
|-----------|--------|-----------|
| **Quartz 2.3** | batch | `ClaimAgingJob`, `FraudScoringJob`, `PayoutSchedulingJob`, `PremiumRecalculationJob`, `SpringBatchQuartzJobLauncher` (Quartz→Spring Batch bridge), cron triggers, misfire instructions |
| **Spring Batch 3.0** | batch | `ClaimExportItemReader/Processor/Writer` (`ItemStream` lifecycle, restartable checkpointing, ThreadLocal formatters), `PolicyFileImportReader/Processor/Writer`, `ClaimExportPartitioner` (range-based), `ClaimReconciliationTasklet`, `BatchJobCompletionListener`, JDBC `JobRepository`, `JobOperator` for ops control, partitioned steps with `ThreadPoolTaskExecutor` |

### Category 6: Utility Libraries & Testing
| Framework | Module | Key Files |
|-----------|--------|-----------|
| **Log4j 1.2 + SLF4J** | all | `log4j.properties` — `RollingFileAppender`, MDC correlation IDs, pattern layout, multiple appenders |
| **Apache Commons** (Lang, IO, Collections) | dao/service | `StringUtils`, `IOUtils`, `CollectionUtils` throughout codebase |
| **JAXB** (runtime) | integration | XML ↔ POJO marshalling with XSD validation |
| **JUnit 3** (legacy) | domain | `PolicyTest.java`, `ClaimTest.java` — extends `TestCase`, `setUp()`/`tearDown()`, static `suite()` |
| **JUnit 4 + Spring** | dao | `ClaimDaoIntegrationTest` — `@RunWith(SpringJUnit4ClassRunner)`, `@ContextConfiguration`, `@Transactional` |
| **JUnit 4 + Mockito** | service | `ClaimServiceTest`, `UnderwritingServiceTest` — `@Mock`, `@InjectMocks`, `verify()`, `ArgumentCaptor` |

### Category 7: Build & Packaging
| Tool | Location | Key Files |
|------|----------|-----------|
| **Maven** (multi-module) | root | Parent POM with `dependencyManagement`, 4 profiles (dev/staging/prod/integration-tests), plugin management |
| **Ant** (legacy) | root | `build.xml` — init/clean/compile/test/war/ear/dist/deploy + checkstyle/db-schema/backup/verify-deployment/generate-build-info targets, `build.properties` |
| **Jenkins** (scripted pipeline) | root | `Jenkinsfile` — multi-stage pipeline with Ant verification, build manifest generation, production deployment with DB backup + auto-rollback |
| **EAR packaging** | ear | `application.xml`, `jboss-app.xml`, `maven-ear-plugin` configuration |
| **WAR packaging** | web | `maven-war-plugin`, `web.xml`, servlet container deployment descriptor |

### Category 8: Data Access Beyond ORM
| Pattern | Module | Key Files |
|---------|--------|-----------|
| **Raw JDBC** | dao | `ClaimJdbcDao` — `PreparedStatement`, manual `ResultSet` mapping, batch operations, `try-finally` resource management |
| **Stored Procedures** (CallableStatement) | dao | `StoredProcedureDao` — `{? = call SP_CALCULATE_CLAIM_RESERVE(?)}`, IN/OUT/INOUT params, ResultSet-returning procs |
| **Database Functions** | dao | `FN_CLAIM_AGE_DAYS`, `FN_POLICY_CLAIMS_TOTAL`, `FN_CALCULATE_LOSS_RATIO` via CallableStatement |
| **Pessimistic Locking** | dao | `SP_LOCK_CLAIM_FOR_PROCESSING` — `SELECT ... FOR UPDATE`, manual transaction control |
| **Schema Management** | dao | `schema.sql`, `schema-procedures.sql`, `schema-security.sql`, `schema-etl.sql` — DDL, triggers, views, stored procedures, SCD Type 2 history, audit tables, ETL staging |

### Category 9: Enterprise Java Patterns
| Pattern | Module | Key Files |
|---------|--------|-----------|
| **EJB-Style Session Bean Facade** | service | `ClaimServiceSessionBean` — `@PostConstruct`/`@PreDestroy`, `Propagation.REQUIRED/REQUIRES_NEW/SUPPORTS/NOT_SUPPORTED` |
| **JTA-Style Transaction Management** | dao/service | `HibernateTransactionManager`, `@Transactional`, `rollbackFor`, programmatic transaction control |
| **Optimistic Locking** | domain | `@Version` on all entities, `StaleObjectStateException` handling |
| **JMX MBeans** | web | `ClaimProcessingMBean`, `ConnectionPoolMonitorMBean` — runtime monitoring and management |
| **Compensating Transactions** (Saga) | service | `CompensationChain` — LIFO undo stack with best-effort execution; `ClaimPayoutCoordinator` — 6-step payout saga across DB / payment gateway / JMS / SOAP with full compensation |

### Category 10: Modernization Frameworks
| Framework | Module | Key Files |
|-----------|--------|-----------|
| **Spring Boot 2.7** | modernization | `GreyLegacyModernApplication`, `@ImportResource` for legacy XML, `application.yml` |
| **MapStruct 1.5** | modernization | `ClaimMapper`, `PolicyMapper` — compile-time DTO mapping replacing hand-written converters |
| **Flyway 8.5** | modernization | `V1__baseline_schema.sql`, `V2__add_indexes_and_constraints.sql`, `V3__seed_reference_data.sql` |
| **Micrometer** | modernization | `ClaimMetricsService` — `Counter`, `Timer` replacing JMX MBeans for observability |
| **Spring Boot REST** | modernization | `ClaimRestController`, `PolicyRestController` — `@RestController` replacing Struts Actions |
| **Hibernate L2 Cache** (Ehcache) | dao | `ehcache.xml` — entity caches, collection caches, query cache, disk overflow |

### Application Server Configurations

Grey Legacy ships with production-grade configurations for multiple Java EE application servers, reflecting real enterprise multi-container deployment scenarios.

| Server | Config Files | Key Features |
|--------|-------------|--------------|
| **Apache Tomcat 8.5** | `config/tomcat/server.xml`, `context.xml`, `tomcat-users.xml` | JNDI DataSource (PostgreSQL, pool 10-50), JMS ConnectionFactory (ActiveMQ failover), HTTPS/TLS 1.2, AJP, sticky sessions, StuckThreadDetectionValve |
| **JBoss EAP 7 / WildFly** | `config/jboss/standalone-greylegacy.xml` | Datasources with vault credentials, Artemis JMS (DLQ, queues, topics), JAAS security domains, EJB3 pools, Undertow, modular logging |
| **IBM WebSphere** (conceptual) | `web/src/webapp/WEB-INF/ibm-web-bnd.xml` | Security role → LDAP group bindings, JNDI resource reference bindings |
| **Oracle WebLogic** (conceptual) | `web/src/webapp/WEB-INF/weblogic.xml` | Child-first classloading, session config, JSP precompilation, security role assignments, JNDI mappings |

### Security Infrastructure

| Component | Location | Description |
|-----------|----------|-------------|
| **JAAS LoginModule** | `web/src/main/java/.../security/GreyLegacyLoginModule.java` | Database-backed JAAS authentication, SHA-256 hashing, account lockout (5 failures), multi-container JNDI fallback |
| **JAAS Configuration** | `config/security/jaas.conf` | 4 login contexts: GreyLegacy (web), GreyLegacyWS (SOAP), GreyLegacyBatch (batch), GreyLegacyJMX (admin) |
| **Security Schema** | `dao/src/main/resources/schema-security.sql` | APP_USER, APP_USER_ROLE, LOGIN_AUDIT, DATA_ACCESS_AUDIT, SECURITY_CONFIG tables; seed users/roles; audit views |
| **Web Security** | `web/src/webapp/WEB-INF/web.xml` | 6 `security-constraint` blocks, FORM login, 5 `security-role` declarations, HttpOnly/Secure cookies, CONFIDENTIAL transport |
| **JNDI Utility** | `web/src/main/java/.../jndi/JndiLookupUtil.java` | Multi-container JNDI lookup with fallback (java:comp/env/, java:jboss/, java:/, bare name), cached InitialContext |
| **FORM Login Pages** | `web/src/webapp/login.jsp`, `loginError.jsp` | Container-managed FORM login via `j_security_check`, error/timeout/locked messaging, PRG redirect on failure |

### ETL Pipeline

| Component | Location | Description |
|-----------|----------|-------------|
| **Claims Export** | `scripts/etl-claims-export.sh` | PostgreSQL COPY → CSV, SHA-256 checksums, control files, SFTP transfer, email notification |
| **Claims Import** | `scripts/etl-claims-import.sh` | Directory polling, CSV → staging tables, validation, merge, reconciliation, archiving |
| **ETL Schema** | `dao/src/main/resources/schema-etl.sql` | ETL_BATCH_LOG, STG_CLAIM/POLICY/PAYMENT_IMPORT staging tables, validation/merge stored procedures, reconciliation views, data quality scorecard |
| **Sample Data** | `integration/src/main/resources/sample-data/` | `sample-claims-import.csv` (15 claims), `sample-policies-import.xml` (10 policies) |

## Technology Stack

| Layer | Technology | Version |
|-------|-----------|---------|
| Web Framework | Apache Struts 1 + Spring MVC | 1.3.10 / 4.3.30 |
| Dependency Injection | Spring Framework (XML-era) | 4.3.30.RELEASE |
| ORM | Hibernate + JPA + MyBatis | 5.2.18 / 2.1 / 3.5.11 |
| Database | H2 (dev) / PostgreSQL (prod) | 1.4.200 / 42.2.24 |
| Connection Pool | C3P0 + Commons DBCP | 0.9.5.5 / 1.4 |
| Web Services | Apache CXF (SOAP/WSDL) + WS-Security | 3.2.14 |
| Messaging | JMS / ActiveMQ | 5.15.16 |
| Integration | Apache Camel | 2.25.4 |
| Batch Processing | Quartz + Spring Batch | 2.3.2 / 3.0.10 |
| L2 Cache | Ehcache (Hibernate integration) | 2.10.9.2 |
| Modernization | Spring Boot + MapStruct + Flyway + Micrometer | 2.7.18 / 1.5.5 / 8.5.13 / 1.9.17 |
| View Layer | JSPs + Struts Taglibs + JSTL | — |
| Logging | SLF4J + Log4j 1.2 | 1.7.36 / 1.2.17 |
| Build | Maven (multi-module) + Ant | 3.x / — |
| Testing | JUnit 3/4 + Mockito + Spring Test | 4.13.2 / 2.28.2 |

## Claims Processing Workflows

### FNOL (First Notice of Loss)
- Struts form captures claimant info, policy number, incident details
- Validates policy coverage and status
- Creates claim entity via Hibernate
- Generates audit trail entry and initial snapshot

### Adjudication
- Multi-step service-layer workflow
- Validates coverage dates and policy status
- Auto-assigns adjuster by specialization
- Calculates preliminary liability (estimated loss - deductible, capped at coverage limit)

### Adjustment and Payout
- Adjuster review with recommendation (APPROVE/DENY/ESCALATE)
- Fraud scoring via nightly batch job (heuristic rules, 0-100 scale)
- Payout calculation with deductible and coverage limit application
- Payment scheduling with status lifecycle (SCHEDULED → PROCESSING → COMPLETED)

### Audit and History
- Append-only audit table (`@Immutable` entity)
- Entity versioning via `@Version`
- Historical claim snapshots at key lifecycle events
- Full audit trail with correlation IDs and user tracking

## Building

```bash
# Full build (all modules)
mvn clean install

# Ant build (legacy)
ant dist

# Skip modernization module
mvn clean install -pl !modernization
```

## Installation

Grey Legacy provides platform-specific installers for Windows, Linux, and macOS. Each installer handles all prerequisites, deploys the application on Apache Tomcat 8.5, registers a system service, and installs a desktop dashboard application.

### Prerequisites

- **Java 8+** (JDK) — must be installed and available on `PATH` (or `JAVA_HOME` set)
- **Network access** — if Tomcat is not bundled, the installer downloads it from Apache archives automatically

### Building the Installers

```bash
# Build the WAR and package distributable archives for all platforms
chmod +x installers/build-installers.sh
./installers/build-installers.sh

# Skip the Maven build if you already have a WAR
./installers/build-installers.sh --skip-build
```

This produces distributable archives in `dist/`:
- `greylegacy-1.0.0-windows.zip`
- `greylegacy-1.0.0-linux.tar.gz`
- `greylegacy-1.0.0-macos.tar.gz`

### Building the GUI Installer & Desktop App

```bash
cd installers
bash gui/build-gui.sh
```

This compiles and packages two JARs:
- `gui/greylegacy-installer.jar` — graphical installer/uninstaller wizard
- `gui/greylegacy-app.jar` — post-installation desktop dashboard

### Linux

**Install** (requires root):

```bash
sudo ./installers/linux/install.sh
```

| Option | Description |
|--------|-------------|
| `--silent` | Non-interactive installation |
| `--install-dir <path>` | Installation directory (default: `/opt/greylegacy`) |
| `--port <port>` | HTTP port (default: `8080`) |
| `--user <user>` | Service user account (default: `greylegacy`) |
| `--skip-firewall` | Skip firewall configuration |

What the installer does:
1. Verifies Java installation
2. Creates a dedicated service user (`greylegacy`)
3. Downloads and extracts Apache Tomcat 8.5.100 (or uses bundled/pre-installed)
4. Deploys the application WAR to Tomcat
5. Copies configuration files and generates `setenv.sh`
6. Sets file ownership and permissions
7. Creates a systemd service (`greylegacy`) with security hardening and auto-restart
8. Configures firewall (firewalld, ufw, or iptables)
9. Installs the desktop dashboard app with a `.desktop` shortcut in the application menu

**Uninstall**:

```bash
sudo ./installers/linux/uninstall.sh
```

| Option | Description |
|--------|-------------|
| `--silent` | Non-interactive uninstallation |
| `--keep-data` | Preserve application data directory |
| `--keep-logs` | Preserve log files |
| `--keep-user` | Keep the service user account |

**Service management after installation**:

```bash
sudo systemctl start greylegacy     # Start the service
sudo systemctl stop greylegacy      # Stop the service
sudo systemctl restart greylegacy   # Restart the service
sudo systemctl status greylegacy    # Check status
journalctl -u greylegacy -f         # Follow service logs
```

### Windows

**Install** (requires Administrator):

```cmd
installers\windows\install.cmd
```

| Option | Description |
|--------|-------------|
| `--silent` | Non-interactive installation |
| `--install-dir <path>` | Installation directory (default: `C:\GreyLegacy`) |
| `--port <port>` | HTTP port (default: `8080`) |

What the installer does:
1. Verifies Java installation and `JAVA_HOME`
2. Creates directory structure under the install directory
3. Downloads and extracts Apache Tomcat 8.5.100 (or uses bundled/pre-installed)
4. Deploys the application WAR to Tomcat
5. Copies configuration files and generates `setenv.bat`
6. Registers a Windows service (`GreyLegacyTomcat`) with auto-start and failure recovery
7. Adds a firewall rule for the HTTP port
8. Creates an Add/Remove Programs entry
9. Installs the desktop dashboard app with a Start Menu shortcut

**Uninstall** (requires Administrator):

```cmd
installers\windows\uninstall.cmd
```

| Option | Description |
|--------|-------------|
| `--silent` | Non-interactive uninstallation |
| `--keep-data` | Preserve the data directory |
| `--keep-logs` | Preserve log files |

**Service management after installation**:

```cmd
net start GreyLegacyTomcat    &rem Start the service
net stop GreyLegacyTomcat     &rem Stop the service
sc query GreyLegacyTomcat     &rem Check status
```

### macOS

**Install** (requires sudo):

```bash
sudo ./installers/macos/install.sh
```

| Option | Description |
|--------|-------------|
| `--silent` | Non-interactive installation |
| `--install-dir <path>` | Installation directory (default: `/usr/local/greylegacy`) |
| `--port <port>` | HTTP port (default: `8080`) |

What the installer does:
1. Verifies Java installation (uses `/usr/libexec/java_home`)
2. Creates directory structure under the install directory
3. Downloads and extracts Apache Tomcat 8.5.100 (or uses bundled/pre-installed)
4. Deploys the application WAR to Tomcat
5. Copies configuration files and generates `setenv.sh`
6. Creates a macOS LaunchDaemon (`com.greylegacy.tomcat`) for auto-start
7. Sets file permissions
8. Installs the desktop dashboard app in `/Applications/Grey Legacy.app`

**Uninstall**:

```bash
sudo ./installers/macos/uninstall.sh
```

| Option | Description |
|--------|-------------|
| `--silent` | Non-interactive uninstallation |
| `--keep-data` | Preserve the data directory |
| `--keep-logs` | Preserve log files |

**Service management after installation**:

```bash
sudo launchctl load /Library/LaunchDaemons/com.greylegacy.tomcat.plist     # Start
sudo launchctl unload /Library/LaunchDaemons/com.greylegacy.tomcat.plist   # Stop
sudo launchctl list | grep com.greylegacy.tomcat                           # Status
```

### GUI Installer

For a graphical installation experience, launch the installer wizard:

```bash
java -jar installers/gui/greylegacy-installer.jar              # Install wizard
java -jar installers/gui/greylegacy-installer.jar --uninstall  # Uninstall wizard
```

The wizard provides a 4-step guided process: Welcome → Configure (install directory, port) → Install (live progress) → Complete. It auto-detects the operating system and delegates to the appropriate platform CLI installer.

### Bundling Tomcat

To create self-contained installers that include Tomcat (no internet required):

| Platform | Place this file in the platform installer directory |
|----------|---------------------------------------------------|
| Windows | `apache-tomcat-8.5.100-windows-x64.zip` |
| Linux | `apache-tomcat-8.5.100.tar.gz` |
| macOS | `apache-tomcat-8.5.100.tar.gz` |

If Tomcat is not bundled, the installer downloads it automatically from Apache archives.

---

## Usage

### Accessing the Application

After installation and starting the service, the application is available at:

```
http://localhost:8080/greylegacy/
```

Replace `8080` with the port specified during installation if a custom port was used.

#### Default Pages

| URL | Description |
|-----|-------------|
| `/greylegacy/` | Main dashboard (redirects to login if unauthenticated) |
| `/greylegacy/login.jsp` | FORM-based login page |
| `/greylegacy/services/ClaimService?wsdl` | SOAP ClaimService WSDL |
| `/greylegacy/services/PolicyLookupService?wsdl` | SOAP PolicyLookupService WSDL |

### Desktop Dashboard App

After installation, a desktop dashboard application is available for managing the Grey Legacy service without using the command line.

**Launch from the application menu** (installed automatically) or manually:

```bash
java -jar /opt/greylegacy/greylegacy-app.jar          # Linux
java -jar C:\GreyLegacy\greylegacy-app.jar             # Windows
open "/Applications/Grey Legacy.app"                    # macOS
```

The dashboard provides:

| Feature | Description |
|---------|-------------|
| **Service Status** | Real-time monitoring with auto-refresh every 10 seconds (checks HTTP health and OS-level service status) |
| **Open Web UI** | Opens the application in your default web browser |
| **Start / Stop / Restart** | Control the Tomcat service directly from the dashboard |
| **Log Viewer** | Live tail of `catalina.out` with auto-scroll |
| **System Tray** | Minimizes to the system tray with a right-click popup menu for quick access |

### Viewing Logs

```bash
# Linux — follow systemd journal
journalctl -u greylegacy -f

# Linux / macOS — follow Tomcat log directly
tail -f /opt/greylegacy/logs/catalina.out         # Linux
tail -f /usr/local/greylegacy/logs/catalina.out   # macOS

# Windows — open in text editor
type C:\GreyLegacy\logs\catalina.out
```

### Health Checks

A health check script is provided for monitoring:

```bash
chmod +x scripts/health-check.sh
./scripts/health-check.sh
```

---

## Deployment

```bash
# WAR deployment to Tomcat 8.5+
cp web/target/grey-legacy.war $CATALINA_HOME/webapps/
# Use config/tomcat/server.xml for production Tomcat configuration
# Use config/tomcat/tomcat-users.xml for container-managed auth

# EAR deployment to JBoss/WildFly
cp ear/target/grey-legacy.ear $JBOSS_HOME/standalone/deployments/
# Use config/jboss/standalone-greylegacy.xml for JBoss EAP configuration

# Spring Boot modernization module
java -jar modernization/target/grey-legacy-modernization.jar

# JAAS configuration (required for all containers)
# Set -Djava.security.auth.login.config=config/security/jaas.conf
```

## Key Configuration Files

| File | Purpose |
|------|---------|
| `web/src/webapp/WEB-INF/web.xml` | Servlet config, filter chain, CXF servlet, security constraints, FORM login |
| `web/src/webapp/WEB-INF/struts-config.xml` | Struts action mappings, form beans, forwards |
| `web/src/webapp/WEB-INF/validation.xml` | Struts Validator framework rules |
| `web/src/webapp/WEB-INF/applicationContext.xml` | Root Spring context, TX advice, AOP |
| `web/src/webapp/WEB-INF/ibm-web-bnd.xml` | WebSphere security role → LDAP group bindings |
| `web/src/webapp/WEB-INF/weblogic.xml` | WebLogic deployment descriptor (classloading, sessions, JNDI) |
| `dao/src/main/resources/applicationContext-dao.xml` | DataSource, SessionFactory, MyBatis, DAOs |
| `dao/src/main/resources/mybatis-config.xml` | MyBatis global config, type aliases, mappers |
| `dao/src/main/resources/ehcache.xml` | Hibernate L2 cache regions (entity, collection, query) |
| `dao/src/main/resources/schema-security.sql` | Security/audit tables, seed users and roles |
| `dao/src/main/resources/schema-etl.sql` | ETL staging tables, validation/merge procedures, reconciliation views |
| `service/src/main/resources/applicationContext-service.xml` | Service beans with property injection |
| `integration/src/main/resources/applicationContext-integration.xml` | CXF SOAP endpoints |
| `integration/src/main/resources/applicationContext-ws-security.xml` | WS-Security interceptors (WSS4J) |
| `integration/src/main/resources/applicationContext-camel.xml` | Apache Camel routes and context |
| `integration/src/main/resources/applicationContext-jms.xml` | ActiveMQ, JMS queues, message listeners |
| `batch/src/main/resources/applicationContext-batch.xml` | Quartz scheduler, job triggers |
| `batch/src/main/resources/applicationContext-spring-batch.xml` | Spring Batch jobs, readers, writers |
| `dao/src/main/resources/hibernate.cfg.xml` | Hibernate session factory configuration |
| `dao/src/main/resources/database.properties` | Database connection and pool settings |
| `config/tomcat/server.xml` | Tomcat 8.5 production config (JNDI, HTTPS, AJP, pooling) |
| `config/jboss/standalone-greylegacy.xml` | JBoss EAP 7 config (datasources, JMS, JAAS, logging) |
| `config/security/jaas.conf` | JAAS login contexts (web, SOAP, batch, JMX) |
| `config/checkstyle/checkstyle.xml` | Checkstyle rules — legacy-friendly thresholds (150-line methods, 20 cyclomatic complexity), m_/p_ prefix patterns |
| `modernization/src/main/resources/application.yml` | Spring Boot config (replaces scattered XML+properties) |

## SOAP Endpoints

| Endpoint | URL | Operations | Security |
|----------|-----|-----------|----------|
| ClaimService | `/services/ClaimService` | submitClaim, getClaimStatus, getClaimDetails, updateClaimStatus | WS-Security UsernameToken |
| PolicyLookupService | `/services/PolicyLookupService` | lookupPolicy, validateCoverage | WS-Security UsernameToken |

WSDLs available at `{endpoint}?wsdl`

### Outbound SOAP Clients (Partner Services)

| Client | Target | Pattern |
|--------|--------|---------|
| FraudScoringServiceClient | External fraud scoring API | WS-Security UsernameToken, exponential backoff retry (3 attempts) |
| VinLookupServiceClient | External VIN decoder API | Circuit breaker (5-failure threshold, 60s cooldown), retry, VIN masking |

## Batch Jobs

| Job | Schedule | Purpose |
|-----|----------|---------|
| ClaimAgingJob | 2:00 AM daily | 4-tier aging analysis (30/60/90/120 days) |
| FraudScoringJob | 2:30 AM daily | Heuristic fraud scoring for unscored claims |
| PayoutSchedulingJob | 3:00 AM daily | Process scheduled payments |
| PremiumRecalculationJob | 1:00 AM monthly | Claims-based premium adjustments |
| ClaimExportJob | 4:00 AM daily | Spring Batch CSV export with SSN masking, ItemStream checkpointing |
| PartitionedExportJob | On-demand | Partitioned variant (4-way range split) for high-volume export |
| PolicyImportJob | On-demand | CSV policy import with validation, duplicate detection, idempotent upsert |
| ReconciliationJob | 5:00 AM daily | Payment reconciliation via JDBC aggregate |

### Spring Batch Infrastructure
- **JDBC JobRepository** — persistent job metadata for restartability (`schema-batch.sql`)
- **JobOperator** — operational control (stop/restart/abandon) without restarting the app
- **Quartz→Spring Batch bridge** — `SpringBatchQuartzJobLauncher` converts Quartz triggers to Spring Batch launches with unique parameters

## Modernization

See [MODERNIZATION_ROADMAP.md](MODERNIZATION_ROADMAP.md) for the full migration strategy covering:
- Struts → Spring Boot migration
- SOAP → REST migration
- Hibernate XML → annotations migration
- MyBatis integration for reporting
- Monolith decomposition
- Testing strategy
- CI/CD pipeline
- Observability (Micrometer/Prometheus replacing JMX)
- Risk mitigation matrix with probability/impact analysis
- Technical debt inventory with phase-by-phase resolution
- Staffing and budget model
- Architecture Decision Records (ADRs)

## Documentation

| Document | Description |
|----------|-------------|
| [ARCHITECTURE.md](ARCHITECTURE.md) | Layered architecture, transaction boundaries, EIP patterns, data access strategies, security architecture, error handling, deployment topology, design decision log |
| [MODERNIZATION_ROADMAP.md](MODERNIZATION_ROADMAP.md) | 7-phase migration strategy (18-24 months), technology migration matrix, ADRs, staffing model |
| [TROUBLESHOOTING_LEGACY_SYSTEMS.md](TROUBLESHOOTING_LEGACY_SYSTEMS.md) | 14-section debugging guide: Spring/Hibernate/SOAP/JMS/Batch/Deployment troubleshooting with real stack traces and fix recipes |
