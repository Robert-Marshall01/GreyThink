# Troubleshooting Legacy Enterprise Java Systems

> A battle-tested runbook for diagnosing and resolving issues in XML-configured Spring/Hibernate/Struts
> enterprise applications. Written from years of production incidents in the **Grey Legacy** insurance
> claims platform and similar monolithic Java EE systems.

---

## Table of Contents

1. [Reading Stack Traces from XML-Configured Systems](#1-reading-stack-traces-from-xml-configured-systems)
2. [Debugging Hibernate Session Issues](#2-debugging-hibernate-session-issues)
3. [Diagnosing N+1 Queries](#3-diagnosing-n1-queries)
4. [JDBC Connection Pool Exhaustion](#4-jdbc-connection-pool-exhaustion)
5. [Troubleshooting SOAP Faults](#5-troubleshooting-soap-faults)
6. [Interpreting WSDL/XSD Errors](#6-interpreting-wsdlxsd-errors)
7. [Understanding Classloader Issues in Monoliths](#7-understanding-classloader-issues-in-monoliths)
8. [Transaction Management Debugging](#8-transaction-management-debugging)
9. [Thread Safety Issues in Servlet Containers](#9-thread-safety-issues-in-servlet-containers)
10. [Legacy Environment Diagnosis](#10-legacy-environment-diagnosis)

---

## 1. Reading Stack Traces from XML-Configured Systems

### Spring BeanCreationException Chain Analysis

Spring XML-configured applications produce deeply nested exception chains during context startup.
The key skill is reading from **bottom to top** — the root cause is always at the deepest
"Caused by" block.

**Pattern:** The outermost exception is always `BeanCreationException`. Each layer adds context
about _which bean_ failed and _why_. Read the chain like a call stack:

```
BeanCreationException → BeanCreationException → BeanCreationException → ROOT CAUSE
(web context)            (service bean)           (dao bean)              (actual error)
```

### Example: Missing Bean Definition

```
org.springframework.beans.factory.BeanCreationException:
  Error creating bean with name 'claimService' defined in
  class path resource [applicationContext-service.xml]:
  Cannot resolve reference to bean 'claimDao' while setting
  bean property 'claimDao';

  nested exception is org.springframework.beans.factory.NoSuchBeanDefinitionException:
  No bean named 'claimDao' is defined

    at org.springframework.beans.factory.support.BeanDefinitionValueResolver
      .resolveReference(BeanDefinitionValueResolver.java:342)
    at org.springframework.beans.factory.support.AbstractAutowireCapableBeanFactory
      .createBean(AbstractAutowireCapableBeanFactory.java:456)
    ...

Caused by: org.springframework.beans.factory.NoSuchBeanDefinitionException:
  No bean named 'claimDao' is defined
    at org.springframework.beans.factory.support.DefaultListableBeanFactory
      .getBeanDefinition(DefaultListableBeanFactory.java:687)
```

**How to read this:**
1. The error is in `applicationContext-service.xml` (stated in the first line)
2. The bean `claimService` depends on `claimDao`
3. `claimDao` is not defined anywhere Spring can find it
4. **Fix:** Check that `applicationContext-dao.xml` is imported, or that the bean ID matches exactly

**Common causes:**
- Typo in bean name (`claimDAO` vs `claimDao`)
- Missing `<import resource="applicationContext-dao.xml"/>` in parent context
- Bean defined in a context file that is not listed in `web.xml` `contextConfigLocation`

### Example: Circular Dependency

```
org.springframework.beans.factory.BeanCreationException:
  Error creating bean with name 'claimService' defined in
  class path resource [applicationContext-service.xml]:
  Bean with name 'claimService' has been injected into other beans
  [policyService] in its raw version as part of a circular reference,
  but has eventually been wrapped.

  This means that said other beans do not use the final version of
  the bean. This is often the result of over-eager type matching —
  consider using 'getBeanNamesOfType' with the 'allowEagerInit'
  flag turned off, for example.

Caused by: org.springframework.beans.factory.BeanCurrentlyInCreationException:
  Error creating bean with name 'policyService':
  Requested bean is currently in creation:
  Is there an unresolvable circular reference?
```

**How to read this:**
1. `claimService` → depends on `policyService` → depends on `claimService` (cycle!)
2. Spring _can_ resolve circular references for setter-injection but **not** for constructor-injection
3. AOP proxies (e.g., `@Transactional`) make this worse — the raw bean vs. proxied bean mismatch

**Fix options:**
- Refactor to break the cycle (extract shared logic into a third bean)
- Switch one dependency from constructor injection to setter injection
- Use `@Lazy` on one of the injected dependencies
- Add `lazy-init="true"` on one of the `<bean>` definitions in XML

### Identifying Which XML File Has the Error

The exception message always includes:

```
defined in class path resource [applicationContext-service.xml]
```

or

```
defined in ServletContext resource [/WEB-INF/applicationContext.xml]
```

- **`class path resource`** → file is on the classpath (e.g., `src/main/resources/`)
- **`ServletContext resource`** → file is under `WEB-INF/`
- **`file [/opt/app/config/...]`** → file loaded from filesystem path

### ClassNotFoundException: web.xml vs applicationContext.xml

When you see `ClassNotFoundException`, the location tells you where to look:

**In web.xml (listener/filter/servlet class):**
```
javax.servlet.ServletException: Error instantiating servlet class
  com.greylegacy.web.ClaimSearchServlet

Caused by: java.lang.ClassNotFoundException:
  com.greylegacy.web.ClaimSearchServlet
```
→ The class is not in `WEB-INF/classes` or any JAR in `WEB-INF/lib`. Check your build artifact.

**In applicationContext.xml (bean class):**
```
org.springframework.beans.factory.CannotLoadBeanClassException:
  Cannot find class [com.greylegacy.service.ClaimService]
  for bean with name 'claimService' defined in class path resource
  [applicationContext-service.xml]

Caused by: java.lang.ClassNotFoundException:
  com.greylegacy.service.ClaimService
```
→ The module JAR containing that class is missing from the deployment. In Grey Legacy,
this means the `service` module JAR wasn't included in the WAR's `WEB-INF/lib`.

---

## 2. Debugging Hibernate Session Issues

### LazyInitializationException

**The most infamous Hibernate error.** Occurs when you access a lazily-loaded collection or
proxy _after_ the Hibernate Session has been closed.

**Stack trace pattern:**
```
org.hibernate.LazyInitializationException:
  failed to lazily initialize a collection of role:
  com.greylegacy.domain.Policy.claims, could not initialize proxy - no Session

    at org.hibernate.collection.internal.AbstractPersistentCollection
      .throwLazyInitializationException(AbstractPersistentCollection.java:587)
    at org.hibernate.collection.internal.AbstractPersistentCollection
      .withTemporarySessionIfNeeded(AbstractPersistentCollection.java:204)
    at org.hibernate.collection.internal.AbstractPersistentCollection
      .initialize(AbstractPersistentCollection.java:566)
    at org.hibernate.collection.internal.AbstractPersistentCollection
      .read(AbstractPersistentCollection.java:135)
    at org.hibernate.collection.internal.PersistentBag
      .iterator(PersistentBag.java:275)
    at com.greylegacy.web.actions.PolicyDetailAction.execute(PolicyDetailAction.java:42)
```

**The problem:**
```java
// In service layer — Session is open here
public Policy findPolicy(Long id) {
    Policy policy = policyDao.findById(id); // Session open, claims NOT loaded yet
    return policy;
}   // <-- Session closes when transaction commits

// In Struts Action or JSP — Session is CLOSED
Policy policy = policyService.findPolicy(id);
for (Claim claim : policy.getClaims()) {  // BOOM! LazyInitializationException
    // ...
}
```

**Fix 1: OpenSessionInView filter (web.xml)**
```xml
<!-- Keep Hibernate Session open through the entire HTTP request -->
<filter>
    <filter-name>openSessionInView</filter-name>
    <filter-class>
        org.springframework.orm.hibernate5.support.OpenSessionInViewFilter
    </filter-class>
</filter>
<filter-mapping>
    <filter-name>openSessionInView</filter-name>
    <url-pattern>/*</url-pattern>
</filter-mapping>
```

> **Warning:** OpenSessionInView is considered an anti-pattern by many. It can mask N+1 problems
> and cause unexpected database hits during view rendering.

**Fix 2: Explicit initialization in service layer (preferred)**
```java
public Policy findPolicyWithClaims(Long id) {
    Policy policy = policyDao.findById(id);
    Hibernate.initialize(policy.getClaims());  // Force load while Session is open
    return policy;
}
```

**Fix 3: Fetch join query**
```java
public Policy findPolicyWithClaims(Long id) {
    return (Policy) session.createQuery(
        "SELECT p FROM Policy p LEFT JOIN FETCH p.claims WHERE p.id = :id")
        .setParameter("id", id)
        .uniqueResult();
}
```

### NonUniqueObjectException

**Occurs when two different Java objects with the same identifier exist in the same Session.**

```
org.hibernate.NonUniqueObjectException:
  A different object with the same identifier value was already
  associated with the session:
  [com.greylegacy.domain.Claim#1042]
```

**Common cause:**
```java
Claim claimFromQuery = session.get(Claim.class, 1042L);   // attached to session
Claim claimFromDto = new Claim();
claimFromDto.setId(1042L);
session.update(claimFromDto);   // BOOM — two objects with id 1042
```

**Fix:** Use `session.merge()` instead of `session.update()`:
```java
Claim merged = (Claim) session.merge(claimFromDto);  // Merges state into managed entity
```

### StaleObjectStateException (Optimistic Locking Failure)

**Occurs when two transactions modify the same row.** Grey Legacy's `BaseEntity` uses `@Version`
which enables optimistic locking.

```
org.hibernate.StaleObjectStateException:
  Row was updated or deleted by another transaction
  (or unsaved-value mapping was incorrect):
  [com.greylegacy.domain.Claim#1042]
```

**Scenario:**
1. User A loads Claim #1042 (version=3)
2. User B loads Claim #1042 (version=3)
3. User A saves changes → version becomes 4 ✓
4. User B saves changes → Hibernate checks version, finds 4 ≠ 3 → BOOM

**Fix:** Catch and retry, or show conflict resolution UI:
```java
try {
    claimService.updateClaim(claim);
} catch (StaleObjectStateException e) {
    // Reload fresh data, show user a merge/conflict screen
    Claim fresh = claimService.findClaim(claim.getId());
    request.setAttribute("conflictClaim", fresh);
    return mapping.findForward("conflictResolution");
}
```

### Session Is Closed Errors

```
org.hibernate.SessionException: Session is closed!
    at org.hibernate.internal.AbstractSessionImpl.errorIfClosed(AbstractSessionImpl.java:133)
```

**Common causes:**
- Manually closing the session and then trying to use it again
- Session-per-request pattern not configured correctly
- Exception during transaction causing session to be invalidated

**Fix:** Never manually close sessions managed by Spring. Let `HibernateTransactionManager`
or `OpenSessionInViewFilter` manage the lifecycle.

### Session Boundary Management

**Rule of thumb:**
- **Service layer** owns the transaction boundary (`@Transactional`)
- **DAO layer** uses the session provided by the current transaction
- **Web layer** should _never_ directly interact with the Hibernate Session
- All lazy loading must complete within the transaction (or use OSIV)

```java
// CORRECT: Service controls the boundary
@Transactional
public ClaimSummary getClaimSummary(Long claimId) {
    Claim claim = claimDao.findById(claimId);
    // Access collections INSIDE the transaction
    int paymentCount = claim.getPayments().size();
    int noteCount = claim.getNotes().size();
    return new ClaimSummary(claim, paymentCount, noteCount);
}
```

---

## 3. Diagnosing N+1 Queries

### What Is N+1?

The N+1 problem occurs when loading a collection of N parent entities triggers N additional
queries to load their children — one query per parent.

**Concrete example with Policy → Claims:**

```java
// This code looks innocent...
List<Policy> policies = session.createQuery("FROM Policy").list();  // 1 query
for (Policy p : policies) {
    System.out.println(p.getClaims().size());  // N queries (one per policy!)
}
```

**Generated SQL (with 100 policies):**
```sql
-- Query 1: Load all policies
SELECT p.* FROM POLICY p;

-- Query 2: Load claims for policy #1
SELECT c.* FROM CLAIM c WHERE c.POLICY_ID = 1;

-- Query 3: Load claims for policy #2
SELECT c.* FROM CLAIM c WHERE c.POLICY_ID = 2;

-- ... 98 more queries ...

-- Query 101: Load claims for policy #100
SELECT c.* FROM CLAIM c WHERE c.POLICY_ID = 100;
```

**Total: 101 queries instead of 2.** On a system with 10,000 policies, that's 10,001 queries.

### How to Detect

**Step 1: Enable SQL logging in `hibernate.cfg.xml`:**
```xml
<property name="hibernate.show_sql">true</property>
<property name="hibernate.format_sql">true</property>
<property name="hibernate.use_sql_comments">true</property>
```

**Step 2: Enable statistics in your session factory:**
```xml
<property name="hibernate.generate_statistics">true</property>
```

**Step 3: Log and count queries:**
```java
Statistics stats = sessionFactory.getStatistics();
stats.setStatisticsEnabled(true);
long before = stats.getQueryExecutionCount();

// ... execute business logic ...

long after = stats.getQueryExecutionCount();
log.warn("Queries executed: " + (after - before));
```

**Step 4: Watch for patterns in SQL logs:**
Look for the same SELECT statement repeated with different parameter values — that's your
N+1's fingerprint.

### How to Fix

**Fix 1: HQL Fetch Join**
```java
// BEFORE: N+1 (101 queries for 100 policies)
List<Policy> policies = session.createQuery("FROM Policy").list();

// AFTER: Single join query (1 query)
List<Policy> policies = session.createQuery(
    "SELECT DISTINCT p FROM Policy p LEFT JOIN FETCH p.claims"
).list();
```

**Fix 2: @BatchSize annotation**
```java
@OneToMany(mappedBy = "policy", fetch = FetchType.LAZY)
@BatchSize(size = 25)  // Load claims in batches of 25 policies at a time
private List<Claim> claims;
```

Generated SQL with `@BatchSize(size = 25)` and 100 policies:
```sql
-- Query 1: Load all policies
SELECT p.* FROM POLICY p;

-- Query 2: Batch-load claims for policies 1-25
SELECT c.* FROM CLAIM c WHERE c.POLICY_ID IN (1, 2, 3, ... 25);

-- Query 3: Batch-load claims for policies 26-50
SELECT c.* FROM CLAIM c WHERE c.POLICY_ID IN (26, 27, 28, ... 50);

-- Query 4-5: Two more batch queries
-- Total: 5 queries instead of 101
```

**Fix 3: Subselect Fetching**
```java
@OneToMany(mappedBy = "policy", fetch = FetchType.LAZY)
@Fetch(FetchMode.SUBSELECT)
private List<Claim> claims;
```

Generated SQL:
```sql
-- Query 1: Load all policies
SELECT p.* FROM POLICY p;

-- Query 2: Subselect loads ALL claims for ALL loaded policies
SELECT c.* FROM CLAIM c WHERE c.POLICY_ID IN (SELECT p.ID FROM POLICY p);
-- Total: 2 queries
```

**Fix 4: JPA EntityGraph**
```java
@EntityGraph(attributePaths = {"claims", "claims.payments"})
@NamedQuery(name = "Policy.findAllWithClaims",
    query = "SELECT p FROM Policy p")
```

**Fix 5: Criteria API**
```java
CriteriaBuilder cb = session.getCriteriaBuilder();
CriteriaQuery<Policy> cq = cb.createQuery(Policy.class);
Root<Policy> root = cq.from(Policy.class);
root.fetch("claims", JoinType.LEFT);
cq.select(root).distinct(true);
List<Policy> policies = session.createQuery(cq).getResultList();
```

### Performance Impact Metrics

| Scenario               | Queries | DB Round-trips | Typical Latency |
|------------------------|---------|----------------|-----------------|
| N+1 (100 policies)     | 101     | 101            | ~2,000 ms       |
| Fetch Join             | 1       | 1              | ~50 ms          |
| @BatchSize(25)         | 5       | 5              | ~100 ms         |
| Subselect              | 2       | 2              | ~60 ms          |

---

## 4. JDBC Connection Pool Exhaustion

### Symptoms

```
java.sql.SQLException: Cannot acquire a new connection.
  All pooled connections are in use.

com.mchange.v2.resourcepool.CannotAcquireResourceException:
  A ResourcePool could not acquire a resource from its primary factory
  or source.
    at com.mchange.v2.resourcepool.BasicResourcePool.awaitAvailable(BasicResourcePool.java:1422)
    at com.mchange.v2.resourcepool.BasicResourcePool.checkoutResource(BasicResourcePool.java:594)
```

Other symptoms:
- Application hangs — pages load forever
- Thread dump shows many threads stuck on `awaitAvailable`
- Eventual `TimeoutException` after pool checkout timeout

### C3P0 Specific Diagnostics

Add these to `database.properties` to detect leaked connections:

```properties
# Detect connections not returned to pool within 30 seconds
c3p0.unreturnedConnectionTimeout=30

# Log the stack trace of where the leaked connection was checked out
c3p0.debugUnreturnedConnectionStackTraces=true
```

When enabled, you'll see stack traces in the log showing exactly where leaked connections
were acquired:

```
WARN  [c3p0] A checked-out resource is overdue, and will be destroyed.
  Checked out at:
    java.lang.Thread.getStackTrace(Thread.java:1559)
    com.mchange.v2.c3p0.impl.NewProxyConnection.<init>(NewProxyConnection.java:85)
    ...
    com.greylegacy.dao.ClaimDao.findByClaimNumber(ClaimDao.java:87)
    com.greylegacy.service.ClaimService.processClaim(ClaimService.java:142)
```

### Common Causes

**Cause 1: Unclosed connections in JDBC code**
```java
// BROKEN: Connection leak!
public Claim findByClaimNumber(String claimNumber) {
    Connection conn = dataSource.getConnection();
    PreparedStatement ps = conn.prepareStatement(
        "SELECT * FROM CLAIM WHERE CLAIM_NUMBER = ?");
    ps.setString(1, claimNumber);
    ResultSet rs = ps.executeQuery();
    if (rs.next()) {
        return mapClaim(rs);
    }
    // BUG: conn is never closed — it leaks if an exception occurs or if we return early
    return null;
}
```

**Fixed:**
```java
public Claim findByClaimNumber(String claimNumber) {
    Connection conn = null;
    PreparedStatement ps = null;
    ResultSet rs = null;
    try {
        conn = dataSource.getConnection();
        ps = conn.prepareStatement(
            "SELECT * FROM CLAIM WHERE CLAIM_NUMBER = ?");
        ps.setString(1, claimNumber);
        rs = ps.executeQuery();
        if (rs.next()) {
            return mapClaim(rs);
        }
        return null;
    } catch (SQLException e) {
        throw new DataAccessException("Failed to find claim: " + claimNumber, e);
    } finally {
        // ALWAYS close in reverse order, in finally block
        closeQuietly(rs);
        closeQuietly(ps);
        closeQuietly(conn);
    }
}
```

**Even better — try-with-resources (Java 7+):**
```java
public Claim findByClaimNumber(String claimNumber) {
    String sql = "SELECT * FROM CLAIM WHERE CLAIM_NUMBER = ?";
    try (Connection conn = dataSource.getConnection();
         PreparedStatement ps = conn.prepareStatement(sql)) {
        ps.setString(1, claimNumber);
        try (ResultSet rs = ps.executeQuery()) {
            if (rs.next()) {
                return mapClaim(rs);
            }
        }
        return null;
    } catch (SQLException e) {
        throw new DataAccessException("Failed to find claim: " + claimNumber, e);
    }
}
```

**Cause 2: Long-running transactions holding connections**
- Batch jobs that process thousands of claims in a single transaction
- Calls to external services (SOAP, REST) while holding a database connection

**Cause 3: Pool too small for concurrent load**

### How to Monitor

**JMX MBean metrics:** Connect JConsole to the running JVM. Navigate to:
```
com.mchange.v2.c3p0 → PooledDataSource → numBusyConnections
                                        → numIdleConnections
                                        → numConnections
```

**Programmatic check:**
```java
ComboPooledDataSource cpds = (ComboPooledDataSource) dataSource;
log.info("Busy: " + cpds.getNumBusyConnections());
log.info("Idle: " + cpds.getNumIdleConnections());
log.info("Total: " + cpds.getNumConnections());
```

### Recommended Pool Configuration

```properties
# Pool sizing
c3p0.minPoolSize=5
c3p0.maxPoolSize=30
c3p0.initialPoolSize=5
c3p0.acquireIncrement=3

# Timeout: how long to wait for a connection before failing (ms)
c3p0.checkoutTimeout=10000

# Idle connection testing
c3p0.idleConnectionTestPeriod=300
c3p0.preferredTestQuery=SELECT 1

# Connection age management
c3p0.maxIdleTime=600
c3p0.maxConnectionAge=3600

# Leak detection (enable in dev/staging, disable in production)
c3p0.unreturnedConnectionTimeout=30
c3p0.debugUnreturnedConnectionStackTraces=true
```

---

## 5. Troubleshooting SOAP Faults

### Common CXF SOAP Faults

**HTTP 500 with SOAP fault envelope:**
```xml
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <soap:Fault>
      <faultcode>soap:Server</faultcode>
      <faultstring>
        Unmarshalling Error: unexpected element
        (uri:"http://greylegacy.com/claims", local:"ClaimRequest").
        Expected elements are
        &lt;{http://greylegacy.com/claims/v2}ClaimRequest&gt;
      </faultstring>
    </soap:Fault>
  </soap:Body>
</soap:Envelope>
```

**This means:** Namespace mismatch. The client sent `claims` namespace but the server
expects `claims/v2`.

### WSDL Validation Errors

```
org.apache.cxf.service.factory.ServiceConstructionException:
  Failed to create service.
    ...
Caused by: javax.wsdl.WSDLException:
  WSDLException: faultCode=PARSER_ERROR:
  Problem parsing 'file:/app/wsdl/ClaimService.wsdl'.
  org.xml.sax.SAXParseException: src-resolve:
  Cannot resolve the name 'tns:ClaimType' to a(n) 'type definition' component.
```

**Fix:** Ensure the XSD type is defined and the namespace prefix maps correctly:
```xml
<!-- In ClaimService.wsdl — make sure this import exists -->
<wsdl:types>
  <xsd:schema>
    <xsd:import namespace="http://greylegacy.com/claims/types"
                schemaLocation="claims-types.xsd"/>
  </xsd:schema>
</wsdl:types>
```

### JAXB Marshalling Errors

```
javax.xml.bind.MarshalException:
  unable to marshal type "com.greylegacy.integration.dto.ClaimResponse"
  as an element because it is missing an @XmlRootElement annotation

    at com.sun.xml.bind.v2.runtime.XMLSerializer.reportError(XMLSerializer.java:244)
```

**Fix:** Add the annotation:
```java
@XmlRootElement(name = "ClaimResponse",
                namespace = "http://greylegacy.com/claims/types")
@XmlType(name = "ClaimResponseType",
         namespace = "http://greylegacy.com/claims/types")
public class ClaimResponse {
    // ...
}
```

### Reading SOAP Fault XML

Enable CXF message logging to see raw request/response:

```xml
<!-- In applicationContext-integration.xml -->
<cxf:bus>
  <cxf:features>
    <cxf:logging/>
  </cxf:features>
</cxf:bus>
```

Or via system property:
```
-Dorg.apache.cxf.Logger=org.apache.cxf.common.logging.Log4jLogger
```

**Log output shows full SOAP exchange:**
```
INFO  [org.apache.cxf.services.ClaimService] Outbound Message
--------------------------------------
ID: 1
Address: https://partner.example.com/claims/v2
Encoding: UTF-8
Content-Type: text/xml
Headers: {SOAPAction=["submitClaim"]}
Payload:
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <ns2:SubmitClaimRequest xmlns:ns2="http://greylegacy.com/claims/types">
      <claimNumber>CLM-2025-001042</claimNumber>
      ...
    </ns2:SubmitClaimRequest>
  </soap:Body>
</soap:Envelope>
--------------------------------------
```

### Certificate/SSL Issues

```
javax.net.ssl.SSLHandshakeException:
  sun.security.validator.ValidatorException:
  PKIX path building failed:
  sun.security.provider.certpath.SunCertPathBuilderException:
  unable to find valid certification path to requested target
```

**Diagnosis:**
```bash
# Test the endpoint SSL certificate
openssl s_client -connect partner.example.com:443 -showcerts

# Import the certificate into the JVM truststore
keytool -import -alias partner-cert \
  -file partner-cert.pem \
  -keystore $JAVA_HOME/lib/security/cacerts \
  -storepass changeit
```

Or configure CXF to use a custom truststore:
```xml
<http-conf:conduit name="*.http-conduit">
  <http-conf:tlsClientParameters>
    <sec:trustManagers>
      <sec:keyStore type="JKS"
                    file="/app/config/truststore.jks"
                    password="changeit"/>
    </sec:trustManagers>
  </http-conf:tlsClientParameters>
</http-conf:conduit>
```

### Testing with curl

```bash
# Send a SOAP request via curl for testing
curl -X POST \
  -H "Content-Type: text/xml; charset=utf-8" \
  -H "SOAPAction: submitClaim" \
  -d @soap-request.xml \
  http://localhost:8080/services/ClaimService

# Fetch the WSDL
curl http://localhost:8080/services/ClaimService?wsdl
```

---

## 6. Interpreting WSDL/XSD Errors

### Schema Validation Failures

```
org.xml.sax.SAXParseException:
  cvc-complex-type.2.4.a: Invalid content was found starting with
  element 'claimAmount'. One of '{estimatedLoss}' is expected.
```

**Meaning:** The XML element name doesn't match the schema. The XSD expects `estimatedLoss`
but the message contains `claimAmount`. Check the generated JAXB classes vs. the XSD.

### Complex Type Resolution Errors

```
org.xml.sax.SAXParseException:
  src-resolve: Cannot resolve the name 'tns:PolicyInfoType'
  to a(n) 'type definition' component.
```

**Causes:**
- The type is defined in a different XSD that hasn't been imported
- The namespace prefix (`tns:`) points to the wrong namespace URI
- Typo in the type name

**Fix:** Verify namespace URIs match between WSDL and XSD:
```xml
<!-- claims-types.xsd -->
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            targetNamespace="http://greylegacy.com/claims/types"
            xmlns:tns="http://greylegacy.com/claims/types"
            xmlns:pol="http://greylegacy.com/policy/types">

  <!-- Must import the policy types schema -->
  <xsd:import namespace="http://greylegacy.com/policy/types"
              schemaLocation="policy-types.xsd"/>

  <xsd:complexType name="ClaimWithPolicyType">
    <xsd:sequence>
      <xsd:element name="claimNumber" type="xsd:string"/>
      <xsd:element name="policyInfo" type="pol:PolicyInfoType"/>
    </xsd:sequence>
  </xsd:complexType>
</xsd:schema>
```

### Import vs Include in WSDL

| Feature     | `<xsd:import>`                          | `<xsd:include>`                           |
|-------------|-----------------------------------------|-------------------------------------------|
| Namespaces  | Brings in schema from **different** NS  | Brings in schema from **same** NS         |
| Use when    | Combining types from multiple schemas   | Splitting a large schema into parts       |
| Common bug  | Missing `namespace` attribute           | Target NS mismatch between files          |

### Namespace Management Pitfalls

1. **Default namespace confusion:** Omitting `xmlns:tns` and relying on default namespace
   causes elements to be unqualified in generated XML
2. **elementFormDefault:** If set to `"qualified"`, all elements must be namespace-prefixed.
   If `"unqualified"` (default), only top-level elements get prefixed
3. **Version drift:** WSDL v1 uses namespace `http://greylegacy.com/claims` but v2 uses
   `http://greylegacy.com/claims/v2` — clients built against v1 will fail against v2

### CXF Code Generation Issues

```bash
# Re-generate JAXB classes from WSDL
mvn cxf-codegen-plugin:wsdl2java

# Common problems:
# 1. Generated classes overwrite custom modifications
# 2. Binding file (.xjb) needed to resolve naming conflicts
# 3. Multiple WSDLs generating classes into same package
```

**Binding customization for conflicting names:**
```xml
<!-- bindings.xjb -->
<jaxb:bindings xmlns:jaxb="http://java.sun.com/xml/ns/jaxb" version="2.1">
  <jaxb:bindings schemaLocation="claims-types.xsd">
    <jaxb:bindings node="//xsd:complexType[@name='StatusType']">
      <jaxb:class name="ClaimStatusType"/>
    </jaxb:bindings>
  </jaxb:bindings>
</jaxb:bindings>
```

---

## 7. Understanding Classloader Issues in Monoliths

### WAR vs EAR Classloading Hierarchy

```
                Bootstrap ClassLoader (rt.jar, JDK classes)
                         │
                System ClassLoader (CLASSPATH, app server libs)
                         │
                Application Server ClassLoader (shared libs)
                         │
              ┌──────────┴──────────┐
              │                     │
         EAR ClassLoader       EAR ClassLoader
              │                     │
        ┌─────┴─────┐         WAR ClassLoader
        │            │
   WAR ClassLoader  EJB ClassLoader
   (WEB-INF/lib)    (EJB-JAR libs)
```

### Parent-First vs Child-First

- **Parent-first (default):** ClassLoader asks parent first. App server's version of a library
  wins over your WAR's version. This is Java's default delegation model.
- **Child-first:** ClassLoader checks its own JARs first. Your WAR's version wins.
  Required when the app server bundles an older version of a library you need.

**WebSphere:** Configured per application in admin console → Application → Class loading
→ "Classes loaded with local class loader first (parent last)"

**JBoss/WildFly:** Configured in `jboss-deployment-structure.xml`:
```xml
<jboss-deployment-structure>
  <deployment>
    <dependencies>
      <module name="org.hibernate" export="true"/>
    </dependencies>
    <exclusions>
      <module name="org.apache.commons.logging"/>
    </exclusions>
  </deployment>
</jboss-deployment-structure>
```

### ClassCastException Between "Same" Class

The most confusing classloader issue:

```
java.lang.ClassCastException:
  com.greylegacy.domain.Claim cannot be cast to com.greylegacy.domain.Claim
```

**Yes, same fully-qualified class name, but loaded by different classloaders.**
In Java, a class's identity is `(classname, classloader)` — two copies of the same
`.class` file loaded by different classloaders are **different types**.

**Common scenario:**
- `domain.jar` is in both `WEB-INF/lib` AND the app server's shared lib directory
- The service layer loads `Claim` from the shared copy
- The web layer loads `Claim` from `WEB-INF/lib`
- Passing a `Claim` object between them causes `ClassCastException`

### Diagnosing Classloader Issues

**Use `-verbose:class` JVM flag:**
```
[Loaded com.greylegacy.domain.Claim from file:/app/server/lib/domain.jar]
[Loaded com.greylegacy.domain.Claim from file:/app/webapp/WEB-INF/lib/domain.jar]
```

When you see the same class loaded from two locations — that's your problem.

**Programmatic inspection:**
```java
ClassLoader cl = Claim.class.getClassLoader();
log.info("Claim loaded by: " + cl);
log.info("ClassLoader hierarchy:");
while (cl != null) {
    log.info("  -> " + cl);
    cl = cl.getParent();
}
```

### JAR Hell: Conflicting Library Versions

```bash
# Find duplicate JARs in your deployment
find /app -name "*.jar" | xargs -I {} basename {} | sort | uniq -d

# Common conflicts in enterprise Java:
# commons-logging 1.1 vs 1.2
# log4j 1.x vs 2.x (bridge JARs needed)
# hibernate-core 4.x (app server) vs 5.x (your app)
# jackson-databind version mismatches
```

**Maven dependency:tree helps:**
```bash
mvn dependency:tree -Dincludes=commons-logging
```

---

## 8. Transaction Management Debugging

### Transaction Propagation Mismatches

```java
@Service
public class ClaimService {

    @Transactional(propagation = Propagation.REQUIRED)  // Uses existing TX
    public void processClaim(Claim claim) {
        claimDao.save(claim);

        try {
            auditService.logAudit(claim);  // What happens if this fails?
        } catch (Exception e) {
            log.error("Audit failed", e);
            // BUG: If auditService.logAudit uses REQUIRED, the entire
            // transaction is marked for rollback — even though we caught
            // the exception! The claim save will be rolled back too.
        }
    }
}

@Service
public class AuditService {

    @Transactional(propagation = Propagation.REQUIRES_NEW)  // Independent TX
    public void logAudit(Claim claim) {
        // With REQUIRES_NEW: failures here don't affect the caller's TX
        // With REQUIRED: failures here poison the caller's TX
        auditDao.save(new ClaimAuditEntry(...));
    }
}
```

**Key rules:**
- `REQUIRED` (default): joins the caller's transaction. If it fails, the **entire** transaction
  is marked rollback-only — catching the exception in the caller does NOT undo this.
- `REQUIRES_NEW`: suspends the caller's transaction and starts a new one. Failures here
  are isolated from the caller.

### Read-Only Transaction Violations

```
org.springframework.dao.InvalidDataAccessApiUsageException:
  Write operations are not allowed in read-only mode (FlushMode.MANUAL)
```

```java
@Transactional(readOnly = true)
public void updateClaimStatus(Long id, ClaimStatus newStatus) {
    // BUG: This is a read-only transaction — Hibernate won't flush changes
    Claim claim = claimDao.findById(id);
    claim.setStatus(newStatus);  // This change will be silently lost!
}
```

### Spring @Transactional Proxy Limitation (Self-Invocation)

**This is the #1 most misunderstood Spring behavior:**

```java
@Service
public class ClaimService {

    public void processBatch(List<Long> claimIds) {
        for (Long id : claimIds) {
            processOneClaim(id);  // BUG: @Transactional is IGNORED here!
        }
    }

    @Transactional
    public void processOneClaim(Long id) {
        // This method IS @Transactional, but when called from within
        // the same class, Spring's proxy is bypassed.
        // The call goes directly to the target object, not through
        // the AOP proxy that manages the transaction.
        Claim claim = claimDao.findById(id);
        claim.setStatus(ClaimStatus.APPROVED);
    }
}
```

**Why it happens:** Spring `@Transactional` works via AOP proxies. External callers go through
the proxy → transaction is started. Internal self-calls bypass the proxy → no transaction.

**Fix options:**
1. Separate into two classes (preferred)
2. Inject the service into itself (hacky): `@Autowired private ClaimService self;`
3. Use AspectJ compile-time weaving instead of proxies

### Two-Phase Commit Issues with JTA

When coordinating transactions across multiple resources (database + JMS):

```
javax.transaction.SystemException:
  The transaction has been rolled back because a resource adapter
  raised a heuristic mixed exception.
```

**Meaning:** Some resources committed, others rolled back. Data is now inconsistent.

**Debug:** Enable JTA transaction logging:
```xml
<!-- For Atomikos -->
<property name="com.atomikos.icatch.log_base_name" value="tx-logs"/>
<property name="com.atomikos.icatch.enable_logging" value="true"/>
```

### Debugging with Spring Transaction Logging

Add to `log4j.properties`:
```properties
# Show when transactions begin, commit, rollback
log4j.logger.org.springframework.transaction=DEBUG

# Show which method triggered the transaction
log4j.logger.org.springframework.transaction.interceptor=TRACE

# Show JDBC connection acquisition
log4j.logger.org.springframework.jdbc.datasource=DEBUG
```

**Sample output:**
```
DEBUG TransactionInterceptor - Getting transaction for
  [com.greylegacy.service.ClaimService.processClaim]
DEBUG HibernateTransactionManager - Creating new transaction
  with name [com.greylegacy.service.ClaimService.processClaim]:
  PROPAGATION_REQUIRED,ISOLATION_DEFAULT
DEBUG HibernateTransactionManager - Opened new Session for
  Hibernate transaction
DEBUG HibernateTransactionManager - Committing Hibernate transaction
  on Session [SessionImpl(1234567890)]
```

---

## 9. Thread Safety Issues in Servlet Containers

### Struts Actions Are Singletons!

**Critical gotcha:** In Struts 1.x, Action classes are **singletons**. The servlet container
creates ONE instance and shares it across ALL threads/requests.

```java
// BROKEN: Instance variable in a Struts Action
public class ClaimSearchAction extends Action {

    private String searchTerm;  // BUG! Shared across ALL requests!

    public ActionForward execute(ActionMapping mapping, ActionForm form,
                                 HttpServletRequest request,
                                 HttpServletResponse response) {
        ClaimSearchForm searchForm = (ClaimSearchForm) form;
        this.searchTerm = searchForm.getClaimNumber();  // Thread A writes "CLM-001"

        // ... some processing ...

        // Thread B may have overwritten searchTerm with "CLM-002" by now!
        List<Claim> results = claimService.search(this.searchTerm);  // Race condition!
        request.setAttribute("results", results);
        return mapping.findForward("results");
    }
}
```

**Fix:** Use only local variables in Action methods:
```java
public class ClaimSearchAction extends Action {
    // No instance variables! (except injected services which are thread-safe)

    public ActionForward execute(ActionMapping mapping, ActionForm form,
                                 HttpServletRequest request,
                                 HttpServletResponse response) {
        ClaimSearchForm searchForm = (ClaimSearchForm) form;
        String searchTerm = searchForm.getClaimNumber();  // Local variable — thread-safe
        List<Claim> results = claimService.search(searchTerm);
        request.setAttribute("results", results);
        return mapping.findForward("results");
    }
}
```

### HttpSession Concurrency

```java
// BROKEN: Not thread-safe — two AJAX requests can race
HttpSession session = request.getSession();
Integer count = (Integer) session.getAttribute("viewCount");
count = (count == null) ? 1 : count + 1;  // read-modify-write is NOT atomic
session.setAttribute("viewCount", count);
```

**Fix:** Synchronize on the session (but beware performance implications):
```java
synchronized (session) {
    Integer count = (Integer) session.getAttribute("viewCount");
    count = (count == null) ? 1 : count + 1;
    session.setAttribute("viewCount", count);
}
```

### SimpleDateFormat in Multi-Threaded Context

```java
// BROKEN: SimpleDateFormat is NOT thread-safe!
public class ClaimReportService {
    // Shared across all threads
    private static final SimpleDateFormat DATE_FORMAT =
        new SimpleDateFormat("yyyy-MM-dd");

    public String formatLossDate(Claim claim) {
        return DATE_FORMAT.format(claim.getLossDate());  // Race condition!
        // Can produce garbled dates, ArrayIndexOutOfBoundsException,
        // or NumberFormatException
    }
}
```

**Fix options:**
```java
// Option 1: Create new instance each time (simple but allocates)
public String formatLossDate(Claim claim) {
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
    return sdf.format(claim.getLossDate());
}

// Option 2: ThreadLocal (reuses per thread)
private static final ThreadLocal<SimpleDateFormat> DATE_FORMAT =
    ThreadLocal.withInitial(() -> new SimpleDateFormat("yyyy-MM-dd"));

public String formatLossDate(Claim claim) {
    return DATE_FORMAT.get().format(claim.getLossDate());
}

// Option 3: Java 8+ DateTimeFormatter (immutable and thread-safe)
private static final DateTimeFormatter DATE_FORMAT =
    DateTimeFormatter.ofPattern("yyyy-MM-dd");
```

### ThreadLocal Memory Leaks

```java
// DANGEROUS in servlet containers!
private static final ThreadLocal<UserContext> userContext = new ThreadLocal<>();

public void setUser(UserContext ctx) {
    userContext.set(ctx);  // Stored on the thread
}

// If you never call userContext.remove(), the value stays on the thread.
// Servlet container thread pools REUSE threads — meaning:
// 1. Request A sets UserContext for "admin"
// 2. Thread goes back to pool
// 3. Request B (from different user) gets the same thread
// 4. Request B sees "admin" UserContext — SECURITY BUG + MEMORY LEAK
```

**Fix:** Always clean up in a servlet filter:
```java
public class ThreadLocalCleanupFilter implements Filter {
    public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain)
            throws IOException, ServletException {
        try {
            chain.doFilter(req, res);
        } finally {
            UserContext.clear();  // Always clean up!
        }
    }
}
```

### Servlet Container Thread Pool Exhaustion

**Symptom:** All requests hang, new connections refused.

**Causes:**
- All threads waiting on external service (SOAP call with no timeout)
- All threads waiting for database connections (see Section 4)
- Deadlock between threads

**Diagnosis:** Take a thread dump:
```bash
# Option 1: jstack
jstack <pid> > thread_dump.txt

# Option 2: kill -3 (Unix) — dumps to stdout/stderr
kill -3 <pid>

# Option 3: JMX / JConsole → Threads tab → "Detect Deadlock"
```

**Look for:** Many threads in `WAITING` or `BLOCKED` state on the same monitor.

---

## 10. Legacy Environment Diagnosis

### Checking JVM Version and Flags

```bash
# JVM version
java -version

# Active JVM flags for a running process
jinfo -flags <pid>

# All system properties
jinfo -sysprops <pid>

# Or from within the application
System.getProperty("java.version");
System.getProperty("java.vendor");
Runtime.getRuntime().maxMemory();   // Max heap in bytes
Runtime.getRuntime().totalMemory(); // Currently allocated heap
Runtime.getRuntime().freeMemory();  // Free within allocated heap
```

### Heap Dump Analysis Basics

```bash
# Generate heap dump
jmap -dump:format=b,file=heap_dump.hprof <pid>

# Or trigger on OutOfMemoryError (add to JVM args)
-XX:+HeapDumpOnOutOfMemoryError
-XX:HeapDumpPath=/app/dumps/
```

**Analyze with Eclipse MAT (Memory Analyzer Tool):**
1. Open the `.hprof` file
2. Check "Leak Suspects" report
3. Look at "Dominator Tree" for largest object graphs
4. Common culprits in Grey Legacy: large result sets cached in HttpSession,
   Hibernate first-level cache holding too many entities

### Thread Dump Reading Basics

```
"http-nio-8080-exec-5" #42 daemon prio=5 os_prio=0
   java.lang.Thread.State: WAITING (parking)
    at sun.misc.Unsafe.park(Native Method)
    - parking to wait for <0x00000006c7e00e88>
      (a java.util.concurrent.locks.AbstractQueuedSynchronizer$ConditionObject)
    at com.mchange.v2.resourcepool.BasicResourcePool.awaitAvailable(BasicResourcePool.java:1467)
    at com.mchange.v2.resourcepool.BasicResourcePool.checkoutResource(BasicResourcePool.java:594)
    at com.mchange.v2.c3p0.impl.C3P0PooledConnectionPool.checkoutAndMarkConnectionInUse
    ...
    at com.greylegacy.dao.ClaimDao.findOpenClaims(ClaimDao.java:64)
    at com.greylegacy.service.ClaimService.getOpenClaimsDashboard(ClaimService.java:88)
```

**Reading this:**
1. Thread name `http-nio-8080-exec-5` → Tomcat HTTP request handler thread
2. State `WAITING` → blocked, not consuming CPU
3. Parked at `BasicResourcePool.awaitAvailable` → waiting for a database connection
4. Called from `ClaimDao.findOpenClaims` → business code location
5. **Diagnosis:** Connection pool exhaustion (see Section 4)

### GC Log Interpretation

**Enable GC logging:**
```bash
# Java 8
-XX:+PrintGCDetails -XX:+PrintGCTimeStamps -Xloggc:/app/logs/gc.log

# Java 9+
-Xlog:gc*:file=/app/logs/gc.log:time,uptime,level,tags
```

**Key patterns to watch:**
```
# Normal minor GC (fast, expected)
[GC (Allocation Failure) [PSYoungGen: 262144K->32768K(305664K)]
  524288K->295012K(1005568K), 0.0523421 secs]

# Full GC (slow, concerning if frequent)
[Full GC (Ergonomics) [PSYoungGen: 32768K->0K(305664K)]
  [ParOldGen: 680000K->412345K(699904K)] 712768K->412345K(1005568K),
  [Metaspace: 98304K->98304K(1130496K)], 2.3456789 secs]
```

**Red flags:**
- Full GC happening every few seconds → heap too small or memory leak
- Heap not shrinking after Full GC → memory leak (objects can't be collected)
- GC pause times > 5 seconds → stop-the-world pauses affecting users

### JMX/JConsole Connection Setup

```bash
# Add to JVM startup args
-Dcom.sun.management.jmxremote
-Dcom.sun.management.jmxremote.port=9999
-Dcom.sun.management.jmxremote.authenticate=false
-Dcom.sun.management.jmxremote.ssl=false
-Djava.rmi.server.hostname=<server-ip>

# Connect JConsole
jconsole <server-ip>:9999
```

**Key MBeans to monitor:**
- `java.lang:type=Memory` → Heap/non-heap usage
- `java.lang:type=Threading` → Thread count, deadlock detection
- `java.lang:type=GarbageCollector` → GC counts and times
- `com.mchange.v2.c3p0` → Connection pool stats
- `org.hibernate:type=Statistics` → Hibernate query/cache stats

### Common JVM Tuning Parameters for Enterprise Apps

```bash
# Heap sizing
-Xms2g            # Initial heap (set equal to Xmx to avoid resize pauses)
-Xmx4g            # Maximum heap
-XX:NewRatio=2    # Old:Young ratio (2 = Old is 2x Young)

# GC selection (Java 8)
-XX:+UseG1GC                    # G1 Garbage Collector (recommended)
-XX:MaxGCPauseMillis=200        # Target max GC pause time
-XX:G1HeapRegionSize=16m        # G1 region size

# Metaspace (Java 8+, replaces PermGen)
-XX:MetaspaceSize=256m
-XX:MaxMetaspaceSize=512m

# Thread stack size (reduce if creating many threads)
-Xss512k

# Diagnostics
-XX:+HeapDumpOnOutOfMemoryError
-XX:HeapDumpPath=/app/dumps/
-XX:+PrintGCDetails
-XX:+PrintGCTimeStamps

# Enterprise app typical config
JAVA_OPTS="-server -Xms4g -Xmx4g -XX:+UseG1GC \
  -XX:MaxGCPauseMillis=200 \
  -XX:MetaspaceSize=256m -XX:MaxMetaspaceSize=512m \
  -XX:+HeapDumpOnOutOfMemoryError \
  -XX:HeapDumpPath=/app/dumps/ \
  -Dfile.encoding=UTF-8 \
  -Djava.net.preferIPv4Stack=true \
  -Dcom.sun.management.jmxremote \
  -Dcom.sun.management.jmxremote.port=9999 \
  -Dcom.sun.management.jmxremote.authenticate=false \
  -Dcom.sun.management.jmxremote.ssl=false"
```

---

## Quick Reference: Error → Section Mapping

| Error Message Pattern | Section |
|---|---|
| `BeanCreationException` | [1](#1-reading-stack-traces-from-xml-configured-systems) |
| `NoSuchBeanDefinitionException` | [1](#1-reading-stack-traces-from-xml-configured-systems) |
| `ClassNotFoundException` | [1](#1-reading-stack-traces-from-xml-configured-systems) / [7](#7-understanding-classloader-issues-in-monoliths) |
| `LazyInitializationException` | [2](#2-debugging-hibernate-session-issues) |
| `NonUniqueObjectException` | [2](#2-debugging-hibernate-session-issues) |
| `StaleObjectStateException` | [2](#2-debugging-hibernate-session-issues) |
| Hundreds of identical SELECT statements in log | [3](#3-diagnosing-n1-queries) |
| `Cannot acquire a new connection` | [4](#4-jdbc-connection-pool-exhaustion) |
| `CannotAcquireResourceException` | [4](#4-jdbc-connection-pool-exhaustion) |
| SOAP `<faultstring>` | [5](#5-troubleshooting-soap-faults) |
| `SAXParseException` in WSDL/XSD | [6](#6-interpreting-wsdlxsd-errors) |
| `ClassCastException` same class name | [7](#7-understanding-classloader-issues-in-monoliths) |
| `UnexpectedRollbackException` | [8](#8-transaction-management-debugging) |
| `InvalidDataAccessApiUsageException` read-only | [8](#8-transaction-management-debugging) |
| Race conditions / garbled data | [9](#9-thread-safety-issues-in-servlet-containers) |
| `OutOfMemoryError` | [10](#10-legacy-environment-diagnosis) |
| Application hangs / timeouts | [4](#4-jdbc-connection-pool-exhaustion) / [9](#9-thread-safety-issues-in-servlet-containers) / [10](#10-legacy-environment-diagnosis) |

---

*Last updated: 2025. Maintained by the Grey Legacy platform engineering team.*

---

## 11. JMS / ActiveMQ Debugging

### Messages Not Being Consumed

**Symptom:** Messages pile up in the queue, consumer count shows 0.

**Diagnosis checklist:**
1. **Is the listener container started?**
   ```xml
   <!-- In applicationContext-jms.xml, check autoStartup -->
   <jms:listener-container connection-factory="connectionFactory"
                           acknowledge="transacted"
                           concurrency="3-10"
                           auto-startup="true">
   ```

2. **Is the connection factory URL correct?**
   ```properties
   # Check if broker URL matches the running ActiveMQ instance
   jms.broker.url=failover:(tcp://localhost:61616)?timeout=3000
   ```

3. **Check ActiveMQ admin console:** `http://localhost:8161/admin/queues.jsp`
   - Number of Pending Messages (enqueued but not consumed)
   - Number of Consumers (should be > 0)
   - Messages Dequeued (should be increasing)

4. **Thread dump:** Look for threads named `DefaultMessageListenerContainer-*`:
   ```
   "DefaultMessageListenerContainer-1" #87 prio=5 java.lang.Thread.State: TIMED_WAITING
       at org.apache.activemq.transport.failover.FailoverTransport.oneway(...)
   ```
   If stuck at `FailoverTransport.oneway`, the broker is unreachable.

### Poison Messages / DLQ

**Symptom:** Message keeps redelivering, eventually lands in DLQ.

```
WARN  [ClaimEventListener] Redelivery attempt 4/5 for message ID:
  broker01-12345-1709312345123-1:1:1:1:1
ERROR [ClaimEventListener] Message exceeded max redelivery attempts,
  routing to DLQ: DLQ.GreyLegacyClaimEvents
```

**Investigation:**
```bash
# Check DLQ via ActiveMQ CLI
activemq browse DLQ.GreyLegacyClaimEvents

# Or via JMX
jconsole → org.apache.activemq → Broker → Queue → DLQ.GreyLegacyClaimEvents
  → getAttribute("QueueSize")
```

**Common causes:**
- Message body cannot be deserialized (class version mismatch after deployment)
- Business logic throws unchecked exception on every attempt
- Database constraint violation during message processing

**Fix:** Use the `DeadLetterQueueProcessor` to inspect and replay:
```java
// Replay a DLQ message (see DeadLetterQueueProcessor.processDeadLetters())
// 1. Read message from DLQ
// 2. Log the failure reason
// 3. Fix the root cause
// 4. Re-send to original queue
```

### JMS Transaction Integration

**Symptom:** Message consumed but database changes rolled back (or vice versa).

This happens when JMS and database transactions are not coordinated:

```java
// BROKEN: Two separate transaction managers
@Transactional  // Uses HibernateTransactionManager (DB only)
public void onMessage(Message message) {
    Claim claim = parseClaim(message);
    claimDao.save(claim);  // DB transaction
    // If this throws AFTER message was acknowledged, data is saved
    // but message is lost. If DB fails, message is redelivered
    // but may be processed again (duplicate).
}
```

**Fix options:**
1. **JTA/XA transactions** (two-phase commit): Use `JtaTransactionManager` with XA DataSource and XA ConnectionFactory
2. **Idempotent consumer** (Grey Legacy's approach): Accept at-least-once delivery, deduplicate via `IdempotentMessageConsumer` with message ID cache
3. **Outbox pattern**: Write to a database outbox table within the DB transaction, then asynchronously publish to JMS

---

## 12. Batch Job Troubleshooting

### Spring Batch Job Failures

**Symptom:** `JobExecution` has status `FAILED`.

```
ERROR [BatchJobCompletionListener] Job claimExportJob FAILED
  ExitStatus: exitCode=FAILED; exitDescription=
  org.springframework.batch.item.ItemStreamException:
    Failed to initialize the reader
```

**Diagnosis:**
```sql
-- Check job execution history
SELECT JOB_INSTANCE_ID, JOB_NAME, STATUS, EXIT_CODE, EXIT_MESSAGE,
       START_TIME, END_TIME
FROM BATCH_JOB_EXECUTION
WHERE JOB_NAME = 'claimExportJob'
ORDER BY START_TIME DESC
LIMIT 10;

-- Check step execution details
SELECT STEP_NAME, STATUS, READ_COUNT, WRITE_COUNT, SKIP_COUNT,
       ROLLBACK_COUNT, EXIT_MESSAGE
FROM BATCH_STEP_EXECUTION
WHERE JOB_EXECUTION_ID = <id>
ORDER BY START_TIME;
```

### Restarting a Failed Job

Spring Batch tracks execution state in JDBC `JobRepository`. A failed job can be restarted from the last committed chunk:

```java
// Via JobOperator (exposed via JMX or admin endpoint)
Long executionId = jobOperator.restart(failedExecutionId);

// The reader's ItemStream.open() is called with the last ExecutionContext,
// which contains the checkpoint (e.g., last processed row number).
// ClaimExportItemReader.open() reads the "claim.export.current.offset" key.
```

**Common restart issues:**
- `JobInstanceAlreadyCompleteException` — the job already completed; use different parameters
- `JobRestartException` — the last execution is still `STARTED` or `STOPPING`; abandon it first:
  ```java
  jobOperator.abandon(stuckExecutionId);
  jobOperator.restart(stuckExecutionId);
  ```

### Quartz Scheduler Issues

**Symptom:** Jobs not firing at expected schedule.

```
WARN  [QuartzScheduler] All triggers of Job ClaimAgingJob are set to PAUSED state
```

**Check Quartz tables:**
```sql
-- See all triggers and their states
SELECT TRIGGER_NAME, TRIGGER_STATE, NEXT_FIRE_TIME, PREV_FIRE_TIME,
       MISFIRE_INSTR
FROM QRTZ_TRIGGERS;

-- MISFIRE_INSTR values:
-- 0 = smart policy (default)
-- 1 = fire now
-- 2 = do nothing (skip missed)
```

**Common causes:**
- Misfire instruction set to DO_NOTHING and the application was down during the scheduled time
- Quartz clustered mode: another node already fired the trigger
- System clock skew between cluster nodes
- Thread pool exhaustion: `org.quartz.threadPool.threadCount` too low

### Batch Performance Tuning

```xml
<!-- Increase chunk size for better throughput -->
<batch:chunk reader="claimReader" processor="claimProcessor"
             writer="claimWriter" commit-interval="500"/>
<!-- Default is 100; increase for large datasets, decrease for
     finer-grained checkpointing -->

<!-- Partitioned step for parallel processing -->
<batch:step id="partitionedExportStep">
    <batch:partition partitioner="claimExportPartitioner">
        <batch:handler grid-size="4"
                       task-executor="batchTaskExecutor"/>
    </batch:partition>
</batch:step>
```

---

## 13. Deployment Debugging

### WAR Deployment Failures on Tomcat

**Symptom:** Application doesn't start after WAR deployment. `catalina.out` shows:

```
SEVERE [main] org.apache.catalina.core.StandardContext.startInternal
  One or more listeners failed to start. Full details will be found
  in the appropriate container log file
```

**Diagnosis steps:**
1. Check `$CATALINA_HOME/logs/localhost.<date>.log` (not catalina.out) for the real error
2. Common root causes:
   - Missing JARs in `WEB-INF/lib` (ClassNotFoundException)
   - Spring context failure (BeanCreationException chain — see Section 1)
   - Database unreachable during context startup
   - JNDI resource not defined in server.xml/context.xml

### EAR Deployment Failures on JBoss

**Symptom:** `grey-legacy.ear.failed` marker file appears in deployments directory.

```bash
# Read deployment error
cat $JBOSS_HOME/standalone/deployments/grey-legacy.ear.failed

# Or check server log
tail -200 $JBOSS_HOME/standalone/log/server.log | grep -A5 "WFLYCTL"
```

**Common JBoss deployment errors:**

```
WFLYCTL0412: Required services that are not installed:
    [jboss.naming.context.java.jdbc/GreyLegacyDS]
```
→ DataSource not configured in standalone.xml. Add the `<datasource>` element.

```
WFLYSRV0153: Failed to process phase PARSE of deployment "grey-legacy.ear"
    java.lang.ClassNotFoundException: org.hibernate.ejb.HibernatePersistence
```
→ JBoss has its own Hibernate module. Either exclude it (jboss-deployment-structure.xml)
or align your Hibernate version with the server's bundled version.

### Rollback Procedures

```bash
# Tomcat rollback (uses deploy.sh backup)
cd /opt/backups
ls -la grey-legacy-*.war  # Find the previous good WAR
cp grey-legacy-20250315-143000.war /opt/tomcat/webapps/grey-legacy.war
sudo systemctl restart tomcat

# JBoss rollback
cp /opt/backups/grey-legacy-20250315-143000.ear \
   $JBOSS_HOME/standalone/deployments/grey-legacy.ear
touch $JBOSS_HOME/standalone/deployments/grey-legacy.ear.dodeploy
```

### Environment-Specific Configuration Mismatches

The most common production deployment issue: configuration that works in dev but fails in prod.

**Checklist:**
| Setting | Dev | Prod | Common Mistake |
|---------|-----|------|----------------|
| DB URL | `jdbc:h2:mem:test` | `jdbc:postgresql://db-prod:5432/greylegacy` | Wrong URL in prod properties |
| DB credentials | `sa` / (empty) | Vault-managed | Credentials not provisioned |
| JMS broker | `tcp://localhost:61616` | `failover:(tcp://mq-1:61616,tcp://mq-2:61616)` | Missing failover URI |
| JNDI prefix | `java:comp/env/` (Tomcat) | `java:jboss/` (JBoss) | Wrong prefix for container |
| File paths | `C:\temp\export` | `/opt/greylegacy/export` | Windows paths in Unix deploy |
| Log level | `DEBUG` | `WARN` | DEBUG flooding prod logs |

---

## 14. Container-Specific Debugging

### Tomcat Thread Dump

```bash
# Via PID
jstack $(pgrep -f catalina) > /tmp/thread-dump.txt

# Via JMX (if enabled in server.xml)
jconsole localhost:9010
# → Threads tab → "Detect Deadlock"

# Via Tomcat manager (if deployed)
curl -u admin:password http://localhost:8080/manager/text/threaddump
```

### JBoss/WildFly CLI Diagnostics

```bash
# Connect to JBoss CLI
$JBOSS_HOME/bin/jboss-cli.sh --connect

# Check deployment status
/deployment=grey-legacy.ear:read-attribute(name=status)

# Check datasource pool stats
/subsystem=datasources/data-source=GreyLegacyDS/statistics=pool:read-resource(include-runtime=true)

# Check JMS queue depth
/subsystem=messaging-activemq/server=default/jms-queue=GreyLegacyClaimEvents:read-attribute(name=message-count)

# Force garbage collection
:gc
```

### Heap/Thread Dump on OutOfMemoryError

Add to JVM args (in start-app.cmd, Tomcat CATALINA_OPTS, or JBoss JAVA_OPTS):

```bash
-XX:+HeapDumpOnOutOfMemoryError
-XX:HeapDumpPath=/opt/dumps/grey-legacy-heap.hprof
-XX:OnOutOfMemoryError="kill -9 %p"
```

The `-XX:OnOutOfMemoryError` flag kills the JVM after dumping, ensuring a clean restart by the process manager (systemd, supervisord). Running on a half-dead JVM after OOM is worse than a clean restart.

---

## Quick Reference: Error → Section Mapping (Updated)

| Error Message Pattern | Section |
|---|---|
| `BeanCreationException` | [1](#1-reading-stack-traces-from-xml-configured-systems) |
| `NoSuchBeanDefinitionException` | [1](#1-reading-stack-traces-from-xml-configured-systems) |
| `ClassNotFoundException` | [1](#1-reading-stack-traces-from-xml-configured-systems) / [7](#7-understanding-classloader-issues-in-monoliths) |
| `LazyInitializationException` | [2](#2-debugging-hibernate-session-issues) |
| `NonUniqueObjectException` | [2](#2-debugging-hibernate-session-issues) |
| `StaleObjectStateException` | [2](#2-debugging-hibernate-session-issues) |
| Hundreds of identical SELECT statements in log | [3](#3-diagnosing-n1-queries) |
| `Cannot acquire a new connection` | [4](#4-jdbc-connection-pool-exhaustion) |
| `CannotAcquireResourceException` | [4](#4-jdbc-connection-pool-exhaustion) |
| SOAP `<faultstring>` | [5](#5-troubleshooting-soap-faults) |
| `SAXParseException` in WSDL/XSD | [6](#6-interpreting-wsdlxsd-errors) |
| `ClassCastException` same class name | [7](#7-understanding-classloader-issues-in-monoliths) |
| `UnexpectedRollbackException` | [8](#8-transaction-management-debugging) |
| `InvalidDataAccessApiUsageException` read-only | [8](#8-transaction-management-debugging) |
| Race conditions / garbled data | [9](#9-thread-safety-issues-in-servlet-containers) |
| `OutOfMemoryError` | [10](#10-legacy-environment-diagnosis) |
| Application hangs / timeouts | [4](#4-jdbc-connection-pool-exhaustion) / [9](#9-thread-safety-issues-in-servlet-containers) / [10](#10-legacy-environment-diagnosis) |
| Messages piling up in JMS queue | [11](#11-jms--activemq-debugging) |
| DLQ / poison messages | [11](#11-jms--activemq-debugging) |
| `JobInstanceAlreadyCompleteException` | [12](#12-batch-job-troubleshooting) |
| `JobRestartException` | [12](#12-batch-job-troubleshooting) |
| Quartz triggers in PAUSED state | [12](#12-batch-job-troubleshooting) |
| WAR deployment fails / `.failed` marker | [13](#13-deployment-debugging) |
| `WFLYCTL0412` missing services | [13](#13-deployment-debugging) |
| Container thread dump / heap dump | [14](#14-container-specific-debugging) |

---

*Last updated: 2026. Maintained by the Grey Legacy platform engineering team.*
