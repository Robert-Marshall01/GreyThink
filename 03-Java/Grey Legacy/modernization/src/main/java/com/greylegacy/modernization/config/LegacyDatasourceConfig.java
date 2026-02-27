package com.greylegacy.modernization.config;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.orm.hibernate5.HibernateTransactionManager;
import org.springframework.orm.hibernate5.LocalSessionFactoryBean;

import javax.sql.DataSource;
import java.beans.PropertyVetoException;
import java.util.Properties;

/**
 * Java-based configuration replacing legacy XML datasource and Hibernate setup.
 *
 * <p>Demonstrates the migration from applicationContext-dao.xml to annotation-based
 * Spring configuration. The @Profile annotations replace Maven profiles for
 * environment-specific settings.</p>
 *
 * <h3>Migration from XML:</h3>
 * <pre>
 * BEFORE (applicationContext-dao.xml):
 *   &lt;bean id="dataSource" class="com.mchange.v2.c3p0.ComboPooledDataSource"&gt;
 *     &lt;property name="driverClass" value="${db.driver}"/&gt;
 *     ...
 *   &lt;/bean&gt;
 *
 * AFTER (this class):
 *   @Bean
 *   public DataSource dataSource() { ... }
 * </pre>
 */
@Configuration
public class LegacyDatasourceConfig {

    @Value("${spring.datasource.url:jdbc:h2:mem:greylegacy}")
    private String jdbcUrl;

    @Value("${spring.datasource.username:sa}")
    private String username;

    @Value("${spring.datasource.password:}")
    private String password;

    @Value("${spring.datasource.driver-class-name:org.h2.Driver}")
    private String driverClass;

    /**
     * C3P0 connection pool matching the legacy configuration.
     * In production, this would be replaced by HikariCP (Spring Boot default).
     */
    @Bean(destroyMethod = "close")
    @Profile("legacy")
    public DataSource legacyDataSource() throws PropertyVetoException {
        ComboPooledDataSource dataSource = new ComboPooledDataSource();
        dataSource.setDriverClass(driverClass);
        dataSource.setJdbcUrl(jdbcUrl);
        dataSource.setUser(username);
        dataSource.setPassword(password);

        // Legacy C3P0 pool settings (matching production config)
        dataSource.setMinPoolSize(5);
        dataSource.setMaxPoolSize(20);
        dataSource.setMaxIdleTime(300);
        dataSource.setAcquireIncrement(5);
        dataSource.setMaxStatements(100);
        dataSource.setTestConnectionOnCheckout(true);

        return dataSource;
    }

    /**
     * Hibernate SessionFactory bean — required for legacy DAO classes
     * that extend AbstractHibernateDao and use SessionFactory directly.
     *
     * Modern code should use JPA EntityManager instead, but this bean
     * enables backward compatibility during migration.
     */
    @Bean
    @Profile("legacy")
    public LocalSessionFactoryBean sessionFactory(DataSource dataSource) {
        LocalSessionFactoryBean sessionFactory = new LocalSessionFactoryBean();
        sessionFactory.setDataSource(dataSource);
        sessionFactory.setPackagesToScan("com.greylegacy.domain");
        sessionFactory.setMappingResources(
            "hibernate-mappings/Policy.hbm.xml",
            "hibernate-mappings/Claim.hbm.xml",
            "hibernate-mappings/ClaimPayment.hbm.xml"
        );

        Properties hibernateProperties = new Properties();
        hibernateProperties.setProperty("hibernate.dialect", "org.hibernate.dialect.H2Dialect");
        hibernateProperties.setProperty("hibernate.show_sql", "false");
        hibernateProperties.setProperty("hibernate.hbm2ddl.auto", "none"); // Flyway manages schema
        hibernateProperties.setProperty("hibernate.jdbc.batch_size", "25");
        sessionFactory.setHibernateProperties(hibernateProperties);

        return sessionFactory;
    }

    @Bean
    @Profile("legacy")
    public HibernateTransactionManager transactionManager(
            LocalSessionFactoryBean sessionFactory) {
        HibernateTransactionManager txManager = new HibernateTransactionManager();
        txManager.setSessionFactory(sessionFactory.getObject());
        return txManager;
    }
}
