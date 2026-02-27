package com.greylegacy.modernization;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ImportResource;

/**
 * Spring Boot application entry point for the modernized Grey Legacy system.
 *
 * <p>This class demonstrates the migration path from a legacy XML-configured
 * Spring application to a modern annotation-driven Spring Boot application.</p>
 *
 * <h3>Migration Strategy (Strangler Fig Pattern):</h3>
 * <ol>
 *   <li>Import existing XML configurations via @ImportResource</li>
 *   <li>Gradually replace XML beans with @Component/@Service/@Repository</li>
 *   <li>Replace JSP views with REST endpoints</li>
 *   <li>Replace SOAP services with REST APIs</li>
 *   <li>Replace hbm2ddl.auto with Flyway migrations</li>
 *   <li>Replace JMX MBeans with Micrometer metrics + Actuator</li>
 * </ol>
 *
 * <p>The @ImportResource annotation allows the Boot app to reuse existing
 * Spring XML bean definitions during the migration, avoiding a big-bang rewrite.</p>
 */
@SpringBootApplication(scanBasePackages = {
    "com.greylegacy.modernization",
    "com.greylegacy.service",
    "com.greylegacy.dao"
})
@ImportResource({
    "classpath:applicationContext-dao.xml",
    "classpath:applicationContext-service.xml"
})
public class GreyLegacyModernApplication {

    public static void main(String[] args) {
        SpringApplication.run(GreyLegacyModernApplication.class, args);
    }
}
