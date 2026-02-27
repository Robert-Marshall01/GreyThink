package com.greylegacy.web.filter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.UUID;

/**
 * Servlet filter that logs all incoming requests and adds correlation IDs.
 * Demonstrates legacy servlet filter patterns used in enterprise Java.
 * 
 * Configured in web.xml, this filter:
 * - Generates a unique correlation ID per request
 * - Adds it to MDC for log correlation
 * - Logs request/response metadata
 * - Measures request duration
 */
public class AuditLoggingFilter implements Filter {

    private static final Logger log = LoggerFactory.getLogger(AuditLoggingFilter.class);
    private static final String CORRELATION_ID_HEADER = "X-Correlation-ID";
    private static final String MDC_CORRELATION_ID = "correlationId";
    private static final String MDC_USER = "user";
    private static final String MDC_REMOTE_ADDR = "remoteAddr";

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
        log.info("AuditLoggingFilter initialized");
    }

    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse,
                          FilterChain chain) throws IOException, ServletException {

        HttpServletRequest request = (HttpServletRequest) servletRequest;
        HttpServletResponse response = (HttpServletResponse) servletResponse;

        // Generate or extract correlation ID
        String correlationId = request.getHeader(CORRELATION_ID_HEADER);
        if (correlationId == null || correlationId.trim().isEmpty()) {
            correlationId = UUID.randomUUID().toString();
        }

        // Set MDC context for logging
        MDC.put(MDC_CORRELATION_ID, correlationId);
        MDC.put(MDC_USER, request.getRemoteUser() != null ? request.getRemoteUser() : "anonymous");
        MDC.put(MDC_REMOTE_ADDR, request.getRemoteAddr());

        // Add correlation ID to response
        response.setHeader(CORRELATION_ID_HEADER, correlationId);

        long startTime = System.currentTimeMillis();

        log.info("REQUEST  {} {} from {} [{}]",
                request.getMethod(),
                request.getRequestURI(),
                request.getRemoteAddr(),
                correlationId);

        try {
            chain.doFilter(servletRequest, servletResponse);
        } finally {
            long duration = System.currentTimeMillis() - startTime;

            log.info("RESPONSE {} {} status={} duration={}ms [{}]",
                    request.getMethod(),
                    request.getRequestURI(),
                    response.getStatus(),
                    duration,
                    correlationId);

            // Clear MDC
            MDC.remove(MDC_CORRELATION_ID);
            MDC.remove(MDC_USER);
            MDC.remove(MDC_REMOTE_ADDR);
        }
    }

    @Override
    public void destroy() {
        log.info("AuditLoggingFilter destroyed");
    }
}
