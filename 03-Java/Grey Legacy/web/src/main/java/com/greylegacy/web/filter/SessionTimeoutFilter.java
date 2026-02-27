package com.greylegacy.web.filter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;

/**
 * Filter that checks for session timeout and redirects to login.
 * Excludes static resources and the login page itself.
 */
public class SessionTimeoutFilter implements Filter {

    private static final Logger log = LoggerFactory.getLogger(SessionTimeoutFilter.class);
    private String loginPage = "/login.jsp";
    private String[] excludedPaths = {"/login", "/css/", "/js/", "/images/", "/health"};

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
        String configLoginPage = filterConfig.getInitParameter("loginPage");
        if (configLoginPage != null) {
            this.loginPage = configLoginPage;
        }
    }

    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse,
                          FilterChain chain) throws IOException, ServletException {

        HttpServletRequest request = (HttpServletRequest) servletRequest;
        HttpServletResponse response = (HttpServletResponse) servletResponse;

        String requestURI = request.getRequestURI();

        // Skip filter for excluded paths
        for (String excluded : excludedPaths) {
            if (requestURI.contains(excluded)) {
                chain.doFilter(servletRequest, servletResponse);
                return;
            }
        }

        HttpSession session = request.getSession(false);
        if (session == null || session.getAttribute("authenticatedUser") == null) {
            log.warn("Session expired or not authenticated. Redirecting to login. URI: {}", requestURI);
            response.sendRedirect(request.getContextPath() + loginPage);
            return;
        }

        chain.doFilter(servletRequest, servletResponse);
    }

    @Override
    public void destroy() {
    }
}
