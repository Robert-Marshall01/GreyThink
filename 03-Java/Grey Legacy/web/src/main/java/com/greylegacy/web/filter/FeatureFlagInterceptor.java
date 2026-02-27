package com.greylegacy.web.filter;

import java.util.LinkedHashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.greylegacy.service.feature.FeatureFlags;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;

/**
 * Spring {@link HandlerInterceptor} that gates HTTP endpoints behind
 * feature flags.
 * <p>
 * If a request URI matches a configured URL pattern whose backing feature flag
 * is disabled, the interceptor short-circuits the request with a {@code 404
 * Not Found} – making the endpoint appear as though it does not exist.
 * </p>
 *
 * @since 2.0.0
 */
public class FeatureFlagInterceptor implements HandlerInterceptor {

    private static final Logger LOG = LoggerFactory.getLogger(FeatureFlagInterceptor.class);

    /**
     * Maps URL path prefixes to the feature-flag property name that must be
     * enabled for the endpoint to be reachable.
     */
    private Map<String, String> urlPatternToFlag = new LinkedHashMap<String, String>();

    /**
     * Configures the URL-prefix → feature-flag mappings.
     * Typically injected via Spring XML:
     * <pre>{@code
     * <property name="urlPatternToFlag">
     *   <map>
     *     <entry key="/api/"   value="feature.rest.api.enabled"/>
     *     <entry key="/ws/"    value="feature.soap.endpoints.enabled"/>
     *   </map>
     * </property>
     * }</pre>
     */
    public void setUrlPatternToFlag(Map<String, String> urlPatternToFlag) {
        this.urlPatternToFlag = urlPatternToFlag;
    }

    @Override
    public boolean preHandle(HttpServletRequest request,
                             HttpServletResponse response,
                             Object handler) throws Exception {

        String uri = request.getRequestURI();
        String contextPath = request.getContextPath();
        String path = uri.substring(contextPath.length());

        for (Map.Entry<String, String> entry : urlPatternToFlag.entrySet()) {
            String pattern = entry.getKey();
            String flagName = entry.getValue();

            if (path.startsWith(pattern)) {
                boolean enabled = FeatureFlags.getInstance().isEnabled(flagName);
                if (!enabled) {
                    LOG.warn("Blocked request to {} — feature flag '{}' is disabled", path, flagName);
                    response.sendError(HttpServletResponse.SC_NOT_FOUND);
                    return false;
                }
                LOG.debug("Feature flag '{}' is enabled — allowing {}", flagName, path);
            }
        }

        return true;
    }

    @Override
    public void postHandle(HttpServletRequest request,
                           HttpServletResponse response,
                           Object handler,
                           ModelAndView modelAndView) throws Exception {
        // no-op
    }

    @Override
    public void afterCompletion(HttpServletRequest request,
                                HttpServletResponse response,
                                Object handler,
                                Exception ex) throws Exception {
        // no-op
    }
}
