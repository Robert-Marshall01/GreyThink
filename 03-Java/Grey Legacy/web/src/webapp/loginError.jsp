<%@ page contentType="text/html;charset=UTF-8" language="java" %>
    <%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
        <!DOCTYPE html>
        <html>

        <head>
            <title>Grey Legacy — Authentication Failed</title>
            <link rel="stylesheet" href="${pageContext.request.contextPath}/css/claims.css" />
        </head>

        <body>

            <%-- Container-Managed FORM Login Error Page========================================This JSP is referenced
                by the <form-error-page> element in web.xml.
                The Servlet container redirects here when j_security_check authentication
                fails (wrong credentials, locked account, expired password, etc.).

                SECURITY NOTE: This page deliberately does not reveal whether the
                username or the password was incorrect. Specific failure reasons
                (account locked, expired, disabled) are logged server-side by
                GreyLegacyLoginModule and recorded in the LOGIN_AUDIT table.

                DESIGN NOTE: This page immediately redirects to login.jsp with an
                error parameter, following the Post-Redirect-Get pattern to prevent
                the browser from re-submitting credentials on refresh.
                --%>

                <%-- Strategy: Redirect back to login.jsp with ?error=1 parameter. This prevents the user from
                    seeing "j_security_check" in the address bar and avoids credential re-submission on browser refresh.
                    The login.jsp page checks for the 'error' parameter and displays the appropriate error message. --%>
                    <c:redirect url="/login.jsp?error=1" />

        </body>

        </html>