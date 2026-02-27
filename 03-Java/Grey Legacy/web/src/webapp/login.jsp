<%@ page contentType="text/html;charset=UTF-8" language="java" %>
    <%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
        <!DOCTYPE html>
        <html>

        <head>
            <title>Grey Legacy — Sign In</title>
            <link rel="stylesheet" href="${pageContext.request.contextPath}/css/claims.css" />
            <style>
                /* Login-specific styles — kept inline because login.jsp is served
           outside the authenticated context (no Spring Security filter chain). */
                .login-wrapper {
                    display: flex;
                    justify-content: center;
                    align-items: center;
                    min-height: 80vh;
                }

                .login-box {
                    background: #fff;
                    border-radius: 4px;
                    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);
                    padding: 40px 35px 30px;
                    width: 380px;
                }

                .login-box h1 {
                    text-align: center;
                    font-size: 1.3em;
                    border-bottom: none;
                    margin-bottom: 5px;
                }

                .login-box .subtitle {
                    text-align: center;
                    margin-bottom: 25px;
                }

                .login-box .form-group {
                    margin-bottom: 18px;
                }

                .login-box .form-group label {
                    display: block;
                    font-weight: 500;
                    margin-bottom: 5px;
                    color: #555;
                }

                .login-box .form-group input {
                    width: 100%;
                    padding: 10px 12px;
                    border: 1px solid #ccc;
                    border-radius: 3px;
                    font-size: 14px;
                }

                .login-box .form-group input:focus {
                    border-color: #3498db;
                    outline: none;
                    box-shadow: 0 0 0 2px rgba(52, 152, 219, 0.2);
                }

                .login-box .btn-login {
                    width: 100%;
                    padding: 10px;
                    background-color: #2c3e50;
                    color: #fff;
                    border: none;
                    border-radius: 3px;
                    font-size: 15px;
                    cursor: pointer;
                    margin-top: 5px;
                }

                .login-box .btn-login:hover {
                    background-color: #34495e;
                }

                .login-footer {
                    text-align: center;
                    margin-top: 20px;
                    font-size: 12px;
                    color: #95a5a6;
                }
            </style>
        </head>

        <body>

            <%-- Container-Managed FORM Login Page=================================This JSP is referenced by the
                <form-login-page> element in web.xml.
                The form MUST POST to j_security_check with parameters j_username and
                j_password — these are mandated by the Servlet specification (§13.6.3).

                The container (Tomcat, JBoss, WebSphere, WebLogic) intercepts the POST,
                authenticates via the configured JAAS LoginModule (GreyLegacyLoginModule),
                and redirects to the originally requested URL on success, or to
                loginError.jsp on failure.

                This page is deliberately simple — no JavaScript frameworks, no AJAX.
                Legacy insurance systems prioritize reliability over aesthetics.
                --%>

                <div class="login-wrapper">
                    <div class="login-box">
                        <h1>Grey Legacy</h1>
                        <p class="subtitle">Insurance Claims Processing System</p>

                        <%-- Display error message if redirected from loginError.jsp --%>
                            <c:if test="${param.error != null}">
                                <div class="alert alert-danger">
                                    Invalid username or password. Please try again.
                                </div>
                            </c:if>

                            <%-- Display session timeout message --%>
                                <c:if test="${param.timeout != null}">
                                    <div class="alert alert-warning">
                                        Your session has expired. Please sign in again.
                                    </div>
                                </c:if>

                                <%-- Display account locked message --%>
                                    <c:if test="${param.locked != null}">
                                        <div class="alert alert-danger">
                                            Account locked due to too many failed attempts. Contact your administrator.
                                        </div>
                                    </c:if>

                                    <%-- j_security_check is the standard Servlet container login endpoint. j_username
                                        and j_password are the standard parameter names. These are NOT configurable —
                                        they are defined in the Servlet spec. --%>
                                        <form method="POST" action="j_security_check" autocomplete="off">

                                            <div class="form-group">
                                                <label for="j_username">Username</label>
                                                <input type="text" id="j_username" name="j_username"
                                                    placeholder="Enter your username" autocomplete="username" required
                                                    autofocus />
                                            </div>

                                            <div class="form-group">
                                                <label for="j_password">Password</label>
                                                <input type="password" id="j_password" name="j_password"
                                                    placeholder="Enter your password" autocomplete="current-password"
                                                    required />
                                            </div>

                                            <button type="submit" class="btn-login">Sign In</button>
                                        </form>

                                        <div class="login-footer">
                                            <p>Authorized personnel only. All access is logged and audited.</p>
                                            <p>&copy; Grey Legacy Insurance Systems</p>
                                        </div>
                    </div>
                </div>

        </body>

        </html>