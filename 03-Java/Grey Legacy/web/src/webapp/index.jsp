<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<!DOCTYPE html>
<html>
<head>
    <title>Grey Legacy Claims System</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/css/claims.css" />
</head>
<body>
    <div class="container">
        <jsp:include page="/WEB-INF/jsp/includes/header.jsp" />

        <h1>Welcome to Grey Legacy</h1>
        <p class="subtitle">Enterprise Insurance Claims Processing System</p>

        <div class="dashboard-cards">
            <div class="card">
                <h3>Quick Actions</h3>
                <ul style="list-style:none; padding-top:10px;">
                    <li style="margin-bottom:8px;">
                        <a href="${pageContext.request.contextPath}/fnol.do" class="btn btn-primary">
                            New Claim (FNOL)
                        </a>
                    </li>
                    <li style="margin-bottom:8px;">
                        <a href="${pageContext.request.contextPath}/searchClaims.do" class="btn btn-secondary">
                            Search Claims
                        </a>
                    </li>
                    <li style="margin-bottom:8px;">
                        <a href="${pageContext.request.contextPath}/dashboard.do" class="btn btn-secondary">
                            Dashboard
                        </a>
                    </li>
                </ul>
            </div>
            <div class="card">
                <h3>System Info</h3>
                <div style="padding-top:10px; color:#7f8c8d;">
                    <p><strong>Version:</strong> 1.0.0</p>
                    <p><strong>Build:</strong> Enterprise Edition</p>
                    <p><strong>Server:</strong> <%= application.getServerInfo() %></p>
                    <p><strong>Java:</strong> <%= System.getProperty("java.version") %></p>
                </div>
            </div>
        </div>

        <jsp:include page="/WEB-INF/jsp/includes/footer.jsp" />
    </div>
</body>
</html>
