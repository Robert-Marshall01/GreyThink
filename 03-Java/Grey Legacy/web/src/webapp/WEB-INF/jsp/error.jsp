<%@ page contentType="text/html;charset=UTF-8" language="java" isErrorPage="true" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>

<!DOCTYPE html>
<html>
<head>
    <title>Grey Legacy - Error</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/css/claims.css" />
</head>
<body>
    <div class="container">
        <jsp:include page="/WEB-INF/jsp/includes/header.jsp" />

        <h1>An Error Has Occurred</h1>

        <div class="alert alert-error">
            <h3>
                <c:choose>
                    <c:when test="${not empty errorTitle}">
                        <c:out value="${errorTitle}" />
                    </c:when>
                    <c:otherwise>
                        Unexpected Error
                    </c:otherwise>
                </c:choose>
            </h3>

            <c:if test="${not empty errorMessage}">
                <p><c:out value="${errorMessage}" /></p>
            </c:if>

            <c:if test="${not empty requestScope['javax.servlet.error.status_code']}">
                <p><strong>HTTP Status Code:</strong> <c:out value="${requestScope['javax.servlet.error.status_code']}" /></p>
            </c:if>

            <c:if test="${not empty requestScope['javax.servlet.error.request_uri']}">
                <p><strong>Requested URI:</strong> <c:out value="${requestScope['javax.servlet.error.request_uri']}" /></p>
            </c:if>

            <c:if test="${not empty requestScope['javax.servlet.error.servlet_name']}">
                <p><strong>Servlet Name:</strong> <c:out value="${requestScope['javax.servlet.error.servlet_name']}" /></p>
            </c:if>

            <c:if test="${not empty requestScope['javax.servlet.error.exception']}">
                <p><strong>Exception:</strong> <c:out value="${requestScope['javax.servlet.error.exception'].class.name}" /></p>
                <p><strong>Message:</strong> <c:out value="${requestScope['javax.servlet.error.exception'].message}" /></p>
            </c:if>
        </div>

        <div class="form-actions">
            <a href="${pageContext.request.contextPath}/dashboard.do" class="btn btn-primary">Return to Dashboard</a>
            <a href="javascript:history.back()" class="btn btn-secondary">Go Back</a>
        </div>

        <jsp:include page="/WEB-INF/jsp/includes/footer.jsp" />
    </div>
</body>
</html>
