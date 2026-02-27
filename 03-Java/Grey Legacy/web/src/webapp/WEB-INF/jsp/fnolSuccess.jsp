<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/fmt" prefix="fmt" %>

<!DOCTYPE html>
<html>
<head>
    <title>Grey Legacy - FNOL Submitted Successfully</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/css/claims.css" />
</head>
<body>
    <div class="container">
        <jsp:include page="/WEB-INF/jsp/includes/header.jsp" />

        <h1>FNOL Submitted Successfully</h1>

        <div class="alert alert-success">
            <p>Your First Notice of Loss has been recorded. A claims adjuster will be assigned shortly.</p>
        </div>

        <div class="detail-section">
            <h2>Claim Summary</h2>
            <table class="detail-table">
                <tr>
                    <th>Claim Number:</th>
                    <td><strong><c:out value="${claim.claimNumber}" /></strong></td>
                </tr>
                <tr>
                    <th>Status:</th>
                    <td>
                        <span class="status-badge status-<c:out value='${claim.status}' />">
                            <c:out value="${claim.status}" />
                        </span>
                    </td>
                </tr>
                <tr>
                    <th>Claim Type:</th>
                    <td><c:out value="${claim.claimType}" /></td>
                </tr>
                <tr>
                    <th>Date of Loss:</th>
                    <td><fmt:formatDate value="${claim.lossDate}" pattern="yyyy-MM-dd" /></td>
                </tr>
                <tr>
                    <th>Date Filed:</th>
                    <td><fmt:formatDate value="${claim.filingDate}" pattern="yyyy-MM-dd HH:mm" /></td>
                </tr>
                <tr>
                    <th>Claimant:</th>
                    <td><c:out value="${claim.claimantFirstName}" /> <c:out value="${claim.claimantLastName}" /></td>
                </tr>
                <tr>
                    <th>Policy Number:</th>
                    <td><c:out value="${claim.policy.policyNumber}" /></td>
                </tr>
                <c:if test="${claim.estimatedLoss != null}">
                <tr>
                    <th>Estimated Loss:</th>
                    <td><fmt:formatNumber value="${claim.estimatedLoss}" type="currency" /></td>
                </tr>
                </c:if>
                <tr>
                    <th>Description:</th>
                    <td><c:out value="${claim.lossDescription}" /></td>
                </tr>
            </table>
        </div>

        <div class="form-actions">
            <a href="${pageContext.request.contextPath}/viewClaim.do?claimId=${claim.id}" class="btn btn-primary">
                View Claim Details
            </a>
            <a href="${pageContext.request.contextPath}/fnol.do" class="btn btn-secondary">
                Submit Another FNOL
            </a>
            <a href="${pageContext.request.contextPath}/dashboard.do" class="btn btn-secondary">
                Return to Dashboard
            </a>
        </div>

        <jsp:include page="/WEB-INF/jsp/includes/footer.jsp" />
    </div>
</body>
</html>
