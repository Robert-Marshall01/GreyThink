<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/fmt" prefix="fmt" %>

<!DOCTYPE html>
<html>
<head>
    <title>Grey Legacy - Claims Dashboard</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/css/claims.css" />
</head>
<body>
    <div class="container">
        <jsp:include page="/WEB-INF/jsp/includes/header.jsp" />

        <h1>Claims Dashboard</h1>

        <!-- ==================== Summary Cards ==================== -->
        <div class="dashboard-cards">
            <div class="card">
                <h3>Open</h3>
                <span class="card-count"><c:out value="${openCount}" default="0" /></span>
            </div>
            <div class="card">
                <h3>Under Review</h3>
                <span class="card-count"><c:out value="${underReviewCount}" default="0" /></span>
            </div>
            <div class="card">
                <h3>Pending Documents</h3>
                <span class="card-count"><c:out value="${pendingDocsCount}" default="0" /></span>
            </div>
            <div class="card">
                <h3>Adjuster Assigned</h3>
                <span class="card-count"><c:out value="${adjusterAssignedCount}" default="0" /></span>
            </div>
            <div class="card">
                <h3>Approved</h3>
                <span class="card-count"><c:out value="${approvedCount}" default="0" /></span>
            </div>
            <div class="card">
                <h3>Denied</h3>
                <span class="card-count"><c:out value="${deniedCount}" default="0" /></span>
            </div>
            <div class="card">
                <h3>Settled</h3>
                <span class="card-count"><c:out value="${settledCount}" default="0" /></span>
            </div>
            <div class="card card-alert">
                <h3>Fraud Suspected</h3>
                <span class="card-count"><c:out value="${fraudSuspectedCount}" default="0" /></span>
            </div>
        </div>

        <!-- ==================== Recent Open Claims ==================== -->
        <div class="detail-section">
            <h2>Recent Open Claims</h2>
            <c:choose>
                <c:when test="${empty recentClaims}">
                    <p class="no-data">No recent open claims.</p>
                </c:when>
                <c:otherwise>
                    <table class="data-table">
                        <thead>
                            <tr>
                                <th>Claim Number</th>
                                <th>Policy Number</th>
                                <th>Claimant</th>
                                <th>Type</th>
                                <th>Status</th>
                                <th>Date Filed</th>
                                <th>Estimated Loss</th>
                            </tr>
                        </thead>
                        <tbody>
                            <c:forEach var="claim" items="${recentClaims}">
                                <tr>
                                    <td>
                                        <a href="${pageContext.request.contextPath}/viewClaim.do?claimId=${claim.id}">
                                            <c:out value="${claim.claimNumber}" />
                                        </a>
                                    </td>
                                    <td><c:out value="${claim.policy.policyNumber}" /></td>
                                    <td><c:out value="${claim.claimantFirstName}" /> <c:out value="${claim.claimantLastName}" /></td>
                                    <td><c:out value="${claim.claimType}" /></td>
                                    <td>
                                        <span class="status-badge status-<c:out value='${claim.status}' />">
                                            <c:out value="${claim.status}" />
                                        </span>
                                    </td>
                                    <td><fmt:formatDate value="${claim.filingDate}" pattern="yyyy-MM-dd" /></td>
                                    <td><fmt:formatNumber value="${claim.estimatedLoss}" type="currency" /></td>
                                </tr>
                            </c:forEach>
                        </tbody>
                    </table>
                </c:otherwise>
            </c:choose>
        </div>

        <!-- ==================== Fraud Alerts ==================== -->
        <div class="detail-section">
            <h2>Fraud Alerts</h2>
            <c:choose>
                <c:when test="${empty fraudAlerts}">
                    <p class="no-data">No active fraud alerts.</p>
                </c:when>
                <c:otherwise>
                    <table class="data-table table-alert">
                        <thead>
                            <tr>
                                <th>Claim Number</th>
                                <th>Claimant</th>
                                <th>Fraud Score</th>
                                <th>Risk Level</th>
                                <th>Date Filed</th>
                                <th>Estimated Loss</th>
                                <th>Actions</th>
                            </tr>
                        </thead>
                        <tbody>
                            <c:forEach var="alert" items="${fraudAlerts}">
                                <tr class="fraud-row fraud-<c:out value='${alert.fraudRiskLevel}' />">
                                    <td>
                                        <a href="${pageContext.request.contextPath}/viewClaim.do?claimId=${alert.id}">
                                            <c:out value="${alert.claimNumber}" />
                                        </a>
                                    </td>
                                    <td><c:out value="${alert.claimantFirstName}" /> <c:out value="${alert.claimantLastName}" /></td>
                                    <td>
                                        <span class="fraud-score">
                                            <fmt:formatNumber value="${alert.fraudScore}" maxFractionDigits="1" />
                                        </span>
                                    </td>
                                    <td>
                                        <span class="fraud-badge fraud-<c:out value='${alert.fraudRiskLevel}' />">
                                            <c:out value="${alert.fraudRiskLevel}" />
                                        </span>
                                    </td>
                                    <td><fmt:formatDate value="${alert.filingDate}" pattern="yyyy-MM-dd" /></td>
                                    <td><fmt:formatNumber value="${alert.estimatedLoss}" type="currency" /></td>
                                    <td>
                                        <a href="${pageContext.request.contextPath}/viewClaim.do?claimId=${alert.id}"
                                           class="btn btn-small">Review</a>
                                    </td>
                                </tr>
                            </c:forEach>
                        </tbody>
                    </table>
                </c:otherwise>
            </c:choose>
        </div>

        <jsp:include page="/WEB-INF/jsp/includes/footer.jsp" />
    </div>
</body>
</html>
