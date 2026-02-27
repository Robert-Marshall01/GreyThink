<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/fmt" prefix="fmt" %>

<!DOCTYPE html>
<html>
<head>
    <title>Grey Legacy - Claim Detail - <c:out value="${claim.claimNumber}" /></title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/css/claims.css" />
</head>
<body>
    <div class="container">
        <jsp:include page="/WEB-INF/jsp/includes/header.jsp" />

        <h1>Claim Detail: <c:out value="${claim.claimNumber}" /></h1>

        <!-- ==================== Claim Header ==================== -->
        <div class="detail-section">
            <h2>Claim Information</h2>
            <table class="detail-table">
                <tr>
                    <th>Claim Number:</th>
                    <td><c:out value="${claim.claimNumber}" /></td>
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
                    <th>Priority:</th>
                    <td><c:out value="${claim.priority}" default="Normal" /></td>
                </tr>
                <tr>
                    <th>Date of Loss:</th>
                    <td><fmt:formatDate value="${claim.lossDate}" pattern="yyyy-MM-dd" /></td>
                    <th>Date Filed:</th>
                    <td><fmt:formatDate value="${claim.filingDate}" pattern="yyyy-MM-dd HH:mm" /></td>
                </tr>
                <tr>
                    <th>Loss Location:</th>
                    <td colspan="3"><c:out value="${claim.lossLocation}" /></td>
                </tr>
                <tr>
                    <th>Description:</th>
                    <td colspan="3"><c:out value="${claim.lossDescription}" /></td>
                </tr>
            </table>
        </div>

        <!-- ==================== Policy Information ==================== -->
        <div class="detail-section">
            <h2>Policy Information</h2>
            <table class="detail-table">
                <tr>
                    <th>Policy Number:</th>
                    <td><c:out value="${claim.policy.policyNumber}" /></td>
                    <th>Policy Type:</th>
                    <td><c:out value="${claim.policy.policyType}" /></td>
                </tr>
                <tr>
                    <th>Policy Status:</th>
                    <td><c:out value="${claim.policy.status}" /></td>
                    <th>Coverage Limit:</th>
                    <td><fmt:formatNumber value="${claim.policy.coverageLimit}" type="currency" /></td>
                </tr>
                <tr>
                    <th>Effective Date:</th>
                    <td><fmt:formatDate value="${claim.policy.effectiveDate}" pattern="yyyy-MM-dd" /></td>
                    <th>Expiration Date:</th>
                    <td><fmt:formatDate value="${claim.policy.expirationDate}" pattern="yyyy-MM-dd" /></td>
                </tr>
                <tr>
                    <th>Policyholder:</th>
                    <td colspan="3"><c:out value="${claim.policy.holderName}" /></td>
                </tr>
            </table>
        </div>

        <!-- ==================== Claimant Information ==================== -->
        <div class="detail-section">
            <h2>Claimant Information</h2>
            <table class="detail-table">
                <tr>
                    <th>Name:</th>
                    <td><c:out value="${claim.claimantFirstName}" /> <c:out value="${claim.claimantLastName}" /></td>
                </tr>
                <tr>
                    <th>Phone:</th>
                    <td><c:out value="${claim.claimantPhone}" default="N/A" /></td>
                    <th>Email:</th>
                    <td><c:out value="${claim.claimantEmail}" default="N/A" /></td>
                </tr>
            </table>
        </div>

        <!-- ==================== Adjuster Information ==================== -->
        <c:if test="${claim.adjuster != null}">
        <div class="detail-section">
            <h2>Assigned Adjuster</h2>
            <table class="detail-table">
                <tr>
                    <th>Name:</th>
                    <td><c:out value="${claim.adjuster.firstName}" /> <c:out value="${claim.adjuster.lastName}" /></td>
                    <th>License #:</th>
                    <td><c:out value="${claim.adjuster.licenseNumber}" /></td>
                </tr>
                <tr>
                    <th>Phone:</th>
                    <td><c:out value="${claim.adjuster.phone}" /></td>
                    <th>Email:</th>
                    <td><c:out value="${claim.adjuster.email}" /></td>
                </tr>
                <tr>
                    <th>Specialization:</th>
                    <td><c:out value="${claim.adjuster.specialization}" /></td>
                    <th>Region:</th>
                    <td><c:out value="${claim.adjuster.region}" /></td>
                </tr>
            </table>
        </div>
        </c:if>

        <!-- ==================== Financial Details ==================== -->
        <div class="detail-section">
            <h2>Financial Details</h2>
            <table class="detail-table">
                <tr>
                    <th>Estimated Loss:</th>
                    <td><fmt:formatNumber value="${claim.estimatedLoss}" type="currency" /></td>
                    <th>Approved Amount:</th>
                    <td><fmt:formatNumber value="${claim.approvedAmount}" type="currency" /></td>
                </tr>
                <tr>
                    <th>Deductible:</th>
                    <td><fmt:formatNumber value="${claim.deductible}" type="currency" /></td>
                    <th>Net Payout:</th>
                    <td>
                        <strong>
                            <fmt:formatNumber value="${claim.approvedAmount - claim.deductible}" type="currency" />
                        </strong>
                    </td>
                </tr>
            </table>
        </div>

        <!-- ==================== Fraud Scoring ==================== -->
        <c:if test="${claim.fraudScore != null}">
        <div class="detail-section">
            <h2>Fraud Risk Assessment</h2>
            <table class="detail-table">
                <tr>
                    <th>Fraud Score:</th>
                    <td>
                        <span class="fraud-score fraud-<c:out value='${claim.fraudRiskLevel}' />">
                            <fmt:formatNumber value="${claim.fraudScore}" maxFractionDigits="1" />
                        </span>
                    </td>
                    <th>Risk Level:</th>
                    <td>
                        <span class="fraud-badge fraud-<c:out value='${claim.fraudRiskLevel}' />">
                            <c:out value="${claim.fraudRiskLevel}" />
                        </span>
                    </td>
                </tr>
                <c:if test="${not empty claim.fraudIndicators}">
                <tr>
                    <th>Fraud Indicators:</th>
                    <td colspan="3">
                        <ul class="fraud-indicators">
                            <c:forEach var="indicator" items="${claim.fraudIndicators}">
                                <li><c:out value="${indicator}" /></li>
                            </c:forEach>
                        </ul>
                    </td>
                </tr>
                </c:if>
            </table>
        </div>
        </c:if>

        <!-- ==================== Payments ==================== -->
        <div class="detail-section">
            <h2>Payments</h2>
            <c:choose>
                <c:when test="${empty claim.payments}">
                    <p class="no-data">No payments have been issued for this claim.</p>
                </c:when>
                <c:otherwise>
                    <table class="data-table">
                        <thead>
                            <tr>
                                <th>Payment #</th>
                                <th>Date</th>
                                <th>Amount</th>
                                <th>Method</th>
                                <th>Status</th>
                                <th>Payee</th>
                                <th>Reference</th>
                            </tr>
                        </thead>
                        <tbody>
                            <c:forEach var="payment" items="${claim.payments}">
                                <tr>
                                    <td><c:out value="${payment.paymentNumber}" /></td>
                                    <td><fmt:formatDate value="${payment.paymentDate}" pattern="yyyy-MM-dd" /></td>
                                    <td><fmt:formatNumber value="${payment.amount}" type="currency" /></td>
                                    <td><c:out value="${payment.paymentMethod}" /></td>
                                    <td>
                                        <span class="status-badge status-<c:out value='${payment.status}' />">
                                            <c:out value="${payment.status}" />
                                        </span>
                                    </td>
                                    <td><c:out value="${payment.payeeName}" /></td>
                                    <td><c:out value="${payment.referenceNumber}" /></td>
                                </tr>
                            </c:forEach>
                        </tbody>
                    </table>
                </c:otherwise>
            </c:choose>
        </div>

        <!-- ==================== Audit History ==================== -->
        <div class="detail-section">
            <h2>Audit History</h2>
            <c:choose>
                <c:when test="${empty auditEntries}">
                    <p class="no-data">No audit history available.</p>
                </c:when>
                <c:otherwise>
                    <table class="data-table">
                        <thead>
                            <tr>
                                <th>Timestamp</th>
                                <th>Action</th>
                                <th>Field Changed</th>
                                <th>Old Value</th>
                                <th>New Value</th>
                                <th>User</th>
                            </tr>
                        </thead>
                        <tbody>
                            <c:forEach var="audit" items="${auditEntries}">
                                <tr>
                                    <td><fmt:formatDate value="${audit.timestamp}" pattern="yyyy-MM-dd HH:mm:ss" /></td>
                                    <td><c:out value="${audit.action}" /></td>
                                    <td><c:out value="${audit.fieldChanged}" default="-" /></td>
                                    <td><c:out value="${audit.oldValue}" default="-" /></td>
                                    <td><c:out value="${audit.newValue}" default="-" /></td>
                                    <td><c:out value="${audit.performedBy}" /></td>
                                </tr>
                            </c:forEach>
                        </tbody>
                    </table>
                </c:otherwise>
            </c:choose>
        </div>

        <!-- ==================== Notes ==================== -->
        <div class="detail-section">
            <h2>Claim Notes</h2>
            <c:choose>
                <c:when test="${empty claim.notes}">
                    <p class="no-data">No notes recorded for this claim.</p>
                </c:when>
                <c:otherwise>
                    <div class="notes-list">
                        <c:forEach var="note" items="${claim.notes}">
                            <div class="note-entry">
                                <div class="note-header">
                                    <strong><c:out value="${note.author}" /></strong>
                                    <span class="note-date">
                                        <fmt:formatDate value="${note.createdDate}" pattern="yyyy-MM-dd HH:mm" />
                                    </span>
                                </div>
                                <div class="note-body">
                                    <c:out value="${note.content}" />
                                </div>
                            </div>
                        </c:forEach>
                    </div>
                </c:otherwise>
            </c:choose>
        </div>

        <div class="form-actions">
            <a href="${pageContext.request.contextPath}/searchClaims.do" class="btn btn-secondary">Back to Search</a>
            <a href="${pageContext.request.contextPath}/dashboard.do" class="btn btn-secondary">Dashboard</a>
        </div>

        <jsp:include page="/WEB-INF/jsp/includes/footer.jsp" />
    </div>
</body>
</html>
