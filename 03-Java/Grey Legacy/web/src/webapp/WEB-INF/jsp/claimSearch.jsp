<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib uri="http://struts.apache.org/tags-html" prefix="html" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/fmt" prefix="fmt" %>

<!DOCTYPE html>
<html>
<head>
    <title>Grey Legacy - Claim Search</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/css/claims.css" />
</head>
<body>
    <div class="container">
        <jsp:include page="/WEB-INF/jsp/includes/header.jsp" />

        <h1>Search Claims</h1>

        <!-- Display errors -->
        <html:errors/>

        <html:form action="/searchClaims" method="post" styleClass="form-standard search-form">
            <div class="form-row">
                <div class="form-group">
                    <label for="claimNumber">Claim Number</label>
                    <html:text property="claimNumber" styleId="claimNumber"
                               styleClass="form-control" maxlength="20" />
                </div>
                <div class="form-group">
                    <label for="policyNumber">Policy Number</label>
                    <html:text property="policyNumber" styleId="policyNumber"
                               styleClass="form-control" maxlength="20" />
                </div>
            </div>
            <div class="form-row">
                <div class="form-group">
                    <label for="status">Claim Status</label>
                    <html:select property="status" styleId="status" styleClass="form-control">
                        <html:option value="">-- All Statuses --</html:option>
                        <html:option value="OPEN">Open</html:option>
                        <html:option value="UNDER_REVIEW">Under Review</html:option>
                        <html:option value="PENDING_DOCUMENTS">Pending Documents</html:option>
                        <html:option value="ADJUSTER_ASSIGNED">Adjuster Assigned</html:option>
                        <html:option value="APPROVED">Approved</html:option>
                        <html:option value="DENIED">Denied</html:option>
                        <html:option value="SETTLED">Settled</html:option>
                        <html:option value="CLOSED">Closed</html:option>
                        <html:option value="REOPENED">Reopened</html:option>
                        <html:option value="FRAUD_SUSPECTED">Fraud Suspected</html:option>
                    </html:select>
                </div>
                <div class="form-group">
                    <label for="claimantLastName">Claimant Last Name</label>
                    <html:text property="claimantLastName" styleId="claimantLastName"
                               styleClass="form-control" maxlength="50" />
                </div>
            </div>
            <div class="form-actions">
                <html:submit styleClass="btn btn-primary">Search</html:submit>
                <html:reset styleClass="btn btn-secondary">Clear</html:reset>
            </div>
        </html:form>

        <!-- Search Results -->
        <c:if test="${searchPerformed}">
            <h2>Search Results</h2>

            <c:choose>
                <c:when test="${empty searchResults}">
                    <div class="alert alert-info">
                        <p>No claims found matching your search criteria.</p>
                    </div>
                </c:when>
                <c:otherwise>
                    <p class="result-count">Found <strong><c:out value="${resultCount}" /></strong> claim(s)</p>
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
                                <th>Actions</th>
                            </tr>
                        </thead>
                        <tbody>
                            <c:forEach var="claim" items="${searchResults}">
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
                                    <td>
                                        <a href="${pageContext.request.contextPath}/viewClaim.do?claimId=${claim.id}"
                                           class="btn btn-small">View</a>
                                    </td>
                                </tr>
                            </c:forEach>
                        </tbody>
                    </table>
                </c:otherwise>
            </c:choose>
        </c:if>

        <jsp:include page="/WEB-INF/jsp/includes/footer.jsp" />
    </div>
</body>
</html>
