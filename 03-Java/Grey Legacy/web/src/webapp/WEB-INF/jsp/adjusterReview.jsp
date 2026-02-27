<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib uri="http://struts.apache.org/tags-html" prefix="html" %>
<%@ taglib uri="http://struts.apache.org/tags-bean" prefix="bean" %>
<%@ taglib uri="http://struts.apache.org/tags-logic" prefix="logic" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/fmt" prefix="fmt" %>

<!DOCTYPE html>
<html>
<head>
    <title>Grey Legacy - Adjuster Review</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/css/claims.css" />
</head>
<body>
    <div class="container">
        <jsp:include page="/WEB-INF/jsp/includes/header.jsp" />

        <h1>Adjuster Review</h1>
        <p class="subtitle">
            Claim: <strong><c:out value="${claim.claimNumber}" /></strong> |
            Claimant: <c:out value="${claim.claimantFirstName}" /> <c:out value="${claim.claimantLastName}" /> |
            Estimated Loss: <fmt:formatNumber value="${claim.estimatedLoss}" type="currency" />
        </p>

        <!-- Display errors -->
        <html:errors/>
        <logic:messagesPresent message="true">
            <div class="alert alert-success">
                <html:messages id="msg" message="true">
                    <bean:write name="msg"/>
                </html:messages>
            </div>
        </logic:messagesPresent>

        <html:form action="/submitAdjusterReview" method="post" styleClass="form-standard">

            <html:hidden property="claimId" />

            <fieldset>
                <legend>Review Details</legend>

                <div class="form-group">
                    <label for="findings">Findings <span class="required">*</span></label>
                    <html:textarea property="findings" styleId="findings"
                                   styleClass="form-control" rows="8" cols="60" />
                    <span class="help-text">Provide a detailed summary of your investigation findings.</span>
                </div>

                <div class="form-row">
                    <div class="form-group">
                        <label for="recommendedAmount">Recommended Amount ($) <span class="required">*</span></label>
                        <html:text property="recommendedAmount" styleId="recommendedAmount"
                                   styleClass="form-control" maxlength="15" />
                    </div>
                    <div class="form-group">
                        <label for="recommendation">Recommendation <span class="required">*</span></label>
                        <html:select property="recommendation" styleId="recommendation" styleClass="form-control">
                            <html:option value="">-- Select Recommendation --</html:option>
                            <html:option value="APPROVE">Approve</html:option>
                            <html:option value="DENY">Deny</html:option>
                            <html:option value="ESCALATE">Escalate</html:option>
                        </html:select>
                    </div>
                </div>
            </fieldset>

            <div class="form-actions">
                <html:submit styleClass="btn btn-primary">Submit Review</html:submit>
                <a href="${pageContext.request.contextPath}/viewClaim.do?claimId=${claim.id}"
                   class="btn btn-secondary">Cancel</a>
            </div>
        </html:form>

        <jsp:include page="/WEB-INF/jsp/includes/footer.jsp" />
    </div>
</body>
</html>
