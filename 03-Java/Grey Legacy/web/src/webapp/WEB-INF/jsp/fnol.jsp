<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib uri="http://struts.apache.org/tags-html" prefix="html" %>
<%@ taglib uri="http://struts.apache.org/tags-bean" prefix="bean" %>
<%@ taglib uri="http://struts.apache.org/tags-logic" prefix="logic" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>

<!DOCTYPE html>
<html>
<head>
    <title>Grey Legacy - First Notice of Loss (FNOL)</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/css/claims.css" />
</head>
<body>
    <div class="container">
        <jsp:include page="/WEB-INF/jsp/includes/header.jsp" />

        <h1>First Notice of Loss (FNOL)</h1>
        <p class="subtitle">Submit a new insurance claim</p>

        <!-- Display errors -->
        <html:errors/>
        <logic:messagesPresent message="true">
            <div class="alert alert-success">
                <html:messages id="msg" message="true">
                    <bean:write name="msg"/>
                </html:messages>
            </div>
        </logic:messagesPresent>

        <html:form action="/submitFnol" method="post" styleClass="form-standard">

            <fieldset>
                <legend>Policy Information</legend>
                <div class="form-group">
                    <label for="policyNumber">Policy Number <span class="required">*</span></label>
                    <html:text property="policyNumber" styleId="policyNumber"
                               styleClass="form-control" maxlength="20" />
                </div>
            </fieldset>

            <fieldset>
                <legend>Claimant Information</legend>
                <div class="form-row">
                    <div class="form-group">
                        <label for="claimantFirstName">First Name <span class="required">*</span></label>
                        <html:text property="claimantFirstName" styleId="claimantFirstName"
                                   styleClass="form-control" maxlength="50" />
                    </div>
                    <div class="form-group">
                        <label for="claimantLastName">Last Name <span class="required">*</span></label>
                        <html:text property="claimantLastName" styleId="claimantLastName"
                                   styleClass="form-control" maxlength="50" />
                    </div>
                </div>
                <div class="form-row">
                    <div class="form-group">
                        <label for="claimantPhone">Phone</label>
                        <html:text property="claimantPhone" styleId="claimantPhone"
                                   styleClass="form-control" maxlength="20" />
                    </div>
                    <div class="form-group">
                        <label for="claimantEmail">Email</label>
                        <html:text property="claimantEmail" styleId="claimantEmail"
                                   styleClass="form-control" maxlength="100" />
                    </div>
                </div>
            </fieldset>

            <fieldset>
                <legend>Incident Details</legend>
                <div class="form-group">
                    <label for="claimType">Claim Type <span class="required">*</span></label>
                    <html:select property="claimType" styleId="claimType" styleClass="form-control">
                        <html:option value="">-- Select Type --</html:option>
                        <html:option value="COLLISION">Collision</html:option>
                        <html:option value="COMPREHENSIVE">Comprehensive</html:option>
                        <html:option value="LIABILITY">Liability</html:option>
                        <html:option value="PROPERTY_DAMAGE">Property Damage</html:option>
                        <html:option value="BODILY_INJURY">Bodily Injury</html:option>
                        <html:option value="MEDICAL">Medical</html:option>
                        <html:option value="THEFT">Theft</html:option>
                        <html:option value="FIRE">Fire</html:option>
                        <html:option value="NATURAL_DISASTER">Natural Disaster</html:option>
                        <html:option value="OTHER">Other</html:option>
                    </html:select>
                </div>
                <div class="form-row">
                    <div class="form-group">
                        <label for="lossDate">Date of Loss <span class="required">*</span></label>
                        <html:text property="lossDate" styleId="lossDate"
                                   styleClass="form-control" maxlength="10" />
                        <span class="help-text">Format: YYYY-MM-DD</span>
                    </div>
                    <div class="form-group">
                        <label for="estimatedLoss">Estimated Loss ($)</label>
                        <html:text property="estimatedLoss" styleId="estimatedLoss"
                                   styleClass="form-control" maxlength="15" />
                    </div>
                </div>
                <div class="form-group">
                    <label for="lossLocation">Loss Location</label>
                    <html:text property="lossLocation" styleId="lossLocation"
                               styleClass="form-control" maxlength="200" />
                </div>
                <div class="form-group">
                    <label for="lossDescription">Description of Loss <span class="required">*</span></label>
                    <html:textarea property="lossDescription" styleId="lossDescription"
                                   styleClass="form-control" rows="5" cols="60" />
                </div>
            </fieldset>

            <div class="form-actions">
                <html:submit styleClass="btn btn-primary">Submit FNOL</html:submit>
                <html:reset styleClass="btn btn-secondary">Reset</html:reset>
            </div>
        </html:form>

        <jsp:include page="/WEB-INF/jsp/includes/footer.jsp" />
    </div>
</body>
</html>
