package com.greylegacy.integration.jms;

import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.ObjectMessage;
import javax.jms.Session;
import javax.jms.TextMessage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jms.support.converter.MessageConversionException;
import org.springframework.jms.support.converter.MessageConverter;

/**
 * Spring {@link MessageConverter} that marshals {@link ClaimEventMessage}
 * instances to and from XML-formatted {@link TextMessage} payloads.
 *
 * <p>Uses simple manual XML building rather than JAXB so that no additional
 * compile-time annotation processing is required — typical of legacy
 * enterprise codebases that predated widespread JAXB adoption.</p>
 *
 * <p>{@link ObjectMessage} is used as a fallback for outbound conversion
 * when XML marshalling is not desired.</p>
 *
 * @author Grey Legacy Integration Team
 * @since 1.0
 */
public class ClaimMessageConverter implements MessageConverter {

    private static final Logger LOG = LoggerFactory.getLogger(ClaimMessageConverter.class);

    private static final String XML_NAMESPACE = "http://greylegacy.com/schema/jms";
    private static final String DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss";

    // -------------------------------------------------------------------------
    // MessageConverter — toMessage
    // -------------------------------------------------------------------------

    @Override
    public Message toMessage(Object object, Session session) throws JMSException, MessageConversionException {
        if (!(object instanceof ClaimEventMessage)) {
            throw new MessageConversionException(
                    "Cannot convert object of type " + object.getClass().getName()
                    + "; expected ClaimEventMessage");
        }

        ClaimEventMessage event = (ClaimEventMessage) object;
        String xml = marshalToXml(event);

        TextMessage textMessage = session.createTextMessage(xml);
        textMessage.setStringProperty("contentType", "application/xml");
        if (event.getCorrelationId() != null) {
            textMessage.setJMSCorrelationID(event.getCorrelationId());
        }
        LOG.debug("Marshalled ClaimEventMessage to XML TextMessage [claimNumber={}]", event.getClaimNumber());
        return textMessage;
    }

    // -------------------------------------------------------------------------
    // MessageConverter — fromMessage
    // -------------------------------------------------------------------------

    @Override
    public Object fromMessage(Message message) throws JMSException, MessageConversionException {
        if (message instanceof TextMessage) {
            String xml = ((TextMessage) message).getText();
            ClaimEventMessage event = unmarshalFromXml(xml);
            LOG.debug("Unmarshalled XML TextMessage to ClaimEventMessage [claimNumber={}]", event.getClaimNumber());
            return event;
        }

        if (message instanceof ObjectMessage) {
            Object payload = ((ObjectMessage) message).getObject();
            if (payload instanceof ClaimEventMessage) {
                return payload;
            }
            throw new MessageConversionException(
                    "ObjectMessage payload is not a ClaimEventMessage: "
                    + (payload != null ? payload.getClass().getName() : "null"));
        }

        throw new MessageConversionException(
                "Unsupported JMS message type: " + message.getClass().getName());
    }

    // -------------------------------------------------------------------------
    // XML marshalling helpers
    // -------------------------------------------------------------------------

    private String marshalToXml(ClaimEventMessage event) {
        SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
        StringBuilder sb = new StringBuilder();
        sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        sb.append("<claimEvent xmlns=\"").append(XML_NAMESPACE).append("\">\n");
        appendElement(sb, "messageId", event.getMessageId());
        appendElement(sb, "claimNumber", event.getClaimNumber());
        appendElement(sb, "eventType", event.getEventType() != null ? event.getEventType().name() : null);
        appendElement(sb, "policyNumber", event.getPolicyNumber());
        appendElement(sb, "claimantName", event.getClaimantName());
        appendElement(sb, "amount", event.getAmount() != null ? event.getAmount().toPlainString() : null);
        appendElement(sb, "timestamp", event.getTimestamp() != null ? sdf.format(event.getTimestamp()) : null);
        appendElement(sb, "correlationId", event.getCorrelationId());
        appendElement(sb, "sourceSystem", event.getSourceSystem());
        sb.append("</claimEvent>");
        return sb.toString();
    }

    private void appendElement(StringBuilder sb, String name, String value) {
        sb.append("  <").append(name).append(">");
        sb.append(value != null ? escapeXml(value) : "");
        sb.append("</").append(name).append(">\n");
    }

    private String escapeXml(String value) {
        return value.replace("&", "&amp;")
                    .replace("<", "&lt;")
                    .replace(">", "&gt;")
                    .replace("\"", "&quot;")
                    .replace("'", "&apos;");
    }

    private ClaimEventMessage unmarshalFromXml(String xml) {
        ClaimEventMessage event = new ClaimEventMessage();
        event.setMessageId(extractElement(xml, "messageId"));
        event.setClaimNumber(extractElement(xml, "claimNumber"));

        String eventTypeStr = extractElement(xml, "eventType");
        if (eventTypeStr != null && !eventTypeStr.isEmpty()) {
            try {
                event.setEventType(ClaimEventMessage.EventType.valueOf(eventTypeStr));
            } catch (IllegalArgumentException ex) {
                LOG.warn("Unknown eventType in XML: {}", eventTypeStr);
            }
        }

        event.setPolicyNumber(extractElement(xml, "policyNumber"));
        event.setClaimantName(extractElement(xml, "claimantName"));

        String amountStr = extractElement(xml, "amount");
        if (amountStr != null && !amountStr.isEmpty()) {
            event.setAmount(new BigDecimal(amountStr));
        }

        String timestampStr = extractElement(xml, "timestamp");
        if (timestampStr != null && !timestampStr.isEmpty()) {
            try {
                SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);
                event.setTimestamp(sdf.parse(timestampStr));
            } catch (ParseException ex) {
                LOG.warn("Failed to parse timestamp '{}': {}", timestampStr, ex.getMessage());
            }
        }

        event.setCorrelationId(extractElement(xml, "correlationId"));
        event.setSourceSystem(extractElement(xml, "sourceSystem"));
        return event;
    }

    /**
     * Extracts the text content of a simple XML element by tag name.
     * Not a full XML parser — adequate for the flat structure used here.
     */
    private String extractElement(String xml, String tagName) {
        String openTag = "<" + tagName + ">";
        String closeTag = "</" + tagName + ">";
        int start = xml.indexOf(openTag);
        int end = xml.indexOf(closeTag);
        if (start < 0 || end < 0 || end <= start) {
            return null;
        }
        return xml.substring(start + openTag.length(), end);
    }
}
