package com.greylegacy.integration.camel;

import org.apache.camel.Exchange;
import org.apache.camel.LoggingLevel;
import org.apache.camel.Predicate;
import org.apache.camel.Processor;
import org.apache.camel.builder.RouteBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Apache Camel Route Builder for Grey Legacy claim integration flows.
 * <p>
 * Apache Camel is the de-facto standard for enterprise integration in
 * large Java shops. It implements the <b>Enterprise Integration Patterns</b>
 * (EIP) from the Hohpe/Woolf book (2003), providing a DSL for message
 * routing, transformation, and mediation.
 * <p>
 * This route builder demonstrates the patterns legacy systems use most:
 * <ul>
 *   <li><b>File Polling</b> — pick up CSV/XML files from a drop directory</li>
 *   <li><b>Content-Based Router</b> — route messages based on file type/content</li>
 *   <li><b>Message Translator</b> — transform between formats (CSV → POJO)</li>
 *   <li><b>Dead Letter Channel</b> — error handling with retry and DLQ</li>
 *   <li><b>Wire Tap</b> — audit logging without affecting the main flow</li>
 *   <li><b>Splitter</b> — break a batch file into individual records</li>
 *   <li><b>JMS Integration</b> — bridge file-based and message-based systems</li>
 *   <li><b>Throttler</b> — rate-limit outbound calls to partner APIs</li>
 *   <li><b>Idempotent Consumer</b> — prevent duplicate processing</li>
 * </ul>
 * <p>
 * In legacy insurance architectures, Camel routes typically bridge:
 * <ul>
 *   <li>Partner file drops (FTP/SFTP/local filesystem) → internal JMS queues</li>
 *   <li>Internal events → outbound SOAP web service calls to reinsurers</li>
 *   <li>Batch files → database import pipelines</li>
 *   <li>Mainframe flat files → modern REST/JSON APIs</li>
 * </ul>
 * <p>
 * Spring integration: This route builder is registered in the Spring
 * context via {@code applicationContext-camel.xml}, which creates a
 * {@code CamelContext} and discovers route builders by type.
 *
 * @see org.apache.camel.builder.RouteBuilder
 */
public class ClaimIntegrationRouteBuilder extends RouteBuilder {

    private static final Logger log = LoggerFactory.getLogger(ClaimIntegrationRouteBuilder.class);

    // --- Route IDs for JMX monitoring and testing ---
    public static final String ROUTE_FILE_INGEST    = "claim-file-ingest";
    public static final String ROUTE_CSV_PROCESSOR  = "claim-csv-processor";
    public static final String ROUTE_XML_PROCESSOR   = "claim-xml-processor";
    public static final String ROUTE_JMS_BRIDGE     = "claim-jms-bridge";
    public static final String ROUTE_OUTBOUND_NOTIFY = "claim-outbound-notify";
    public static final String ROUTE_ERROR_HANDLER  = "claim-error-handler";

    @Override
    public void configure() throws Exception {

        // ================================================================
        // Global Error Handling — Dead Letter Channel Pattern
        // ================================================================
        // Routes that throw exceptions will retry up to 3 times with
        // exponential backoff (1s, 2s, 4s), then move to the DLQ.
        // This mirrors the JMS redelivery policy in applicationContext-jms.xml.

        errorHandler(
            deadLetterChannel("jms:queue:greylegacy.claims.dlq")
                .maximumRedeliveries(3)
                .redeliveryDelay(1000)
                .backOffMultiplier(2)
                .retryAttemptedLogLevel(LoggingLevel.WARN)
                .logExhausted(true)
                .logExhaustedMessageHistory(true)
                .useOriginalMessage()
        );

        // Global exception handler for validation errors — no retry
        onException(IllegalArgumentException.class)
            .handled(true)
            .maximumRedeliveries(0)
            .log(LoggingLevel.ERROR, "Validation error: ${exception.message}")
            .to("jms:queue:greylegacy.claims.validation-errors");

        // ================================================================
        // Route 1: File Ingest — Content-Based Router
        // ================================================================
        // Polls a local directory for partner data files.
        // Uses Content-Based Router to dispatch files by extension.
        // Demonstrates: file component, CBR, move/done/error directories.

        from("file:{{camel.file.inbox}}?include=.*\\.(csv|xml)"
                + "&move=.done/${date:now:yyyyMMdd}/${file:name}"
                + "&moveFailed=.error/${file:name}"
                + "&readLock=changed"
                + "&readLockMinAge=5000"
                + "&sortBy=file:modified"
                + "&maxMessagesPerPoll=10")
            .routeId(ROUTE_FILE_INGEST)
            .log(LoggingLevel.INFO, "Picked up file: ${file:name} (${file:size} bytes)")

            // Wire Tap — send a copy to the audit queue without blocking
            .wireTap("jms:queue:greylegacy.audit.file-events")
                .newExchangeBody(simple("FILE_RECEIVED|${file:name}|${date:now:yyyy-MM-dd HH:mm:ss}"))
            .end()

            // Content-Based Router — dispatch by file type
            .choice()
                .when(header("CamelFileName").endsWith(".csv"))
                    .log("Routing CSV file to CSV processor")
                    .to("direct:process-csv")
                .when(header("CamelFileName").endsWith(".xml"))
                    .log("Routing XML file to XML processor")
                    .to("direct:process-xml")
                .otherwise()
                    .log(LoggingLevel.WARN, "Unknown file type: ${file:name}")
                    .to("jms:queue:greylegacy.claims.unknown-format")
            .end();

        // ================================================================
        // Route 2: CSV Processor — Splitter + Throttler
        // ================================================================
        // Splits a multi-line CSV file into individual claim records,
        // throttles processing to avoid overwhelming the database,
        // and sends each record to the JMS bridge.

        from("direct:process-csv")
            .routeId(ROUTE_CSV_PROCESSOR)
            .log("Processing CSV claim file: ${file:name}")

            // Convert to String for line splitting
            .convertBodyTo(String.class)

            // Splitter — break CSV into individual lines
            .split(body().tokenize("\n"))
                .streaming()  // Process line-by-line, don't load all into memory

                // Skip header row
                .filter(exchangeProperty(Exchange.SPLIT_INDEX).isGreaterThan(0))

                // Throttler — max 50 records per second
                .throttle(50)

                // Set headers for downstream processing
                .setHeader("RecordType", constant("CLAIM_CSV"))
                .setHeader("SourceFile", simple("${file:name}"))
                .setHeader("RecordIndex", simple("${exchangeProperty.CamelSplitIndex}"))

                // Idempotent Consumer — prevent duplicate processing
                // Uses first CSV field (assumed to be claim number) as key
                .idempotentConsumer(
                    simple("${body.substring(0, ${body.indexOf(',')})}"),
                    org.apache.camel.processor.idempotent.MemoryIdempotentRepository
                        .memoryIdempotentRepository(1000)
                )

                .log(LoggingLevel.DEBUG, "Processing CSV record #${exchangeProperty.CamelSplitIndex}")
                .to("direct:jms-bridge")
            .end();

        // ================================================================
        // Route 3: XML Processor — JAXB Unmarshalling
        // ================================================================
        // Processes XML policy/claim files using JAXB data format.

        from("direct:process-xml")
            .routeId(ROUTE_XML_PROCESSOR)
            .log("Processing XML file: ${file:name}")

            // Validate against XSD before processing
            .doTry()
                .to("validator:classpath:xsd/claims-types.xsd")
                .log("XML validation passed")
            .doCatch(Exception.class)
                .log(LoggingLevel.ERROR, "XML validation failed: ${exception.message}")
                .to("jms:queue:greylegacy.claims.validation-errors")
                .stop()
            .end()

            // Set processing headers
            .setHeader("RecordType", constant("CLAIM_XML"))
            .setHeader("SourceFile", simple("${file:name}"))
            .to("direct:jms-bridge");

        // ================================================================
        // Route 4: JMS Bridge — File → Queue Gateway
        // ================================================================
        // Bridges file-based ingest to the existing JMS infrastructure.
        // Enriches the message with metadata before enqueueing.

        from("direct:jms-bridge")
            .routeId(ROUTE_JMS_BRIDGE)
            .setHeader("ProcessedTimestamp", simple("${date:now:yyyy-MM-dd'T'HH:mm:ss.SSSZ}"))
            .setHeader("ProcessingNode", simple("${sys.HOSTNAME}"))

            // Message Translator — convert to the format expected by
            // ClaimEventListener (the existing JMS consumer)
            .process(new Processor() {
                @Override
                public void process(Exchange exchange) throws Exception {
                    String body = exchange.getIn().getBody(String.class);
                    String recordType = exchange.getIn().getHeader("RecordType", String.class);
                    // Wrap in standard envelope for the existing JMS listener
                    String envelope = String.format(
                        "{\"source\":\"CAMEL_FILE_INGEST\",\"type\":\"%s\",\"payload\":\"%s\"}",
                        recordType,
                        body.replace("\"", "\\\"")
                    );
                    exchange.getIn().setBody(envelope);
                }
            })

            .log(LoggingLevel.DEBUG, "Sending to JMS: ${body}")
            .to("jms:queue:greylegacy.claims.inbound?deliveryPersistent=true");

        // ================================================================
        // Route 5: Outbound Notification Route
        // ================================================================
        // Listens on a JMS topic for claim events and notifies external
        // partners via HTTP (REST webhook) or SOAP.
        // Demonstrates: JMS topic consumer, multicast, content enricher.

        from("jms:topic:greylegacy.claims.events"
                + "?durableSubscriptionName=camel-notifier"
                + "&clientId=greylegacy-camel-notifier")
            .routeId(ROUTE_OUTBOUND_NOTIFY)
            .log("Received claim event for notification: ${body}")

            // Rate limit outbound calls to 10/sec
            .throttle(10)

            // Multicast to multiple notification channels in parallel
            .multicast()
                .parallelProcessing()
                .stopOnException()

                // Channel 1: Webhook notification (REST)
                .to("direct:notify-webhook")

                // Channel 2: Audit log
                .to("direct:notify-audit-log")
            .end();

        // ================================================================
        // Route 6: Error Handler Route
        // ================================================================
        // Processes messages that land in the DLQ, logs them, and
        // optionally alerts operations via email.

        from("jms:queue:greylegacy.claims.dlq")
            .routeId(ROUTE_ERROR_HANDLER)
            .log(LoggingLevel.ERROR,
                "DLQ message received — original route: ${header.CamelFailureRouteId}, "
                + "error: ${header.CamelExceptionCaught}")

            // Filter — only alert on high-severity errors
            .filter(header("CamelExceptionCaught").contains("SQLException"))
                .log(LoggingLevel.ERROR, "DATABASE ERROR in DLQ — manual intervention required")
                // In production, this would route to an email/PagerDuty endpoint
            .end();

        // --- Sub-routes for notification channels ---

        from("direct:notify-webhook")
            .log(LoggingLevel.DEBUG, "Would POST webhook for claim event (disabled in dev)");

        from("direct:notify-audit-log")
            .log("Audit: claim event notification processed at ${date:now:yyyy-MM-dd HH:mm:ss}");
    }
}
