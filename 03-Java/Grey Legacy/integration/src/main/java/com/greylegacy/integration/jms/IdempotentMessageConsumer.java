package com.greylegacy.integration.jms;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Idempotent message consumer that prevents duplicate processing of JMS
 * messages.
 *
 * <p>In enterprise messaging, exactly-once delivery semantics are difficult
 * to guarantee.  Network failures, broker failovers, and transaction
 * rollbacks can all cause message redelivery.  The idempotent consumer
 * pattern ensures that processing a message multiple times produces the
 * same result as processing it once.</p>
 *
 * <p>This implementation uses an in-memory LRU cache of processed message
 * IDs.  In production, this would be backed by a database table or
 * distributed cache (Redis, Hazelcast) for cluster-wide idempotency.</p>
 *
 * <p>Usage pattern:</p>
 * <pre>
 *   if (idempotentConsumer.isDuplicate(messageId)) {
 *       LOG.warn("Duplicate message detected: {}", messageId);
 *       return; // skip processing
 *   }
 *   // ... process message ...
 *   idempotentConsumer.markProcessed(messageId);
 * </pre>
 *
 * @author Grey Legacy Integration Team
 * @since 1.0
 */
@Component("idempotentConsumer")
public class IdempotentMessageConsumer {

    private static final Logger LOG = LoggerFactory.getLogger(IdempotentMessageConsumer.class);

    /**
     * Maximum number of message IDs to retain in the LRU cache.
     * Oldest entries are evicted when the limit is reached.
     */
    private static final int MAX_CACHE_SIZE = 10_000;

    /**
     * LRU cache of processed message IDs.
     *
     * <p>Uses a synchronized {@link LinkedHashMap} with access-order
     * eviction.  In production, replace with a JDBC-backed or
     * distributed implementation.</p>
     */
    private final Map<String, Long> processedMessageIds =
            Collections.synchronizedMap(new LinkedHashMap<String, Long>(
                    MAX_CACHE_SIZE + 1, 0.75f, true) {
                @Override
                protected boolean removeEldestEntry(Map.Entry<String, Long> eldest) {
                    boolean evict = size() > MAX_CACHE_SIZE;
                    if (evict) {
                        LOG.trace("Evicting oldest message ID from idempotent cache: {}",
                                eldest.getKey());
                    }
                    return evict;
                }
            });

    /** Tracks message IDs that were explicitly replayed (audit trail). */
    private final Set<String> replayedMessageIds = ConcurrentHashMap.newKeySet();

    // -------------------------------------------------------------------------
    // Public API
    // -------------------------------------------------------------------------

    /**
     * Checks whether a message with the given ID has already been processed.
     *
     * @param messageId the JMS message ID or business correlation ID
     * @return {@code true} if the message was already processed
     */
    public boolean isDuplicate(String messageId) {
        if (messageId == null) {
            LOG.warn("Null message ID — cannot check for duplicates. Treating as unique.");
            return false;
        }
        boolean duplicate = processedMessageIds.containsKey(messageId);
        if (duplicate) {
            LOG.info("Duplicate message detected: id={}, originally processed at={}",
                    messageId, processedMessageIds.get(messageId));
        }
        return duplicate;
    }

    /**
     * Records a message as successfully processed.
     *
     * @param messageId the JMS message ID or business correlation ID
     */
    public void markProcessed(String messageId) {
        if (messageId != null) {
            processedMessageIds.put(messageId, System.currentTimeMillis());
            LOG.debug("Message marked as processed: id={}", messageId);
        }
    }

    /**
     * Removes a message from the processed cache, allowing it to be
     * reprocessed.  Used for message replay scenarios.
     *
     * @param messageId the message ID to allow reprocessing of
     * @return {@code true} if the message was in the cache
     */
    public boolean allowReplay(String messageId) {
        if (messageId == null) return false;
        Long removed = processedMessageIds.remove(messageId);
        if (removed != null) {
            replayedMessageIds.add(messageId);
            LOG.info("Message {} cleared from idempotent cache for replay "
                    + "(originally processed at {})", messageId, removed);
            return true;
        }
        return false;
    }

    /**
     * Returns the number of message IDs currently in the processed cache.
     */
    public int getCacheSize() {
        return processedMessageIds.size();
    }

    /**
     * Returns the number of messages that have been replayed.
     */
    public int getReplayCount() {
        return replayedMessageIds.size();
    }

    /**
     * Clears the entire processed cache.  Use with caution — this
     * removes all duplicate protection.
     */
    public void clearCache() {
        int size = processedMessageIds.size();
        processedMessageIds.clear();
        LOG.warn("Idempotent message cache cleared ({} entries removed)", size);
    }
}
