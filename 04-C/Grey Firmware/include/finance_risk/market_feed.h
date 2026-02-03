/**
 * @file market_feed.h
 * @brief Market Data Feed Driver for Edge Finance Risk Systems
 * 
 * INDUSTRY RELEVANCE:
 * Ultra-low-latency market data processing is essential for trading systems
 * and risk management. This driver enables:
 * - High-frequency price feed ingestion
 * - Order book reconstruction
 * - Market microstructure analysis
 * - Latency-sensitive event processing
 * 
 * Target applications: Algorithmic trading, risk management systems, market
 * surveillance, regulatory reporting, quantitative research platforms.
 * 
 * Standards: FIX Protocol (5.0SP2), ITCH (NASDAQ), OUCH (order entry),
 *            MiFID II (regulatory), Reg SCI (systems compliance)
 */

#ifndef GF_MARKET_FEED_H
#define GF_MARKET_FEED_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Market Feed Types                                                          */
/*===========================================================================*/

typedef enum {
    EXCHANGE_NYSE,
    EXCHANGE_NASDAQ,
    EXCHANGE_CME,
    EXCHANGE_ICE,
    EXCHANGE_CBOE,
    EXCHANGE_LSE,
    EXCHANGE_EUREX,
    EXCHANGE_CUSTOM
} exchange_t;

typedef enum {
    FEED_LEVEL1,            /* Top of book (BBO) */
    FEED_LEVEL2,            /* Market depth */
    FEED_LEVEL3,            /* Full order book */
    FEED_TRADES,            /* Trade tick data */
    FEED_IMBALANCE          /* Auction imbalance */
} feed_level_t;

typedef enum {
    MSG_QUOTE,              /* Quote update */
    MSG_TRADE,              /* Trade execution */
    MSG_ORDER_ADD,          /* Order added */
    MSG_ORDER_MODIFY,       /* Order modified */
    MSG_ORDER_DELETE,       /* Order cancelled */
    MSG_AUCTION_INFO,       /* Auction information */
    MSG_HALT                /* Trading halt */
} message_type_t;

typedef struct {
    char symbol[12];        /* Ticker symbol */
    exchange_t exchange;
    uint64_t timestamp_ns;  /* Nanosecond timestamp */
    double bid_price;
    double ask_price;
    uint32_t bid_size;
    uint32_t ask_size;
    uint16_t bid_exchange;
    uint16_t ask_exchange;
} quote_t;

typedef struct {
    char symbol[12];
    exchange_t exchange;
    uint64_t timestamp_ns;
    double price;
    uint32_t size;
    char side;              /* 'B'uy or 'S'ell */
    uint64_t trade_id;
} trade_t;

typedef struct {
    exchange_t exchange;
    feed_level_t level;
    char* symbols;          /* Comma-separated symbol list */
    uint16_t symbol_count;
    bool include_trades;
    uint32_t max_depth;     /* Max order book depth */
} feed_config_t;

typedef struct {
    uint64_t messages_received;
    uint64_t bytes_received;
    uint64_t latency_min_ns;
    uint64_t latency_max_ns;
    uint64_t latency_avg_ns;
    uint32_t gaps_detected;
    uint32_t recovery_count;
} feed_stats_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int market_feed_init(const feed_config_t* config);
int market_feed_shutdown(void);

int feed_connect(exchange_t exchange);
int feed_disconnect(exchange_t exchange);
int feed_subscribe(const char* symbol);
int feed_unsubscribe(const char* symbol);

int feed_get_quote(const char* symbol, quote_t* quote);
int feed_get_trade(const char* symbol, trade_t* trade);
int feed_get_depth(const char* symbol, quote_t* bids, quote_t* asks,
                  uint16_t max_levels, uint16_t* count);

int feed_get_stats(feed_stats_t* stats);
uint64_t feed_get_latency_ns(void);

bool feed_is_connected(exchange_t exchange);
bool feed_is_market_open(exchange_t exchange);

#endif /* GF_MARKET_FEED_H */
