/**
 * @file market_sensor.h
 * @brief Market Data Sensor Interface for Quantitative Trading Systems
 * 
 * INDUSTRY RELEVANCE:
 * High-frequency trading (HFT) and quantitative finance systems require
 * ultra-low-latency market data acquisition. This driver interfaces with
 * market data feeds, providing normalized tick data, order book snapshots,
 * and trade event streams for algorithmic trading engines.
 * 
 * Applications:
 * - High-frequency trading firms
 * - Quantitative hedge funds
 * - Market making systems
 * - Exchange co-location infrastructure
 * - Algorithmic execution engines
 * 
 * Standards: FIX Protocol, FAST Protocol, ITCH/OUCH
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_MARKET_SENSOR_H
#define GF_MARKET_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define MARKET_MAX_FEEDS            16      /* Maximum data feeds */
#define MARKET_MAX_SYMBOLS          1024    /* Maximum tracked symbols */
#define MARKET_BOOK_DEPTH           10      /* Order book levels */
#define MARKET_TICK_BUFFER          10000   /* Tick buffer size */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Market data feed type */
typedef enum {
    FEED_LEVEL1,                /* Top of book */
    FEED_LEVEL2,                /* Full order book */
    FEED_TRADES,                /* Trade prints */
    FEED_IMBALANCE,             /* Auction imbalance */
    FEED_NEWS,                  /* News events */
    FEED_INDEX                  /* Index values */
} feed_type_t;

/** Asset class */
typedef enum {
    ASSET_EQUITY,
    ASSET_OPTION,
    ASSET_FUTURE,
    ASSET_FOREX,
    ASSET_CRYPTO,
    ASSET_FIXED_INCOME,
    ASSET_COMMODITY
} asset_class_t;

/** Order side */
typedef enum {
    SIDE_BID,
    SIDE_ASK
} order_side_t;

/** Tick type */
typedef enum {
    TICK_TRADE,
    TICK_BID,
    TICK_ASK,
    TICK_OPEN,
    TICK_HIGH,
    TICK_LOW,
    TICK_CLOSE,
    TICK_VOLUME
} tick_type_t;

/** Price level */
typedef struct {
    int64_t price;              /* Price in fixed-point (8 decimals) */
    uint64_t quantity;
    uint32_t order_count;
} price_level_t;

/** Order book snapshot */
typedef struct {
    uint32_t symbol_id;
    char symbol[16];
    price_level_t bids[MARKET_BOOK_DEPTH];
    price_level_t asks[MARKET_BOOK_DEPTH];
    uint64_t timestamp_ns;
    uint64_t sequence;
    bool valid;
} order_book_t;

/** Market tick */
typedef struct {
    uint32_t symbol_id;
    tick_type_t type;
    int64_t price;
    uint64_t quantity;
    uint64_t timestamp_ns;
    uint64_t sequence;
} market_tick_t;

/** Trade event */
typedef struct {
    uint32_t symbol_id;
    int64_t price;
    uint64_t quantity;
    order_side_t aggressor;
    uint64_t timestamp_ns;
    uint64_t trade_id;
} trade_event_t;

/** Feed statistics */
typedef struct {
    uint8_t feed_id;
    uint64_t messages_received;
    uint64_t bytes_received;
    uint32_t avg_latency_ns;
    uint32_t max_latency_ns;
    uint32_t gap_count;
    bool connected;
} feed_stats_t;

/** Feed configuration */
typedef struct {
    uint8_t feed_id;
    feed_type_t type;
    const char *host;
    uint16_t port;
    const char *username;
    bool multicast;
} feed_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int market_sensor_init(void);
void market_sensor_shutdown(void);

int market_add_feed(const feed_config_t *config);
int market_subscribe(uint8_t feed_id, const char *symbol);
int market_unsubscribe(uint8_t feed_id, const char *symbol);

int market_get_book(const char *symbol, order_book_t *book);
int market_get_ticks(const char *symbol, market_tick_t *ticks, uint32_t max_count);
int market_get_trades(const char *symbol, trade_event_t *trades, uint32_t max_count);

int64_t market_get_last_price(const char *symbol);
int64_t market_get_bid(const char *symbol);
int64_t market_get_ask(const char *symbol);

int market_get_feed_stats(uint8_t feed_id, feed_stats_t *stats);

void market_sensor_update(uint32_t elapsed_us);

#endif /* GF_MARKET_SENSOR_H */
