/**
 * @file http_client.h
 * @brief HTTP/1.1 Client with Retry and Timeout Logic
 * 
 * INDUSTRY RELEVANCE:
 *   HTTP is the backbone of IoT and cloud connectivity:
 *   - REST APIs for cloud services (AWS IoT, Azure, Google Cloud)
 *   - OTA firmware updates over HTTPS
 *   - Telemetry data upload to web backends
 *   - Configuration fetching from management portals
 *   - Certificate/credential provisioning
 * 
 * This implementation provides:
 *   - HTTP/1.1 with Keep-Alive support
 *   - Common methods (GET, POST, PUT, DELETE, PATCH)
 *   - Chunked transfer encoding
 *   - Automatic retry with exponential backoff
 *   - Connection timeout and read timeout handling
 *   - Header parsing and response callbacks
 *   - Basic and Bearer token authentication
 *   - Optional TLS support placeholder
 */

#ifndef GF_HTTP_CLIENT_H
#define GF_HTTP_CLIENT_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "net/tcp_ip.h"

/*===========================================================================*/
/* Constants and Limits                                                       */
/*===========================================================================*/

#define GF_HTTP_MAX_HEADERS         16
#define GF_HTTP_MAX_HEADER_VALUE    256
#define GF_HTTP_MAX_URL_LEN         512
#define GF_HTTP_DEFAULT_PORT        80
#define GF_HTTPS_DEFAULT_PORT       443
#define GF_HTTP_MAX_REDIRECTS       5
#define GF_HTTP_DEFAULT_TIMEOUT_MS  30000
#define GF_HTTP_USER_AGENT          "GreyFirmware/1.0"

/*===========================================================================*/
/* HTTP Methods                                                               */
/*===========================================================================*/

typedef enum {
    GF_HTTP_GET = 0,
    GF_HTTP_POST,
    GF_HTTP_PUT,
    GF_HTTP_DELETE,
    GF_HTTP_PATCH,
    GF_HTTP_HEAD,
    GF_HTTP_OPTIONS
} gf_http_method_t;

/*===========================================================================*/
/* HTTP Status Codes                                                          */
/*===========================================================================*/

typedef enum {
    /* Informational */
    GF_HTTP_100_CONTINUE            = 100,
    GF_HTTP_101_SWITCHING           = 101,
    
    /* Success */
    GF_HTTP_200_OK                  = 200,
    GF_HTTP_201_CREATED             = 201,
    GF_HTTP_202_ACCEPTED            = 202,
    GF_HTTP_204_NO_CONTENT          = 204,
    
    /* Redirection */
    GF_HTTP_301_MOVED               = 301,
    GF_HTTP_302_FOUND               = 302,
    GF_HTTP_304_NOT_MODIFIED        = 304,
    GF_HTTP_307_TEMP_REDIRECT       = 307,
    GF_HTTP_308_PERM_REDIRECT       = 308,
    
    /* Client Error */
    GF_HTTP_400_BAD_REQUEST         = 400,
    GF_HTTP_401_UNAUTHORIZED        = 401,
    GF_HTTP_403_FORBIDDEN           = 403,
    GF_HTTP_404_NOT_FOUND           = 404,
    GF_HTTP_405_NOT_ALLOWED         = 405,
    GF_HTTP_408_TIMEOUT             = 408,
    GF_HTTP_429_TOO_MANY            = 429,
    
    /* Server Error */
    GF_HTTP_500_INTERNAL            = 500,
    GF_HTTP_502_BAD_GATEWAY         = 502,
    GF_HTTP_503_UNAVAILABLE         = 503,
    GF_HTTP_504_GATEWAY_TIMEOUT     = 504
} gf_http_status_t;

/*===========================================================================*/
/* Error Codes                                                                */
/*===========================================================================*/

typedef enum {
    GF_HTTP_OK = 0,
    GF_HTTP_ERR_GENERAL = -1,
    GF_HTTP_ERR_MEMORY = -2,
    GF_HTTP_ERR_DNS = -3,
    GF_HTTP_ERR_CONNECT = -4,
    GF_HTTP_ERR_TIMEOUT = -5,
    GF_HTTP_ERR_WRITE = -6,
    GF_HTTP_ERR_READ = -7,
    GF_HTTP_ERR_PARSE = -8,
    GF_HTTP_ERR_REDIRECT_LIMIT = -9,
    GF_HTTP_ERR_TLS = -10,
    GF_HTTP_ERR_BUFFER_FULL = -11,
    GF_HTTP_ERR_RETRY_EXHAUSTED = -12,
    GF_HTTP_ERR_INVALID_URL = -13
} gf_http_err_t;

/*===========================================================================*/
/* HTTP Header                                                                */
/*===========================================================================*/

typedef struct {
    const char     *name;
    char            value[GF_HTTP_MAX_HEADER_VALUE];
} gf_http_header_t;

/*===========================================================================*/
/* URL Components                                                             */
/*===========================================================================*/

typedef struct {
    char            scheme[8];          /* http or https */
    char            host[128];
    uint16_t        port;
    char            path[256];
    char            query[128];
} gf_http_url_t;

/*===========================================================================*/
/* Request Configuration                                                      */
/*===========================================================================*/

typedef struct {
    gf_http_method_t    method;
    const char         *url;
    const gf_http_header_t *headers;
    uint8_t             header_count;
    const void         *body;
    size_t              body_len;
    const char         *content_type;
    
    /* Timeout settings */
    uint32_t            connect_timeout_ms;
    uint32_t            read_timeout_ms;
    uint32_t            total_timeout_ms;
    
    /* Retry settings */
    uint8_t             max_retries;
    uint32_t            retry_base_delay_ms;    /* Base delay for backoff */
    bool                retry_on_5xx;           /* Retry on server errors */
    bool                retry_on_timeout;       /* Retry on timeout */
    
    /* Connection settings */
    bool                keep_alive;
    bool                follow_redirects;
    uint8_t             max_redirects;
    
    /* Authentication */
    const char         *auth_basic;             /* username:password */
    const char         *auth_bearer;            /* Bearer token */
} gf_http_request_t;

/*===========================================================================*/
/* Response Structure                                                         */
/*===========================================================================*/

typedef struct {
    gf_http_status_t    status_code;
    char                status_text[32];
    gf_http_header_t    headers[GF_HTTP_MAX_HEADERS];
    uint8_t             header_count;
    uint32_t            content_length;
    bool                chunked;
    bool                keep_alive;
    
    /* Response body handling */
    void               *body;
    size_t              body_len;
    size_t              body_capacity;
    
    /* Timing info */
    uint32_t            dns_time_ms;
    uint32_t            connect_time_ms;
    uint32_t            total_time_ms;
    
    /* Retry info */
    uint8_t             retry_count;
} gf_http_response_t;

/*===========================================================================*/
/* Streaming Response Callbacks                                               */
/*===========================================================================*/

/**
 * @brief Header callback (called for each header)
 * @return true to continue, false to abort
 */
typedef bool (*gf_http_header_cb_t)(const char *name, 
                                     const char *value,
                                     void *user_data);

/**
 * @brief Body callback (called with body chunks)
 * @return Bytes consumed, -1 on error
 */
typedef int (*gf_http_body_cb_t)(const void *data, 
                                  size_t len,
                                  void *user_data);

/**
 * @brief Progress callback
 * @param bytes_received Total bytes received so far
 * @param content_length Total expected bytes (-1 if unknown)
 */
typedef void (*gf_http_progress_cb_t)(size_t bytes_received,
                                       int32_t content_length,
                                       void *user_data);

typedef struct {
    gf_http_header_cb_t     on_header;
    gf_http_body_cb_t       on_body;
    gf_http_progress_cb_t   on_progress;
    void                   *user_data;
} gf_http_callbacks_t;

/*===========================================================================*/
/* Client Configuration                                                       */
/*===========================================================================*/

typedef struct {
    size_t              rx_buffer_size;         /* Default: 2048 */
    size_t              tx_buffer_size;         /* Default: 1024 */
    uint32_t            default_timeout_ms;     /* Default: 30000 */
    bool                enable_keepalive;       /* Connection pooling */
    uint8_t             max_connections;        /* Connection pool size */
    const char         *default_user_agent;
} gf_http_client_config_t;

/*===========================================================================*/
/* Connection Info (for Keep-Alive)                                           */
/*===========================================================================*/

typedef struct {
    gf_socket_t         socket;
    char                host[128];
    uint16_t            port;
    bool                is_https;
    bool                in_use;
    uint32_t            last_used;
    uint32_t            requests_made;
} gf_http_connection_t;

/*===========================================================================*/
/* Client Statistics                                                          */
/*===========================================================================*/

typedef struct {
    uint32_t            requests_made;
    uint32_t            requests_success;
    uint32_t            requests_failed;
    uint32_t            bytes_sent;
    uint32_t            bytes_received;
    uint32_t            retries_total;
    uint32_t            dns_failures;
    uint32_t            connect_failures;
    uint32_t            timeouts;
    uint32_t            connection_reuses;
} gf_http_stats_t;

/*===========================================================================*/
/* HTTP Client API                                                            */
/*===========================================================================*/

/**
 * @brief Initialize HTTP client
 * @param config Client configuration (NULL for defaults)
 * @return 0 on success
 */
int gf_http_init(const gf_http_client_config_t *config);

/**
 * @brief Shutdown HTTP client
 */
void gf_http_shutdown(void);

/**
 * @brief Execute HTTP request (synchronous, buffers full response)
 * @param request Request configuration
 * @param response Output response structure
 * @return HTTP error code
 */
gf_http_err_t gf_http_request(const gf_http_request_t *request,
                               gf_http_response_t *response);

/**
 * @brief Execute HTTP request with streaming callbacks
 * @param request Request configuration
 * @param callbacks Streaming callbacks
 * @param response Basic response info (may have NULL body)
 * @return HTTP error code
 */
gf_http_err_t gf_http_request_stream(const gf_http_request_t *request,
                                      const gf_http_callbacks_t *callbacks,
                                      gf_http_response_t *response);

/*===========================================================================*/
/* Convenience Methods                                                        */
/*===========================================================================*/

/**
 * @brief Simple GET request
 */
gf_http_err_t gf_http_get(const char *url, 
                           gf_http_response_t *response,
                           uint32_t timeout_ms);

/**
 * @brief Simple POST request with JSON body
 */
gf_http_err_t gf_http_post_json(const char *url,
                                 const char *json_body,
                                 gf_http_response_t *response,
                                 uint32_t timeout_ms);

/**
 * @brief Simple PUT request
 */
gf_http_err_t gf_http_put(const char *url,
                           const void *body,
                           size_t body_len,
                           const char *content_type,
                           gf_http_response_t *response,
                           uint32_t timeout_ms);

/**
 * @brief Simple DELETE request
 */
gf_http_err_t gf_http_delete(const char *url,
                              gf_http_response_t *response,
                              uint32_t timeout_ms);

/*===========================================================================*/
/* Response Management                                                        */
/*===========================================================================*/

/**
 * @brief Allocate response body buffer
 */
int gf_http_response_alloc_body(gf_http_response_t *response, size_t size);

/**
 * @brief Free response resources
 */
void gf_http_response_free(gf_http_response_t *response);

/**
 * @brief Get header value from response
 */
const char *gf_http_response_get_header(const gf_http_response_t *response,
                                         const char *name);

/**
 * @brief Check if status code indicates success (2xx)
 */
bool gf_http_status_is_success(gf_http_status_t status);

/**
 * @brief Check if status code indicates redirect (3xx)
 */
bool gf_http_status_is_redirect(gf_http_status_t status);

/**
 * @brief Check if request should be retried for this status
 */
bool gf_http_status_should_retry(gf_http_status_t status);

/*===========================================================================*/
/* URL Parsing                                                                */
/*===========================================================================*/

/**
 * @brief Parse URL into components
 */
int gf_http_parse_url(const char *url, gf_http_url_t *parsed);

/**
 * @brief URL encode string
 */
int gf_http_url_encode(const char *input, char *output, size_t output_len);

/**
 * @brief URL decode string
 */
int gf_http_url_decode(const char *input, char *output, size_t output_len);

/*===========================================================================*/
/* Connection Pool Management                                                 */
/*===========================================================================*/

/**
 * @brief Get connection from pool (or create new)
 */
gf_http_connection_t *gf_http_get_connection(const char *host, 
                                              uint16_t port,
                                              bool is_https);

/**
 * @brief Return connection to pool
 */
void gf_http_release_connection(gf_http_connection_t *conn, bool reusable);

/**
 * @brief Close all pooled connections
 */
void gf_http_close_all_connections(void);

/*===========================================================================*/
/* Statistics                                                                 */
/*===========================================================================*/

/**
 * @brief Get client statistics
 */
void gf_http_get_stats(gf_http_stats_t *stats);

/**
 * @brief Reset statistics
 */
void gf_http_reset_stats(void);

/*===========================================================================*/
/* Retry Logic Helpers                                                        */
/*===========================================================================*/

/**
 * @brief Calculate retry delay with exponential backoff
 * @param attempt Retry attempt number (0-based)
 * @param base_delay_ms Base delay in milliseconds
 * @param max_delay_ms Maximum delay cap
 * @param jitter_percent Random jitter percentage (0-50)
 * @return Delay in milliseconds
 */
uint32_t gf_http_calc_retry_delay(uint8_t attempt,
                                   uint32_t base_delay_ms,
                                   uint32_t max_delay_ms,
                                   uint8_t jitter_percent);

#endif /* GF_HTTP_CLIENT_H */
