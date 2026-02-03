/**
 * @file rest_client.h
 * @brief RESTful API Client Stub
 * 
 * INDUSTRY RELEVANCE:
 * REST APIs are the backbone of IoT cloud connectivity. Devices must interact
 * with cloud services for data upload, configuration, firmware updates, and
 * remote control. AWS IoT, Azure IoT Hub, and Google Cloud IoT all expose
 * REST interfaces. The IoT cloud platform market exceeds $15B annually.
 * 
 * WHY THIS MATTERS:
 * - HTTP/HTTPS client implementation for embedded
 * - JSON serialization/parsing
 * - Authentication (API keys, OAuth, JWT)
 * - Request retry and error handling
 * - Connection pooling for efficiency
 * 
 * GREY FIRMWARE DEMONSTRATES:
 * - Resource-constrained HTTP client
 * - Request/response handling
 * - Authentication patterns
 * - Error recovery strategies
 */

#ifndef GF_REST_CLIENT_H
#define GF_REST_CLIENT_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_REST_MAX_URL_LEN         256     /* Maximum URL length */
#define GF_REST_MAX_HEADERS         16      /* Maximum headers */
#define GF_REST_MAX_BODY_LEN        4096    /* Maximum request/response body */
#define GF_REST_MAX_CONNECTIONS     4       /* Connection pool size */
#define GF_REST_HEADER_NAME_LEN     32      /* Header name max length */
#define GF_REST_HEADER_VALUE_LEN    128     /* Header value max length */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/* HTTP methods */
typedef enum {
    GF_REST_GET = 0,
    GF_REST_POST,
    GF_REST_PUT,
    GF_REST_PATCH,
    GF_REST_DELETE,
    GF_REST_HEAD,
    GF_REST_OPTIONS
} gf_rest_method_t;

/* Content types */
typedef enum {
    GF_REST_CONTENT_JSON = 0,       /* application/json */
    GF_REST_CONTENT_FORM,           /* application/x-www-form-urlencoded */
    GF_REST_CONTENT_BINARY,         /* application/octet-stream */
    GF_REST_CONTENT_TEXT,           /* text/plain */
    GF_REST_CONTENT_XML             /* application/xml */
} gf_rest_content_t;

/* Authentication type */
typedef enum {
    GF_REST_AUTH_NONE = 0,
    GF_REST_AUTH_BASIC,             /* Basic username:password */
    GF_REST_AUTH_BEARER,            /* Bearer token */
    GF_REST_AUTH_API_KEY,           /* API key header */
    GF_REST_AUTH_AWS_SIG4,          /* AWS Signature v4 */
    GF_REST_AUTH_OAUTH2             /* OAuth 2.0 */
} gf_rest_auth_t;

/* Request status */
typedef enum {
    GF_REST_STATUS_IDLE = 0,
    GF_REST_STATUS_CONNECTING,
    GF_REST_STATUS_SENDING,
    GF_REST_STATUS_WAITING,
    GF_REST_STATUS_RECEIVING,
    GF_REST_STATUS_COMPLETE,
    GF_REST_STATUS_ERROR,
    GF_REST_STATUS_TIMEOUT
} gf_rest_status_t;

/* HTTP header */
typedef struct {
    char    name[GF_REST_HEADER_NAME_LEN];
    char    value[GF_REST_HEADER_VALUE_LEN];
} gf_rest_header_t;

/* Authentication configuration */
typedef struct {
    gf_rest_auth_t  type;
    char            username[32];       /* For basic auth */
    char            password[64];       /* For basic auth */
    char            token[256];         /* For bearer/API key */
    char            api_key_header[32]; /* Header name for API key */
} gf_rest_auth_config_t;

/* Request configuration */
typedef struct {
    gf_rest_method_t    method;
    char                url[GF_REST_MAX_URL_LEN];
    gf_rest_content_t   content_type;
    gf_rest_header_t    headers[GF_REST_MAX_HEADERS];
    int                 header_count;
    const uint8_t      *body;
    uint16_t            body_len;
    uint16_t            timeout_ms;
    uint8_t             max_retries;
    bool                follow_redirects;
} gf_rest_request_t;

/* Response structure */
typedef struct {
    uint16_t            status_code;        /* HTTP status code */
    gf_rest_status_t    request_status;     /* Request completion status */
    gf_rest_header_t    headers[GF_REST_MAX_HEADERS];
    int                 header_count;
    uint8_t             body[GF_REST_MAX_BODY_LEN];
    uint16_t            body_len;
    uint32_t            duration_ms;        /* Request duration */
    uint8_t             retry_count;        /* Retries performed */
    char                error_message[64];  /* Error description */
} gf_rest_response_t;

/* Client configuration */
typedef struct {
    char                base_url[GF_REST_MAX_URL_LEN];  /* Base URL for all requests */
    gf_rest_auth_config_t auth;
    uint16_t            default_timeout_ms;
    uint8_t             default_retries;
    bool                verify_ssl;
    char                user_agent[64];
} gf_rest_client_config_t;

/* Async callback */
typedef void (*gf_rest_callback_t)(const gf_rest_response_t *response, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize REST client
 * @param config Client configuration (NULL for defaults)
 */
int gf_rest_init(const gf_rest_client_config_t *config);

/**
 * @brief Set base URL for relative paths
 */
int gf_rest_set_base_url(const char *base_url);

/**
 * @brief Set authentication credentials
 */
int gf_rest_set_auth(const gf_rest_auth_config_t *auth);

/**
 * @brief Set default header for all requests
 */
int gf_rest_set_default_header(const char *name, const char *value);

/**
 * @brief Perform synchronous request
 * @param request Request configuration
 * @param response Output response
 * @return 0 on success, error code on failure
 */
int gf_rest_request(const gf_rest_request_t *request, 
                    gf_rest_response_t *response);

/**
 * @brief Perform asynchronous request
 * @param request Request configuration
 * @param callback Completion callback
 * @param ctx User context
 * @return Request handle or error
 */
int gf_rest_request_async(const gf_rest_request_t *request,
                          gf_rest_callback_t callback, void *ctx);

/**
 * @brief Cancel pending async request
 */
int gf_rest_cancel(int handle);

/**
 * @brief Convenience: GET request
 */
int gf_rest_get(const char *url, gf_rest_response_t *response);

/**
 * @brief Convenience: POST JSON
 */
int gf_rest_post_json(const char *url, const char *json_body,
                      gf_rest_response_t *response);

/**
 * @brief Convenience: PUT JSON
 */
int gf_rest_put_json(const char *url, const char *json_body,
                     gf_rest_response_t *response);

/**
 * @brief Convenience: DELETE
 */
int gf_rest_delete(const char *url, gf_rest_response_t *response);

/**
 * @brief Get header value from response
 * @return Header value or NULL
 */
const char *gf_rest_get_header(const gf_rest_response_t *response,
                                const char *name);

/**
 * @brief Check if response indicates success (2xx)
 */
bool gf_rest_is_success(const gf_rest_response_t *response);

/**
 * @brief Get status description
 */
const char *gf_rest_status_string(uint16_t status_code);

/**
 * @brief URL encode a string
 * @param input Input string
 * @param output Output buffer
 * @param output_len Buffer size
 * @return Encoded length
 */
int gf_rest_url_encode(const char *input, char *output, int output_len);

/**
 * @brief URL decode a string
 */
int gf_rest_url_decode(const char *input, char *output, int output_len);

/**
 * @brief Process pending async requests (call from main loop)
 */
int gf_rest_process(void);

/**
 * @brief Get connection pool status
 * @param active Output: active connections
 * @param available Output: available connections
 */
int gf_rest_pool_status(int *active, int *available);

#endif /* GF_REST_CLIENT_H */
