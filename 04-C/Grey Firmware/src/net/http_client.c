/**
 * @file http_client.c
 * @brief HTTP/1.1 Client Implementation with Retry and Timeout Logic
 */

#include "net/http_client.h"
#include "net/dns.h"
#include "core/error_handler.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>  /* For strcasecmp on POSIX */

/*===========================================================================*/
/* Internal Constants                                                         */
/*===========================================================================*/

#define HTTP_MAX_LINE_LEN       1024
#define HTTP_POOL_SIZE          4

/*===========================================================================*/
/* Static State                                                               */
/*===========================================================================*/

static gf_http_client_config_t s_config;
static gf_http_stats_t s_stats;
static gf_http_connection_t s_conn_pool[HTTP_POOL_SIZE];
static uint8_t s_rx_buffer[2048];
static uint8_t s_tx_buffer[1024];
static bool s_initialized;
static uint32_t s_current_time;  /* Stub: would use HAL timer */

/*===========================================================================*/
/* Helper Functions                                                           */
/*===========================================================================*/

static const char *method_to_string(gf_http_method_t method)
{
    switch (method) {
        case GF_HTTP_GET:     return "GET";
        case GF_HTTP_POST:    return "POST";
        case GF_HTTP_PUT:     return "PUT";
        case GF_HTTP_DELETE:  return "DELETE";
        case GF_HTTP_PATCH:   return "PATCH";
        case GF_HTTP_HEAD:    return "HEAD";
        case GF_HTTP_OPTIONS: return "OPTIONS";
        default:              return "GET";
    }
}

static int parse_status_line(const char *line, gf_http_response_t *response) __attribute__((unused));
static int parse_status_line(const char *line, gf_http_response_t *response)
{
    /* Parse "HTTP/1.1 200 OK" */
    int major, minor, status;
    char status_text[32] = {0};
    
    if (sscanf(line, "HTTP/%d.%d %d %31[^\r\n]", 
               &major, &minor, &status, status_text) < 3) {
        return -1;
    }
    
    response->status_code = (gf_http_status_t)status;
    strncpy(response->status_text, status_text, sizeof(response->status_text) - 1);
    
    return 0;
}

static int parse_header_line(const char *line, gf_http_response_t *response) __attribute__((unused));
static int parse_header_line(const char *line, gf_http_response_t *response)
{
    if (response->header_count >= GF_HTTP_MAX_HEADERS) {
        return 0;  /* Ignore extra headers */
    }
    
    const char *colon = strchr(line, ':');
    if (!colon) {
        return -1;
    }
    
    /* Extract header name */
    size_t name_len = colon - line;
    char name[64] = {0};
    if (name_len >= sizeof(name)) {
        name_len = sizeof(name) - 1;
    }
    memcpy(name, line, name_len);
    
    /* Skip ": " and extract value */
    const char *value = colon + 1;
    while (*value == ' ') value++;
    
    /* Store header */
    gf_http_header_t *hdr = &response->headers[response->header_count];
    hdr->name = NULL;  /* Would need to strdup in real implementation */
    strncpy(hdr->value, value, GF_HTTP_MAX_HEADER_VALUE - 1);
    
    /* Parse Content-Length */
    if (strcasecmp(name, "Content-Length") == 0) {
        response->content_length = (uint32_t)atoi(value);
    } else if (strcasecmp(name, "Transfer-Encoding") == 0) {
        if (strstr(value, "chunked")) {
            response->chunked = true;
        }
    } else if (strcasecmp(name, "Connection") == 0) {
        if (strstr(value, "keep-alive")) {
            response->keep_alive = true;
        }
    }
    
    response->header_count++;
    return 0;
}


static int build_request(const gf_http_request_t *request,
                          const gf_http_url_t *url,
                          char *buf, size_t maxlen)
{
    int len = 0;
    
    /* Request line */
    len = snprintf(buf, maxlen, "%s %s%s%s HTTP/1.1\r\n",
                   method_to_string(request->method),
                   url->path[0] ? url->path : "/",
                   url->query[0] ? "?" : "",
                   url->query);
    
    /* Host header */
    len += snprintf(buf + len, maxlen - len, "Host: %s", url->host);
    if (url->port != 80 && url->port != 443) {
        len += snprintf(buf + len, maxlen - len, ":%u", url->port);
    }
    len += snprintf(buf + len, maxlen - len, "\r\n");
    
    /* User-Agent */
    len += snprintf(buf + len, maxlen - len, "User-Agent: %s\r\n",
                    s_config.default_user_agent ? s_config.default_user_agent 
                                                 : GF_HTTP_USER_AGENT);
    
    /* Connection */
    len += snprintf(buf + len, maxlen - len, "Connection: %s\r\n",
                    request->keep_alive ? "keep-alive" : "close");
    
    /* Content-Type and Content-Length for body */
    if (request->body && request->body_len > 0) {
        len += snprintf(buf + len, maxlen - len, "Content-Type: %s\r\n",
                        request->content_type ? request->content_type 
                                               : "application/octet-stream");
        len += snprintf(buf + len, maxlen - len, "Content-Length: %zu\r\n",
                        request->body_len);
    }
    
    /* Authorization */
    if (request->auth_bearer) {
        len += snprintf(buf + len, maxlen - len, "Authorization: Bearer %s\r\n",
                        request->auth_bearer);
    } else if (request->auth_basic) {
        /* Note: real implementation would Base64 encode */
        len += snprintf(buf + len, maxlen - len, "Authorization: Basic %s\r\n",
                        request->auth_basic);
    }
    
    /* Custom headers */
    for (uint8_t i = 0; i < request->header_count; i++) {
        len += snprintf(buf + len, maxlen - len, "%s: %s\r\n",
                        request->headers[i].name,
                        request->headers[i].value);
    }
    
    /* End of headers */
    len += snprintf(buf + len, maxlen - len, "\r\n");
    
    return len;
}

/*===========================================================================*/
/* URL Parsing                                                                */
/*===========================================================================*/

int gf_http_parse_url(const char *url, gf_http_url_t *parsed)
{
    if (!url || !parsed) {
        return GF_HTTP_ERR_INVALID_URL;
    }
    
    memset(parsed, 0, sizeof(gf_http_url_t));
    
    /* Parse scheme */
    const char *scheme_end = strstr(url, "://");
    if (!scheme_end) {
        return GF_HTTP_ERR_INVALID_URL;
    }
    
    size_t scheme_len = scheme_end - url;
    if (scheme_len >= sizeof(parsed->scheme)) {
        return GF_HTTP_ERR_INVALID_URL;
    }
    memcpy(parsed->scheme, url, scheme_len);
    
    bool is_https = (strcmp(parsed->scheme, "https") == 0);
    parsed->port = is_https ? GF_HTTPS_DEFAULT_PORT : GF_HTTP_DEFAULT_PORT;
    
    /* Parse host */
    const char *host_start = scheme_end + 3;
    const char *host_end = host_start;
    
    while (*host_end && *host_end != ':' && *host_end != '/' && *host_end != '?') {
        host_end++;
    }
    
    size_t host_len = host_end - host_start;
    if (host_len == 0 || host_len >= sizeof(parsed->host)) {
        return GF_HTTP_ERR_INVALID_URL;
    }
    memcpy(parsed->host, host_start, host_len);
    
    /* Parse port if present */
    if (*host_end == ':') {
        parsed->port = (uint16_t)atoi(host_end + 1);
        while (*host_end && *host_end != '/' && *host_end != '?') {
            host_end++;
        }
    }
    
    /* Parse path */
    if (*host_end == '/') {
        const char *path_end = strchr(host_end, '?');
        size_t path_len;
        
        if (path_end) {
            path_len = path_end - host_end;
        } else {
            path_len = strlen(host_end);
        }
        
        if (path_len >= sizeof(parsed->path)) {
            path_len = sizeof(parsed->path) - 1;
        }
        memcpy(parsed->path, host_end, path_len);
        host_end += path_len;
    } else {
        strcpy(parsed->path, "/");
    }
    
    /* Parse query */
    if (*host_end == '?') {
        strncpy(parsed->query, host_end + 1, sizeof(parsed->query) - 1);
    }
    
    return GF_HTTP_OK;
}

int gf_http_url_encode(const char *input, char *output, size_t output_len)
{
    if (!input || !output || output_len == 0) {
        return -1;
    }
    
    static const char *hex = "0123456789ABCDEF";
    size_t out_pos = 0;
    
    for (const char *p = input; *p && out_pos < output_len - 1; p++) {
        char c = *p;
        
        if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
            (c >= '0' && c <= '9') || c == '-' || c == '_' || c == '.' || c == '~') {
            output[out_pos++] = c;
        } else if (out_pos + 3 < output_len) {
            output[out_pos++] = '%';
            output[out_pos++] = hex[(c >> 4) & 0xF];
            output[out_pos++] = hex[c & 0xF];
        } else {
            break;
        }
    }
    
    output[out_pos] = '\0';
    return (int)out_pos;
}

int gf_http_url_decode(const char *input, char *output, size_t output_len)
{
    if (!input || !output || output_len == 0) {
        return -1;
    }
    
    size_t out_pos = 0;
    
    for (const char *p = input; *p && out_pos < output_len - 1; p++) {
        if (*p == '%' && p[1] && p[2]) {
            int hi = p[1] >= 'a' ? (p[1] - 'a' + 10) :
                     p[1] >= 'A' ? (p[1] - 'A' + 10) : (p[1] - '0');
            int lo = p[2] >= 'a' ? (p[2] - 'a' + 10) :
                     p[2] >= 'A' ? (p[2] - 'A' + 10) : (p[2] - '0');
            output[out_pos++] = (char)((hi << 4) | lo);
            p += 2;
        } else if (*p == '+') {
            output[out_pos++] = ' ';
        } else {
            output[out_pos++] = *p;
        }
    }
    
    output[out_pos] = '\0';
    return (int)out_pos;
}

/*===========================================================================*/
/* Connection Pool                                                            */
/*===========================================================================*/

gf_http_connection_t *gf_http_get_connection(const char *host, uint16_t port,
                                              bool is_https)
{
    /* Look for existing connection */
    for (int i = 0; i < HTTP_POOL_SIZE; i++) {
        gf_http_connection_t *conn = &s_conn_pool[i];
        if (conn->in_use) continue;
        
        if (conn->socket != GF_INVALID_SOCKET &&
            strcmp(conn->host, host) == 0 &&
            conn->port == port &&
            conn->is_https == is_https) {
            conn->in_use = true;
            s_stats.connection_reuses++;
            return conn;
        }
    }
    
    /* Find free slot */
    for (int i = 0; i < HTTP_POOL_SIZE; i++) {
        gf_http_connection_t *conn = &s_conn_pool[i];
        if (conn->socket == GF_INVALID_SOCKET) {
            memset(conn, 0, sizeof(gf_http_connection_t));
            conn->socket = GF_INVALID_SOCKET;
            strncpy(conn->host, host, sizeof(conn->host) - 1);
            conn->port = port;
            conn->is_https = is_https;
            conn->in_use = true;
            return conn;
        }
    }
    
    /* Evict oldest connection */
    gf_http_connection_t *oldest = &s_conn_pool[0];
    for (int i = 1; i < HTTP_POOL_SIZE; i++) {
        if (s_conn_pool[i].last_used < oldest->last_used) {
            oldest = &s_conn_pool[i];
        }
    }
    
    if (oldest->socket != GF_INVALID_SOCKET) {
        gf_close(oldest->socket);
    }
    
    memset(oldest, 0, sizeof(gf_http_connection_t));
    oldest->socket = GF_INVALID_SOCKET;
    strncpy(oldest->host, host, sizeof(oldest->host) - 1);
    oldest->port = port;
    oldest->is_https = is_https;
    oldest->in_use = true;
    
    return oldest;
}

void gf_http_release_connection(gf_http_connection_t *conn, bool reusable)
{
    if (!conn) return;
    
    conn->in_use = false;
    conn->last_used = s_current_time;
    conn->requests_made++;
    
    if (!reusable && conn->socket != GF_INVALID_SOCKET) {
        gf_close(conn->socket);
        conn->socket = GF_INVALID_SOCKET;
    }
}

void gf_http_close_all_connections(void)
{
    for (int i = 0; i < HTTP_POOL_SIZE; i++) {
        if (s_conn_pool[i].socket != GF_INVALID_SOCKET) {
            gf_close(s_conn_pool[i].socket);
            s_conn_pool[i].socket = GF_INVALID_SOCKET;
        }
        s_conn_pool[i].in_use = false;
    }
}

/*===========================================================================*/
/* Retry Logic                                                                */
/*===========================================================================*/

uint32_t gf_http_calc_retry_delay(uint8_t attempt, uint32_t base_delay_ms,
                                   uint32_t max_delay_ms, uint8_t jitter_percent)
{
    /* Exponential backoff: delay = base * 2^attempt */
    uint32_t delay = base_delay_ms;
    for (uint8_t i = 0; i < attempt && delay < max_delay_ms; i++) {
        delay *= 2;
    }
    
    if (delay > max_delay_ms) {
        delay = max_delay_ms;
    }
    
    /* Add jitter */
    if (jitter_percent > 0 && jitter_percent <= 50) {
        /* Stub: would use random number */
        uint32_t jitter_range = (delay * jitter_percent) / 100;
        delay += jitter_range / 2;  /* Simplified: add half jitter */
    }
    
    return delay;
}

bool gf_http_status_should_retry(gf_http_status_t status)
{
    switch (status) {
        case GF_HTTP_408_TIMEOUT:
        case GF_HTTP_429_TOO_MANY:
        case GF_HTTP_500_INTERNAL:
        case GF_HTTP_502_BAD_GATEWAY:
        case GF_HTTP_503_UNAVAILABLE:
        case GF_HTTP_504_GATEWAY_TIMEOUT:
            return true;
        default:
            return false;
    }
}

bool gf_http_status_is_success(gf_http_status_t status)
{
    return status >= 200 && status < 300;
}

bool gf_http_status_is_redirect(gf_http_status_t status)
{
    return status >= 300 && status < 400;
}

/*===========================================================================*/
/* Response Management                                                        */
/*===========================================================================*/

int gf_http_response_alloc_body(gf_http_response_t *response, size_t size)
{
    if (!response || size == 0) {
        return -1;
    }
    
    response->body = malloc(size);
    if (!response->body) {
        return GF_HTTP_ERR_MEMORY;
    }
    
    response->body_capacity = size;
    response->body_len = 0;
    
    return GF_HTTP_OK;
}

void gf_http_response_free(gf_http_response_t *response)
{
    if (response && response->body) {
        free(response->body);
        response->body = NULL;
        response->body_len = 0;
        response->body_capacity = 0;
    }
}

const char *gf_http_response_get_header(const gf_http_response_t *response,
                                         const char *name)
{
    if (!response || !name) {
        return NULL;
    }
    
    for (uint8_t i = 0; i < response->header_count; i++) {
        /* Note: header name comparison would need stored name */
        (void)response->headers[i];
    }
    
    return NULL;
}

/*===========================================================================*/
/* HTTP Client API                                                            */
/*===========================================================================*/

int gf_http_init(const gf_http_client_config_t *config)
{
    if (config) {
        memcpy(&s_config, config, sizeof(gf_http_client_config_t));
    } else {
        memset(&s_config, 0, sizeof(gf_http_client_config_t));
        s_config.rx_buffer_size = 2048;
        s_config.tx_buffer_size = 1024;
        s_config.default_timeout_ms = GF_HTTP_DEFAULT_TIMEOUT_MS;
        s_config.enable_keepalive = true;
        s_config.max_connections = HTTP_POOL_SIZE;
    }
    
    memset(&s_stats, 0, sizeof(gf_http_stats_t));
    memset(s_conn_pool, 0, sizeof(s_conn_pool));
    
    for (int i = 0; i < HTTP_POOL_SIZE; i++) {
        s_conn_pool[i].socket = GF_INVALID_SOCKET;
    }
    
    s_initialized = true;
    return GF_HTTP_OK;
}

void gf_http_shutdown(void)
{
    gf_http_close_all_connections();
    s_initialized = false;
}

gf_http_err_t gf_http_request(const gf_http_request_t *request,
                               gf_http_response_t *response)
{
    if (!s_initialized || !request || !request->url || !response) {
        return GF_HTTP_ERR_GENERAL;
    }
    
    memset(response, 0, sizeof(gf_http_response_t));
    s_stats.requests_made++;
    
    /* Parse URL */
    gf_http_url_t url;
    if (gf_http_parse_url(request->url, &url) != GF_HTTP_OK) {
        s_stats.requests_failed++;
        return GF_HTTP_ERR_INVALID_URL;
    }
    
    bool is_https = (strcmp(url.scheme, "https") == 0);
    
    /* Resolve hostname */
    gf_ipv4_addr_t addr;
    uint32_t dns_start = s_current_time;
    gf_dns_rcode_t dns_result = gf_dns_resolve(url.host, &addr, 
                                                request->connect_timeout_ms);
    response->dns_time_ms = s_current_time - dns_start;
    
    if (dns_result != GF_DNS_RCODE_OK) {
        s_stats.dns_failures++;
        s_stats.requests_failed++;
        return GF_HTTP_ERR_DNS;
    }
    
    /* Retry loop */
    uint8_t max_retries = request->max_retries > 0 ? request->max_retries : 3;
    gf_http_err_t last_error = GF_HTTP_OK;
    
    for (uint8_t attempt = 0; attempt <= max_retries; attempt++) {
        if (attempt > 0) {
            /* Wait before retry */
            uint32_t delay = gf_http_calc_retry_delay(
                attempt - 1,
                request->retry_base_delay_ms > 0 ? request->retry_base_delay_ms : 1000,
                30000,
                20
            );
            /* Stub: would sleep for delay */
            (void)delay;
            s_stats.retries_total++;
        }
        
        /* Get connection */
        gf_http_connection_t *conn = gf_http_get_connection(url.host, url.port, is_https);
        if (!conn) {
            last_error = GF_HTTP_ERR_MEMORY;
            continue;
        }
        
        /* Connect if needed */
        if (conn->socket == GF_INVALID_SOCKET) {
            uint32_t connect_start = s_current_time;
            
            conn->socket = gf_socket(GF_NET_AF_INET, GF_SOCK_STREAM, GF_IPPROTO_TCP);
            if (conn->socket == GF_INVALID_SOCKET) {
                gf_http_release_connection(conn, false);
                last_error = GF_HTTP_ERR_CONNECT;
                s_stats.connect_failures++;
                continue;
            }
            
            gf_sockaddr_in_t dest = {
                .family = GF_NET_AF_INET,
                .port = url.port,
                .addr = addr
            };
            
            if (gf_connect(conn->socket, &dest) != GF_NET_OK) {
                gf_http_release_connection(conn, false);
                last_error = GF_HTTP_ERR_CONNECT;
                s_stats.connect_failures++;
                continue;
            }
            
            response->connect_time_ms = s_current_time - connect_start;
            
            /* TLS handshake would go here for HTTPS */
            if (is_https) {
                /* Stub: would perform TLS handshake */
            }
        }
        
        /* Build request */
        int req_len = build_request(request, &url, (char *)s_tx_buffer, 
                                    sizeof(s_tx_buffer));
        if (req_len < 0) {
            gf_http_release_connection(conn, false);
            last_error = GF_HTTP_ERR_GENERAL;
            continue;
        }
        
        /* Send request */
        int sent = gf_send(conn->socket, s_tx_buffer, req_len, 0);
        if (sent < 0) {
            gf_http_release_connection(conn, false);
            last_error = GF_HTTP_ERR_WRITE;
            continue;
        }
        s_stats.bytes_sent += sent;
        
        /* Send body if present */
        if (request->body && request->body_len > 0) {
            sent = gf_send(conn->socket, request->body, request->body_len, 0);
            if (sent < 0) {
                gf_http_release_connection(conn, false);
                last_error = GF_HTTP_ERR_WRITE;
                continue;
            }
            s_stats.bytes_sent += sent;
        }
        
        /* Receive response */
        /* Stub: simplified response parsing */
        int recv_len = gf_recv(conn->socket, s_rx_buffer, sizeof(s_rx_buffer) - 1, 0);
        if (recv_len < 0) {
            if (recv_len == GF_NET_ERR_TIMEOUT) {
                gf_http_release_connection(conn, false);
                last_error = GF_HTTP_ERR_TIMEOUT;
                s_stats.timeouts++;
                if (request->retry_on_timeout) {
                    continue;
                }
            }
            gf_http_release_connection(conn, false);
            last_error = GF_HTTP_ERR_READ;
            continue;
        }
        
        s_rx_buffer[recv_len] = '\0';
        s_stats.bytes_received += recv_len;
        
        /* Parse response (simplified) */
        /* In real implementation, would parse line by line */
        response->status_code = GF_HTTP_200_OK;
        response->keep_alive = true;
        
        /* Successfully completed request */
        response->retry_count = attempt;
        response->total_time_ms = s_current_time - dns_start;
        
        gf_http_release_connection(conn, response->keep_alive && 
                                         s_config.enable_keepalive);
        
        s_stats.requests_success++;
        return GF_HTTP_OK;
    }
    
    s_stats.requests_failed++;
    return last_error;
}

gf_http_err_t gf_http_request_stream(const gf_http_request_t *request,
                                      const gf_http_callbacks_t *callbacks,
                                      gf_http_response_t *response)
{
    if (!callbacks) {
        return gf_http_request(request, response);
    }
    
    /* Make request with streaming callbacks */
    gf_http_err_t err = gf_http_request(request, response);
    
    if (err == GF_HTTP_OK && callbacks->on_body && response->body) {
        callbacks->on_body(response->body, response->body_len, callbacks->user_data);
    }
    
    return err;
}

/*===========================================================================*/
/* Convenience Methods                                                        */
/*===========================================================================*/

gf_http_err_t gf_http_get(const char *url, gf_http_response_t *response,
                           uint32_t timeout_ms)
{
    gf_http_request_t request = {
        .method = GF_HTTP_GET,
        .url = url,
        .connect_timeout_ms = timeout_ms,
        .read_timeout_ms = timeout_ms,
        .total_timeout_ms = timeout_ms,
        .max_retries = 3,
        .retry_base_delay_ms = 1000,
        .retry_on_5xx = true,
        .retry_on_timeout = true,
        .keep_alive = true,
        .follow_redirects = true,
        .max_redirects = GF_HTTP_MAX_REDIRECTS
    };
    
    return gf_http_request(&request, response);
}

gf_http_err_t gf_http_post_json(const char *url, const char *json_body,
                                 gf_http_response_t *response, uint32_t timeout_ms)
{
    gf_http_request_t request = {
        .method = GF_HTTP_POST,
        .url = url,
        .body = json_body,
        .body_len = json_body ? strlen(json_body) : 0,
        .content_type = "application/json",
        .connect_timeout_ms = timeout_ms,
        .read_timeout_ms = timeout_ms,
        .total_timeout_ms = timeout_ms,
        .max_retries = 3,
        .retry_base_delay_ms = 1000,
        .retry_on_5xx = true,
        .retry_on_timeout = true,
        .keep_alive = true
    };
    
    return gf_http_request(&request, response);
}

gf_http_err_t gf_http_put(const char *url, const void *body, size_t body_len,
                           const char *content_type, gf_http_response_t *response,
                           uint32_t timeout_ms)
{
    gf_http_request_t request = {
        .method = GF_HTTP_PUT,
        .url = url,
        .body = body,
        .body_len = body_len,
        .content_type = content_type,
        .connect_timeout_ms = timeout_ms,
        .read_timeout_ms = timeout_ms,
        .max_retries = 3,
        .keep_alive = true
    };
    
    return gf_http_request(&request, response);
}

gf_http_err_t gf_http_delete(const char *url, gf_http_response_t *response,
                              uint32_t timeout_ms)
{
    gf_http_request_t request = {
        .method = GF_HTTP_DELETE,
        .url = url,
        .connect_timeout_ms = timeout_ms,
        .read_timeout_ms = timeout_ms,
        .max_retries = 2,
        .keep_alive = true
    };
    
    return gf_http_request(&request, response);
}

/*===========================================================================*/
/* Statistics                                                                 */
/*===========================================================================*/

void gf_http_get_stats(gf_http_stats_t *stats)
{
    if (stats) {
        memcpy(stats, &s_stats, sizeof(gf_http_stats_t));
    }
}

void gf_http_reset_stats(void)
{
    memset(&s_stats, 0, sizeof(gf_http_stats_t));
}
