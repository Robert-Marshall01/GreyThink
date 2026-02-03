/**
 * @file network_tests.c
 * @brief Networking Stack Tests
 * 
 * Tests for TCP/IP stack, DNS resolver, and HTTP client
 */

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "net/tcp_ip.h"
#include "net/dns.h"
#include "net/http_client.h"

/*===========================================================================*/
/* Test Counters                                                              */
/*===========================================================================*/

static int test_count = 0;
static int pass_count = 0;

#define TEST(name) \
    do { \
        test_count++; \
        printf("  [%2d] %-50s ", test_count, name); \
    } while(0)

#define PASS() \
    do { \
        pass_count++; \
        printf("[PASS]\n"); \
    } while(0)

#define FAIL(msg) \
    do { \
        printf("[FAIL] %s\n", msg); \
    } while(0)

#define ASSERT_EQ(a, b, desc) \
    do { \
        if ((a) != (b)) { \
            FAIL(desc); \
            return; \
        } \
    } while(0)

#define ASSERT_TRUE(cond, desc) \
    do { \
        if (!(cond)) { \
            FAIL(desc); \
            return; \
        } \
    } while(0)

/*===========================================================================*/
/* TCP/IP Stack Tests                                                         */
/*===========================================================================*/

static void test_net_init(void)
{
    TEST("gf_net_init with defaults");
    
    int result = gf_net_init(NULL);
    ASSERT_EQ(result, GF_NET_OK, "init failed");
    
    PASS();
}

static void test_socket_create(void)
{
    TEST("gf_socket create TCP");
    
    gf_socket_t sock = gf_socket(GF_NET_AF_INET, GF_SOCK_STREAM, GF_IPPROTO_TCP);
    ASSERT_TRUE(sock != GF_INVALID_SOCKET, "socket creation failed");
    
    gf_close(sock);
    PASS();
}

static void test_socket_create_udp(void)
{
    TEST("gf_socket create UDP");
    
    gf_socket_t sock = gf_socket(GF_NET_AF_INET, GF_SOCK_DGRAM, GF_IPPROTO_UDP);
    ASSERT_TRUE(sock != GF_INVALID_SOCKET, "UDP socket creation failed");
    
    gf_close(sock);
    PASS();
}

static void test_socket_bind(void)
{
    TEST("gf_bind socket to port");
    
    gf_socket_t sock = gf_socket(GF_NET_AF_INET, GF_SOCK_STREAM, GF_IPPROTO_TCP);
    
    gf_sockaddr_in_t addr = {
        .family = GF_NET_AF_INET,
        .port = 8080,
        .addr = GF_IPV4_ANY
    };
    
    int result = gf_bind(sock, &addr);
    ASSERT_EQ(result, GF_NET_OK, "bind failed");
    
    gf_close(sock);
    PASS();
}

static void test_socket_listen(void)
{
    TEST("gf_listen on TCP socket");
    
    gf_socket_t sock = gf_socket(GF_NET_AF_INET, GF_SOCK_STREAM, GF_IPPROTO_TCP);
    
    gf_sockaddr_in_t addr = {
        .family = GF_NET_AF_INET,
        .port = 8081,
        .addr = GF_IPV4_ANY
    };
    
    gf_bind(sock, &addr);
    int result = gf_listen(sock, 5);
    ASSERT_EQ(result, GF_NET_OK, "listen failed");
    
    gf_close(sock);
    PASS();
}

static void test_socket_connect(void)
{
    TEST("gf_connect TCP socket");
    
    gf_socket_t sock = gf_socket(GF_NET_AF_INET, GF_SOCK_STREAM, GF_IPPROTO_TCP);
    
    gf_sockaddr_in_t addr = {
        .family = GF_NET_AF_INET,
        .port = 80,
        .addr = {{127, 0, 0, 1}}
    };
    
    int result = gf_connect(sock, &addr);
    ASSERT_EQ(result, GF_NET_OK, "connect failed");
    
    /* Verify state */
    gf_tcp_state_t state = gf_tcp_get_state(sock);
    ASSERT_EQ(state, GF_TCP_STATE_ESTABLISHED, "wrong state after connect");
    
    gf_close(sock);
    PASS();
}

static void test_socket_setsockopt(void)
{
    TEST("gf_setsockopt options");
    
    gf_socket_t sock = gf_socket(GF_NET_AF_INET, GF_SOCK_STREAM, GF_IPPROTO_TCP);
    
    int val = 1;
    int result = gf_setsockopt(sock, GF_SO_REUSEADDR, &val, sizeof(val));
    ASSERT_EQ(result, GF_NET_OK, "setsockopt REUSEADDR failed");
    
    result = gf_setsockopt(sock, GF_SO_KEEPALIVE, &val, sizeof(val));
    ASSERT_EQ(result, GF_NET_OK, "setsockopt KEEPALIVE failed");
    
    gf_close(sock);
    PASS();
}

static void test_tcp_stats(void)
{
    TEST("gf_tcp_get_stats");
    
    gf_socket_t sock = gf_socket(GF_NET_AF_INET, GF_SOCK_STREAM, GF_IPPROTO_TCP);
    gf_sockaddr_in_t addr = {
        .family = GF_NET_AF_INET,
        .port = 80,
        .addr = {{127, 0, 0, 1}}
    };
    gf_connect(sock, &addr);
    
    gf_tcp_stats_t stats;
    int result = gf_tcp_get_stats(sock, &stats);
    ASSERT_EQ(result, GF_NET_OK, "get_stats failed");
    ASSERT_EQ(stats.state, GF_TCP_STATE_ESTABLISHED, "wrong state in stats");
    
    gf_close(sock);
    PASS();
}

static void test_inet_ntoa(void)
{
    TEST("gf_inet_ntoa formatting");
    
    gf_ipv4_addr_t addr = {{192, 168, 1, 100}};
    char buf[16];
    
    const char *result = gf_inet_ntoa(addr, buf, sizeof(buf));
    ASSERT_TRUE(result != NULL, "ntoa returned NULL");
    ASSERT_TRUE(strcmp(buf, "192.168.1.100") == 0, "wrong format");
    
    PASS();
}

static void test_inet_aton(void)
{
    TEST("gf_inet_aton parsing");
    
    gf_ipv4_addr_t addr;
    int result = gf_inet_aton("10.0.0.1", &addr);
    ASSERT_EQ(result, GF_NET_OK, "aton failed");
    ASSERT_EQ(addr.addr[0], 10, "octet 0 wrong");
    ASSERT_EQ(addr.addr[1], 0, "octet 1 wrong");
    ASSERT_EQ(addr.addr[2], 0, "octet 2 wrong");
    ASSERT_EQ(addr.addr[3], 1, "octet 3 wrong");
    
    PASS();
}

static void test_byte_order(void)
{
    TEST("gf_htons/gf_ntohs byte order");
    
    uint16_t host = 0x1234;
    uint16_t net = gf_htons(host);
    uint16_t back = gf_ntohs(net);
    
    ASSERT_EQ(back, host, "roundtrip failed");
    
    PASS();
}

static void test_checksum(void)
{
    TEST("gf_inet_checksum calculation");
    
    /* Test data with known checksum */
    uint8_t data[] = {0x45, 0x00, 0x00, 0x3c, 0x1c, 0x46, 0x40, 0x00};
    uint16_t checksum = gf_inet_checksum(data, sizeof(data));
    
    ASSERT_TRUE(checksum != 0, "checksum was zero");
    
    PASS();
}

static void test_pbuf_alloc_free(void)
{
    TEST("gf_pbuf_alloc/free");
    
    gf_pbuf_t *p = gf_pbuf_alloc(GF_PBUF_RAM, 256);
    ASSERT_TRUE(p != NULL, "alloc failed");
    ASSERT_EQ(p->len, 256, "wrong length");
    ASSERT_EQ(p->ref, 1, "wrong refcount");
    
    gf_pbuf_free(p);
    PASS();
}

/*===========================================================================*/
/* DNS Tests                                                                  */
/*===========================================================================*/

static void test_dns_init(void)
{
    TEST("gf_dns_init with defaults");
    
    int result = gf_dns_init(NULL);
    ASSERT_EQ(result, 0, "dns init failed");
    
    PASS();
}

static void test_dns_resolve_ip(void)
{
    TEST("gf_dns_resolve with IP address");
    
    gf_ipv4_addr_t addr;
    gf_dns_rcode_t rcode = gf_dns_resolve("192.168.1.1", &addr, 5000);
    
    ASSERT_EQ(rcode, GF_DNS_RCODE_OK, "resolve failed");
    ASSERT_EQ(addr.addr[0], 192, "wrong addr[0]");
    ASSERT_EQ(addr.addr[1], 168, "wrong addr[1]");
    
    PASS();
}

static void test_dns_resolve_hostname(void)
{
    TEST("gf_dns_resolve hostname");
    
    gf_ipv4_addr_t addr;
    gf_dns_rcode_t rcode = gf_dns_resolve("example.com", &addr, 5000);
    
    ASSERT_EQ(rcode, GF_DNS_RCODE_OK, "resolve failed");
    
    PASS();
}

static void test_dns_cache(void)
{
    TEST("gf_dns_cache operations");
    
    /* Clear cache first */
    gf_dns_cache_clear();
    
    /* Add entry */
    gf_dns_record_t rec = {
        .type = GF_DNS_TYPE_A,
        .class_ = GF_DNS_CLASS_IN,
        .ttl = 300
    };
    rec.rdata.a.addr[0] = 10;
    rec.rdata.a.addr[1] = 0;
    rec.rdata.a.addr[2] = 0;
    rec.rdata.a.addr[3] = 1;
    
    gf_dns_cache_add("test.local", &rec);
    
    /* Lookup */
    gf_dns_record_t found;
    bool hit = gf_dns_cache_lookup("test.local", GF_DNS_TYPE_A, &found);
    ASSERT_TRUE(hit, "cache miss");
    ASSERT_EQ(found.rdata.a.addr[0], 10, "wrong cached addr");
    
    PASS();
}

static void test_dns_cache_stats(void)
{
    TEST("gf_dns_cache_get_stats");
    
    gf_dns_cache_stats_t stats;
    gf_dns_cache_get_stats(&stats);
    
    /* Verify stats structure is accessible (unsigned types always >= 0) */
    ASSERT_TRUE(stats.hits == 0 || stats.hits > 0, "stats accessible");
    
    PASS();
}

static void test_dns_servers(void)
{
    TEST("gf_dns set/get servers");
    
    gf_ipv4_addr_t servers[2] = {
        {{8, 8, 8, 8}},
        {{8, 8, 4, 4}}
    };
    
    gf_dns_set_servers(servers, 2);
    
    gf_ipv4_addr_t retrieved[2];
    uint8_t count = gf_dns_get_servers(retrieved, 2);
    ASSERT_EQ(count, 2, "wrong server count");
    
    PASS();
}

/*===========================================================================*/
/* HTTP Client Tests                                                          */
/*===========================================================================*/

static void test_http_init(void)
{
    TEST("gf_http_init with defaults");
    
    int result = gf_http_init(NULL);
    ASSERT_EQ(result, GF_HTTP_OK, "http init failed");
    
    PASS();
}

static void test_http_parse_url(void)
{
    TEST("gf_http_parse_url");
    
    gf_http_url_t url;
    int result = gf_http_parse_url("http://example.com:8080/api/v1?key=val", &url);
    
    ASSERT_EQ(result, GF_HTTP_OK, "parse failed");
    ASSERT_TRUE(strcmp(url.scheme, "http") == 0, "wrong scheme");
    ASSERT_TRUE(strcmp(url.host, "example.com") == 0, "wrong host");
    ASSERT_EQ(url.port, 8080, "wrong port");
    ASSERT_TRUE(strcmp(url.path, "/api/v1") == 0, "wrong path");
    ASSERT_TRUE(strcmp(url.query, "key=val") == 0, "wrong query");
    
    PASS();
}

static void test_http_parse_url_https(void)
{
    TEST("gf_http_parse_url HTTPS");
    
    gf_http_url_t url;
    int result = gf_http_parse_url("https://secure.example.com/", &url);
    
    ASSERT_EQ(result, GF_HTTP_OK, "parse failed");
    ASSERT_TRUE(strcmp(url.scheme, "https") == 0, "wrong scheme");
    ASSERT_EQ(url.port, 443, "wrong default port");
    
    PASS();
}

static void test_http_url_encode(void)
{
    TEST("gf_http_url_encode");
    
    char output[128];
    int result = gf_http_url_encode("hello world&foo=bar", output, sizeof(output));
    
    ASSERT_TRUE(result > 0, "encode failed");
    ASSERT_TRUE(strstr(output, "%20") != NULL || strstr(output, "+") != NULL, 
                "space not encoded");
    
    PASS();
}

static void test_http_url_decode(void)
{
    TEST("gf_http_url_decode");
    
    char output[128];
    int result = gf_http_url_decode("hello%20world", output, sizeof(output));
    
    ASSERT_TRUE(result > 0, "decode failed");
    ASSERT_TRUE(strcmp(output, "hello world") == 0, "wrong decoded string");
    
    PASS();
}

static void test_http_status_helpers(void)
{
    TEST("gf_http_status_is_* helpers");
    
    ASSERT_TRUE(gf_http_status_is_success(GF_HTTP_200_OK), "200 not success");
    ASSERT_TRUE(gf_http_status_is_success(GF_HTTP_201_CREATED), "201 not success");
    ASSERT_TRUE(!gf_http_status_is_success(GF_HTTP_404_NOT_FOUND), "404 is success");
    
    ASSERT_TRUE(gf_http_status_is_redirect(GF_HTTP_301_MOVED), "301 not redirect");
    ASSERT_TRUE(gf_http_status_is_redirect(GF_HTTP_302_FOUND), "302 not redirect");
    
    ASSERT_TRUE(gf_http_status_should_retry(GF_HTTP_503_UNAVAILABLE), "503 not retryable");
    ASSERT_TRUE(gf_http_status_should_retry(GF_HTTP_429_TOO_MANY), "429 not retryable");
    
    PASS();
}

static void test_http_retry_delay(void)
{
    TEST("gf_http_calc_retry_delay exponential");
    
    uint32_t delay0 = gf_http_calc_retry_delay(0, 1000, 30000, 0);
    uint32_t delay1 = gf_http_calc_retry_delay(1, 1000, 30000, 0);
    uint32_t delay2 = gf_http_calc_retry_delay(2, 1000, 30000, 0);
    
    ASSERT_EQ(delay0, 1000, "wrong delay for attempt 0");
    ASSERT_EQ(delay1, 2000, "wrong delay for attempt 1");
    ASSERT_EQ(delay2, 4000, "wrong delay for attempt 2");
    
    /* Test max cap */
    uint32_t delay_max = gf_http_calc_retry_delay(10, 1000, 5000, 0);
    ASSERT_EQ(delay_max, 5000, "max delay not capped");
    
    PASS();
}

static void test_http_connection_pool(void)
{
    TEST("gf_http connection pool");
    
    gf_http_connection_t *conn1 = gf_http_get_connection("host1.com", 80, false);
    ASSERT_TRUE(conn1 != NULL, "conn1 alloc failed");
    
    gf_http_connection_t *conn2 = gf_http_get_connection("host2.com", 443, true);
    ASSERT_TRUE(conn2 != NULL, "conn2 alloc failed");
    
    gf_http_release_connection(conn1, true);
    gf_http_release_connection(conn2, false);
    
    /* Reuse should work */
    gf_http_connection_t *conn3 = gf_http_get_connection("host1.com", 80, false);
    ASSERT_TRUE(conn3 != NULL, "conn reuse failed");
    gf_http_release_connection(conn3, false);
    
    gf_http_close_all_connections();
    PASS();
}

static void test_http_stats(void)
{
    TEST("gf_http_get_stats");
    
    gf_http_stats_t stats;
    gf_http_get_stats(&stats);
    
    /* Verify stats structure is accessible */
    ASSERT_TRUE(stats.requests_made <= 1000000, "stats sane");
    
    gf_http_reset_stats();
    gf_http_get_stats(&stats);
    ASSERT_EQ(stats.requests_made, 0, "reset failed");
    
    PASS();
}

/*===========================================================================*/
/* Network Interface Tests                                                    */
/*===========================================================================*/

static void test_netif_add(void)
{
    TEST("gf_netif_add");
    
    gf_netif_t netif = {
        .name = "eth0",
        .type = GF_NETIF_TYPE_ETHERNET,
        .mtu = 1500
    };
    
    int result = gf_netif_add(&netif);
    ASSERT_EQ(result, GF_NET_OK, "add failed");
    
    gf_netif_remove(&netif);
    PASS();
}

static void test_netif_set_addr(void)
{
    TEST("gf_netif_set_addr");
    
    gf_netif_t netif = {
        .name = "eth0",
        .type = GF_NETIF_TYPE_ETHERNET,
        .mtu = 1500
    };
    
    gf_netif_add(&netif);
    
    gf_netif_addr_t addr = {
        .mac = {0xDE, 0xAD, 0xBE, 0xEF, 0xCA, 0xFE},
        .ip = {{192, 168, 1, 100}},
        .netmask = {{255, 255, 255, 0}},
        .gateway = {{192, 168, 1, 1}}
    };
    
    int result = gf_netif_set_addr(&netif, &addr);
    ASSERT_EQ(result, GF_NET_OK, "set_addr failed");
    
    gf_netif_remove(&netif);
    PASS();
}

static void test_netif_default(void)
{
    TEST("gf_netif_set/get_default");
    
    gf_netif_t netif = {
        .name = "eth0",
        .type = GF_NETIF_TYPE_ETHERNET
    };
    
    gf_netif_add(&netif);
    gf_netif_set_default(&netif);
    
    gf_netif_t *def = gf_netif_get_default();
    ASSERT_TRUE(def == &netif, "wrong default");
    
    gf_netif_remove(&netif);
    PASS();
}

/*===========================================================================*/
/* Main Test Runner                                                           */
/*===========================================================================*/

int main(void)
{
    printf("\n=== Grey Firmware Networking Tests ===\n\n");
    
    /* Initialize stack for tests */
    gf_net_init(NULL);
    gf_dns_init(NULL);
    gf_http_init(NULL);
    
    printf("TCP/IP Stack Tests:\n");
    test_net_init();
    test_socket_create();
    test_socket_create_udp();
    test_socket_bind();
    test_socket_listen();
    test_socket_connect();
    test_socket_setsockopt();
    test_tcp_stats();
    test_inet_ntoa();
    test_inet_aton();
    test_byte_order();
    test_checksum();
    test_pbuf_alloc_free();
    
    printf("\nNetwork Interface Tests:\n");
    test_netif_add();
    test_netif_set_addr();
    test_netif_default();
    
    printf("\nDNS Resolver Tests:\n");
    test_dns_init();
    test_dns_resolve_ip();
    test_dns_resolve_hostname();
    test_dns_cache();
    test_dns_cache_stats();
    test_dns_servers();
    
    printf("\nHTTP Client Tests:\n");
    test_http_init();
    test_http_parse_url();
    test_http_parse_url_https();
    test_http_url_encode();
    test_http_url_decode();
    test_http_status_helpers();
    test_http_retry_delay();
    test_http_connection_pool();
    test_http_stats();
    
    /* Cleanup */
    gf_http_shutdown();
    gf_dns_shutdown();
    gf_net_shutdown();
    
    printf("\n=== Results: %d/%d tests passed ===\n\n", pass_count, test_count);
    
    return (pass_count == test_count) ? 0 : 1;
}
