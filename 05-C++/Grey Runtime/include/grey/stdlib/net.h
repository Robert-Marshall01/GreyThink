// Grey Runtime - Standard Library: Networking
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"
#include "grey/objects.h"

#ifdef GREY_PLATFORM_WINDOWS
    #include <winsock2.h>
    #include <ws2tcpip.h>
    using socket_t = SOCKET;
    #define GREY_INVALID_SOCKET INVALID_SOCKET
#else
    #include <sys/socket.h>
    #include <netinet/in.h>
    #include <arpa/inet.h>
    #include <netdb.h>
    #include <unistd.h>
    using socket_t = int;
    #define GREY_INVALID_SOCKET (-1)
#endif

#include <string>
#include <vector>

namespace grey {

class VM;

// ============================================================
// Socket Abstraction
// ============================================================

enum class SocketType { TCP, UDP };

class Socket {
public:
    Socket() = default;
    ~Socket() { close(); }

    bool create(SocketType type) {
        int sock_type = (type == SocketType::TCP) ? SOCK_STREAM : SOCK_DGRAM;
        fd_ = ::socket(AF_INET, sock_type, 0);
        return fd_ != GREY_INVALID_SOCKET;
    }

    bool bind(const std::string& host, u16 port) {
        sockaddr_in addr{};
        addr.sin_family = AF_INET;
        addr.sin_port = htons(port);
        if (host == "0.0.0.0" || host.empty()) {
            addr.sin_addr.s_addr = INADDR_ANY;
        } else {
            inet_pton(AF_INET, host.c_str(), &addr.sin_addr);
        }
        return ::bind(fd_, reinterpret_cast<sockaddr*>(&addr), sizeof(addr)) == 0;
    }

    bool listen(int backlog = 128) {
        return ::listen(fd_, backlog) == 0;
    }

    Socket accept() {
        sockaddr_in client_addr{};
        socklen_t len = sizeof(client_addr);
        socket_t client_fd = ::accept(fd_, reinterpret_cast<sockaddr*>(&client_addr), &len);
        Socket client;
        client.fd_ = client_fd;
        return client;
    }

    bool connect(const std::string& host, u16 port) {
        sockaddr_in addr{};
        addr.sin_family = AF_INET;
        addr.sin_port = htons(port);
        inet_pton(AF_INET, host.c_str(), &addr.sin_addr);
        return ::connect(fd_, reinterpret_cast<sockaddr*>(&addr), sizeof(addr)) == 0;
    }

    i64 send(const byte* data, size_t len) {
        return ::send(fd_, reinterpret_cast<const char*>(data), static_cast<int>(len), 0);
    }

    i64 recv(byte* buffer, size_t len) {
        return ::recv(fd_, reinterpret_cast<char*>(buffer), static_cast<int>(len), 0);
    }

    // UDP
    i64 sendto(const byte* data, size_t len, const std::string& host, u16 port) {
        sockaddr_in addr{};
        addr.sin_family = AF_INET;
        addr.sin_port = htons(port);
        inet_pton(AF_INET, host.c_str(), &addr.sin_addr);
        return ::sendto(fd_, reinterpret_cast<const char*>(data), static_cast<int>(len),
                        0, reinterpret_cast<sockaddr*>(&addr), sizeof(addr));
    }

    i64 recvfrom(byte* buffer, size_t len, std::string& from_host, u16& from_port) {
        sockaddr_in from{};
        socklen_t from_len = sizeof(from);
        i64 n = ::recvfrom(fd_, reinterpret_cast<char*>(buffer), static_cast<int>(len),
                           0, reinterpret_cast<sockaddr*>(&from), &from_len);
        if (n > 0) {
            char ip[INET_ADDRSTRLEN];
            inet_ntop(AF_INET, &from.sin_addr, ip, sizeof(ip));
            from_host = ip;
            from_port = ntohs(from.sin_port);
        }
        return n;
    }

    void close() {
        if (fd_ != GREY_INVALID_SOCKET) {
#ifdef GREY_PLATFORM_WINDOWS
            closesocket(fd_);
#else
            ::close(fd_);
#endif
            fd_ = GREY_INVALID_SOCKET;
        }
    }

    bool is_valid() const { return fd_ != GREY_INVALID_SOCKET; }
    socket_t fd() const { return fd_; }

    // Set socket options
    void set_reuse_addr(bool enable) {
        int val = enable ? 1 : 0;
        setsockopt(fd_, SOL_SOCKET, SO_REUSEADDR, reinterpret_cast<const char*>(&val), sizeof(val));
    }

    void set_nonblocking(bool enable);

private:
    socket_t fd_ = GREY_INVALID_SOCKET;
};

// ============================================================
// DNS Resolution
// ============================================================

struct DNSResult {
    std::string hostname;
    std::vector<std::string> addresses;
    bool success = false;
};

DNSResult dns_resolve(const std::string& hostname);

// ============================================================
// HTTP Client (simple)
// ============================================================

struct HTTPRequest {
    std::string method = "GET";
    std::string url;
    std::unordered_map<std::string, std::string> headers;
    std::vector<byte> body;
};

struct HTTPResponse {
    int status_code = 0;
    std::string status_text;
    std::unordered_map<std::string, std::string> headers;
    std::vector<byte> body;
    bool success = false;
    std::string error;
};

class HTTPClient {
public:
    HTTPClient() = default;

    HTTPResponse request(const HTTPRequest& req);
    HTTPResponse get(const std::string& url);
    HTTPResponse post(const std::string& url, const std::vector<byte>& body,
                      const std::string& content_type = "application/json");

    void set_timeout(int ms) { timeout_ms_ = ms; }

private:
    int timeout_ms_ = 30000;
    bool parse_url(const std::string& url, std::string& host, u16& port, std::string& path);
};

// ============================================================
// Platform init
// ============================================================

void network_init();
void network_shutdown();

// ============================================================
// Register networking natives
// ============================================================

void register_network_natives(VM& vm);

} // namespace grey
