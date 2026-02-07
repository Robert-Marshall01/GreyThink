// Grey Runtime - Networking Standard Library Implementation
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.

#include "grey/stdlib/net.h"
#include "grey/vm.h"

#ifndef GREY_PLATFORM_WINDOWS
#include <fcntl.h>
#endif

namespace grey {

// ============================================================
// Platform Init
// ============================================================

void network_init() {
#ifdef GREY_PLATFORM_WINDOWS
    WSADATA wsa;
    WSAStartup(MAKEWORD(2, 2), &wsa);
#endif
}

void network_shutdown() {
#ifdef GREY_PLATFORM_WINDOWS
    WSACleanup();
#endif
}

// ============================================================
// Socket - Non-blocking
// ============================================================

void Socket::set_nonblocking(bool enable) {
#ifdef GREY_PLATFORM_WINDOWS
    u_long mode = enable ? 1 : 0;
    ioctlsocket(fd_, FIONBIO, &mode);
#else
    int flags = fcntl(fd_, F_GETFL, 0);
    if (enable) flags |= O_NONBLOCK;
    else flags &= ~O_NONBLOCK;
    fcntl(fd_, F_SETFL, flags);
#endif
}

// ============================================================
// DNS Resolution
// ============================================================

DNSResult dns_resolve(const std::string& hostname) {
    DNSResult result;
    result.hostname = hostname;

    struct addrinfo hints{}, *res = nullptr;
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;

    if (getaddrinfo(hostname.c_str(), nullptr, &hints, &res) != 0) {
        result.success = false;
        return result;
    }

    for (auto* p = res; p; p = p->ai_next) {
        char ip[INET_ADDRSTRLEN];
        auto* addr = reinterpret_cast<sockaddr_in*>(p->ai_addr);
        inet_ntop(AF_INET, &addr->sin_addr, ip, sizeof(ip));
        result.addresses.push_back(ip);
    }

    freeaddrinfo(res);
    result.success = true;
    return result;
}

// ============================================================
// HTTP Client
// ============================================================

bool HTTPClient::parse_url(const std::string& url, std::string& host,
                           u16& port, std::string& path) {
    // Simple URL parser
    std::string work = url;
    bool is_https = false;

    if (work.substr(0, 8) == "https://") {
        work = work.substr(8);
        is_https = true;
        port = 443;
    } else if (work.substr(0, 7) == "http://") {
        work = work.substr(7);
        port = 80;
    } else {
        port = 80;
    }

    auto slash = work.find('/');
    if (slash == std::string::npos) {
        host = work;
        path = "/";
    } else {
        host = work.substr(0, slash);
        path = work.substr(slash);
    }

    // Check for port in host
    auto colon = host.find(':');
    if (colon != std::string::npos) {
        port = static_cast<u16>(std::stoi(host.substr(colon + 1)));
        host = host.substr(0, colon);
    }

    return true;
}

HTTPResponse HTTPClient::request(const HTTPRequest& req) {
    HTTPResponse resp;
    std::string host, path;
    u16 port;

    if (!parse_url(req.url, host, port, path)) {
        resp.error = "Invalid URL";
        return resp;
    }

    // Resolve hostname
    auto dns = dns_resolve(host);
    if (!dns.success || dns.addresses.empty()) {
        resp.error = "DNS resolution failed for: " + host;
        return resp;
    }

    // Connect
    Socket sock;
    if (!sock.create(SocketType::TCP)) {
        resp.error = "Cannot create socket";
        return resp;
    }

    if (!sock.connect(dns.addresses[0], port)) {
        resp.error = "Connection failed";
        return resp;
    }

    // Build HTTP request
    std::string http_req = req.method + " " + path + " HTTP/1.1\r\n";
    http_req += "Host: " + host + "\r\n";
    http_req += "Connection: close\r\n";
    for (auto& [k, v] : req.headers) {
        http_req += k + ": " + v + "\r\n";
    }
    if (!req.body.empty()) {
        http_req += "Content-Length: " + std::to_string(req.body.size()) + "\r\n";
    }
    http_req += "\r\n";

    // Send
    sock.send(reinterpret_cast<const byte*>(http_req.data()), http_req.size());
    if (!req.body.empty()) {
        sock.send(req.body.data(), req.body.size());
    }

    // Receive
    std::vector<byte> response_data;
    byte buf[4096];
    while (true) {
        i64 n = sock.recv(buf, sizeof(buf));
        if (n <= 0) break;
        response_data.insert(response_data.end(), buf, buf + n);
    }

    // Parse response
    std::string raw(response_data.begin(), response_data.end());
    auto header_end = raw.find("\r\n\r\n");
    if (header_end == std::string::npos) {
        resp.error = "Malformed response";
        return resp;
    }

    // Parse status line
    auto first_line_end = raw.find("\r\n");
    std::string status_line = raw.substr(0, first_line_end);
    auto sp1 = status_line.find(' ');
    auto sp2 = status_line.find(' ', sp1 + 1);
    if (sp1 != std::string::npos && sp2 != std::string::npos) {
        resp.status_code = std::stoi(status_line.substr(sp1 + 1, sp2 - sp1 - 1));
        resp.status_text = status_line.substr(sp2 + 1);
    }

    // Parse headers
    std::string headers_str = raw.substr(first_line_end + 2, header_end - first_line_end - 2);
    std::istringstream hstream(headers_str);
    std::string hline;
    while (std::getline(hstream, hline)) {
        if (!hline.empty() && hline.back() == '\r') hline.pop_back();
        auto colon = hline.find(':');
        if (colon != std::string::npos) {
            std::string key = hline.substr(0, colon);
            std::string val = hline.substr(colon + 1);
            val.erase(0, val.find_first_not_of(' '));
            resp.headers[key] = val;
        }
    }

    // Body
    resp.body.assign(response_data.begin() + header_end + 4, response_data.end());
    resp.success = true;

    return resp;
}

HTTPResponse HTTPClient::get(const std::string& url) {
    HTTPRequest req;
    req.method = "GET";
    req.url = url;
    return request(req);
}

HTTPResponse HTTPClient::post(const std::string& url, const std::vector<byte>& body,
                               const std::string& content_type) {
    HTTPRequest req;
    req.method = "POST";
    req.url = url;
    req.body = body;
    req.headers["Content-Type"] = content_type;
    return request(req);
}

// ============================================================
// Register Networking Natives
// ============================================================

void register_network_natives(VM& vm) {

    vm.define_native("dns_resolve", [&vm](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* host = static_cast<ObjString*>(args[0].as_object<ObjHeader>());
        auto result = dns_resolve(host->value);

        auto* arr = new ObjArray();
        vm.gc().track(arr);
        for (auto& addr : result.addresses) {
            arr->push(make_string(vm.gc(), addr));
        }
        return Value::object(arr);
    }, 1);

    vm.define_native("http_get", [&vm](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* url = static_cast<ObjString*>(args[0].as_object<ObjHeader>());

        HTTPClient client;
        auto resp = client.get(url->value);

        auto* map = new ObjMap();
        vm.gc().track(map);
        map->set(make_string(vm.gc(), "status"), Value::integer(resp.status_code));
        map->set(make_string(vm.gc(), "status_text"), make_string(vm.gc(), resp.status_text));
        map->set(make_string(vm.gc(), "body"),
                 make_string(vm.gc(), std::string(resp.body.begin(), resp.body.end())));
        map->set(make_string(vm.gc(), "success"), Value::boolean(resp.success));
        if (!resp.error.empty()) {
            map->set(make_string(vm.gc(), "error"), make_string(vm.gc(), resp.error));
        }
        return Value::object(map);
    }, 1);

    vm.define_native("http_post", [&vm](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object()) return Value::nil();
        auto* url = static_cast<ObjString*>(args[0].as_object<ObjHeader>());
        std::string body_str = value_to_string(args[1]);
        std::vector<byte> body(body_str.begin(), body_str.end());

        std::string content_type = "application/json";
        if (argc >= 3 && args[2].is_object()) {
            content_type = static_cast<ObjString*>(args[2].as_object<ObjHeader>())->value;
        }

        HTTPClient client;
        auto resp = client.post(url->value, body, content_type);

        auto* map = new ObjMap();
        vm.gc().track(map);
        map->set(make_string(vm.gc(), "status"), Value::integer(resp.status_code));
        map->set(make_string(vm.gc(), "body"),
                 make_string(vm.gc(), std::string(resp.body.begin(), resp.body.end())));
        map->set(make_string(vm.gc(), "success"), Value::boolean(resp.success));
        return Value::object(map);
    }, 2);

    // ---- Channel ----
    vm.define_native("Channel", [&vm](int argc, Value* args) -> Value {
        size_t cap = (argc >= 1 && args[0].is_int()) ? static_cast<size_t>(args[0].as_int()) : 0;
        auto* chan = new ObjChannel(cap);
        vm.gc().track(chan);
        return Value::object(chan);
    }, 0);

    vm.define_native("channel_send", [](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object()) return Value::boolean(false);
        auto* chan = static_cast<ObjChannel*>(args[0].as_object<ObjHeader>());
        return Value::boolean(chan->send(args[1]));
    }, 2);

    vm.define_native("channel_recv", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* chan = static_cast<ObjChannel*>(args[0].as_object<ObjHeader>());
        Value result;
        if (chan->receive(result)) return result;
        return Value::nil();
    }, 1);

    vm.define_native("channel_close", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* chan = static_cast<ObjChannel*>(args[0].as_object<ObjHeader>());
        chan->close();
        return Value::nil();
    }, 1);
}

} // namespace grey
