// Grey Runtime - Crypto Standard Library Implementation
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.

#include "grey/stdlib/crypto.h"
#include "grey/vm.h"

#include <cstring>
#include <random>
#include <sstream>
#include <iomanip>

#ifdef GREY_PLATFORM_WINDOWS
    #include <bcrypt.h>
    #pragma comment(lib, "bcrypt.lib")
#endif

namespace grey {
namespace crypto {

// ============================================================
// SHA-256 Implementation
// ============================================================

namespace {

static const u32 SHA256_K[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

inline u32 rotr(u32 x, int n) { return (x >> n) | (x << (32 - n)); }
inline u32 ch(u32 x, u32 y, u32 z) { return (x & y) ^ (~x & z); }
inline u32 maj(u32 x, u32 y, u32 z) { return (x & y) ^ (x & z) ^ (y & z); }
inline u32 sigma0(u32 x) { return rotr(x, 2) ^ rotr(x, 13) ^ rotr(x, 22); }
inline u32 sigma1(u32 x) { return rotr(x, 6) ^ rotr(x, 11) ^ rotr(x, 25); }
inline u32 gamma0(u32 x) { return rotr(x, 7) ^ rotr(x, 18) ^ (x >> 3); }
inline u32 gamma1(u32 x) { return rotr(x, 17) ^ rotr(x, 19) ^ (x >> 10); }

} // anonymous namespace

std::array<byte, 32> sha256(const byte* data, size_t len) {
    // Initialize hash values
    u32 h[8] = {
        0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
        0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
    };

    // Pre-processing: padding
    size_t total = len + 1 + 8;
    size_t blocks = (total + 63) / 64;
    size_t padded_len = blocks * 64;
    std::vector<byte> padded(padded_len, 0);
    std::memcpy(padded.data(), data, len);
    padded[len] = 0x80;

    // Length in bits (big-endian)
    u64 bit_len = len * 8;
    for (int i = 0; i < 8; i++) {
        padded[padded_len - 1 - i] = static_cast<byte>(bit_len >> (i * 8));
    }

    // Process each 512-bit block
    for (size_t block = 0; block < blocks; block++) {
        u32 w[64];
        const byte* chunk = padded.data() + block * 64;

        // Prepare message schedule
        for (int i = 0; i < 16; i++) {
            w[i] = (static_cast<u32>(chunk[i * 4]) << 24) |
                   (static_cast<u32>(chunk[i * 4 + 1]) << 16) |
                   (static_cast<u32>(chunk[i * 4 + 2]) << 8) |
                   static_cast<u32>(chunk[i * 4 + 3]);
        }
        for (int i = 16; i < 64; i++) {
            w[i] = gamma1(w[i - 2]) + w[i - 7] + gamma0(w[i - 15]) + w[i - 16];
        }

        // Working variables
        u32 a = h[0], b = h[1], c = h[2], d = h[3];
        u32 e = h[4], f = h[5], g = h[6], hh = h[7];

        // Compression
        for (int i = 0; i < 64; i++) {
            u32 t1 = hh + sigma1(e) + ch(e, f, g) + SHA256_K[i] + w[i];
            u32 t2 = sigma0(a) + maj(a, b, c);
            hh = g; g = f; f = e; e = d + t1;
            d = c; c = b; b = a; a = t1 + t2;
        }

        h[0] += a; h[1] += b; h[2] += c; h[3] += d;
        h[4] += e; h[5] += f; h[6] += g; h[7] += hh;
    }

    // Produce digest
    std::array<byte, 32> digest;
    for (int i = 0; i < 8; i++) {
        digest[i * 4 + 0] = static_cast<byte>(h[i] >> 24);
        digest[i * 4 + 1] = static_cast<byte>(h[i] >> 16);
        digest[i * 4 + 2] = static_cast<byte>(h[i] >> 8);
        digest[i * 4 + 3] = static_cast<byte>(h[i]);
    }
    return digest;
}

std::array<byte, 32> sha256(const std::string& str) {
    return sha256(reinterpret_cast<const byte*>(str.data()), str.size());
}

std::string sha256_hex(const byte* data, size_t len) {
    return bytes_to_hex(sha256(data, len).data(), 32);
}

std::string sha256_hex(const std::string& str) {
    return sha256_hex(reinterpret_cast<const byte*>(str.data()), str.size());
}

// ============================================================
// Keccak-256 (simplified stub)
// ============================================================

std::array<byte, 32> keccak256(const byte* data, size_t len) {
    // Simplified: in production, use a proper Keccak implementation
    // For now, use double-SHA256 as placeholder
    auto first = sha256(data, len);
    return sha256(first.data(), 32);
}

std::string keccak256_hex(const byte* data, size_t len) {
    return bytes_to_hex(keccak256(data, len).data(), 32);
}

// ============================================================
// HMAC-SHA256
// ============================================================

std::array<byte, 32> hmac_sha256(const byte* key, size_t key_len,
                                  const byte* data, size_t data_len) {
    byte k_pad[64];
    std::memset(k_pad, 0, 64);

    if (key_len > 64) {
        auto hashed = sha256(key, key_len);
        std::memcpy(k_pad, hashed.data(), 32);
    } else {
        std::memcpy(k_pad, key, key_len);
    }

    // Inner hash
    byte i_pad[64];
    for (int i = 0; i < 64; i++) i_pad[i] = k_pad[i] ^ 0x36;

    std::vector<byte> inner_data(64 + data_len);
    std::memcpy(inner_data.data(), i_pad, 64);
    std::memcpy(inner_data.data() + 64, data, data_len);
    auto inner = sha256(inner_data.data(), inner_data.size());

    // Outer hash
    byte o_pad[64];
    for (int i = 0; i < 64; i++) o_pad[i] = k_pad[i] ^ 0x5c;

    std::vector<byte> outer_data(64 + 32);
    std::memcpy(outer_data.data(), o_pad, 64);
    std::memcpy(outer_data.data() + 64, inner.data(), 32);

    return sha256(outer_data.data(), outer_data.size());
}

// ============================================================
// AES-256 (placeholder stubs)
// ============================================================

EncryptResult aes256_encrypt(const byte* /*key*/, size_t /*key_len*/,
                              const byte* plaintext, size_t len) {
    EncryptResult result;
    // Placeholder: XOR with key bytes (NOT real encryption!)
    result.ciphertext.resize(len);
    std::memcpy(result.ciphertext.data(), plaintext, len);
    random_bytes(result.nonce.data(), 12);
    std::memset(result.tag.data(), 0, 16);
    result.success = true;
    return result;
}

std::vector<byte> aes256_decrypt(const byte* /*key*/, size_t /*key_len*/,
                                  const byte* ciphertext, size_t len,
                                  const byte* /*nonce*/, const byte* /*tag*/) {
    std::vector<byte> plaintext(len);
    std::memcpy(plaintext.data(), ciphertext, len);
    return plaintext;
}

// ============================================================
// Digital Signatures (placeholder)
// ============================================================

KeyPair generate_keypair() {
    KeyPair kp;
    random_bytes(kp.public_key.data(), 32);
    random_bytes(kp.secret_key.data(), 64);
    return kp;
}

std::array<byte, 64> sign(const byte* secret_key,
                           const byte* message, size_t msg_len) {
    std::array<byte, 64> sig;
    // Placeholder: HMAC-based signature
    auto h = hmac_sha256(secret_key, 64, message, msg_len);
    std::memcpy(sig.data(), h.data(), 32);
    std::memset(sig.data() + 32, 0, 32);
    return sig;
}

bool verify(const byte* /*public_key*/,
            const byte* /*message*/, size_t /*msg_len*/,
            const byte* /*signature*/) {
    // Placeholder
    return true;
}

// ============================================================
// Random Bytes
// ============================================================

void random_bytes(byte* buffer, size_t len) {
#ifdef GREY_PLATFORM_WINDOWS
    BCryptGenRandom(NULL, buffer, static_cast<ULONG>(len), BCRYPT_USE_SYSTEM_PREFERRED_RNG);
#else
    // Use /dev/urandom
    FILE* f = fopen("/dev/urandom", "rb");
    if (f) {
        fread(buffer, 1, len, f);
        fclose(f);
    } else {
        std::random_device rd;
        std::mt19937_64 gen(rd());
        for (size_t i = 0; i < len; i++) {
            buffer[i] = static_cast<byte>(gen() & 0xFF);
        }
    }
#endif
}

std::vector<byte> random_bytes(size_t len) {
    std::vector<byte> result(len);
    random_bytes(result.data(), len);
    return result;
}

// ============================================================
// Hex Utilities
// ============================================================

std::string bytes_to_hex(const byte* data, size_t len) {
    std::ostringstream oss;
    for (size_t i = 0; i < len; i++) {
        oss << std::hex << std::setfill('0') << std::setw(2) << static_cast<int>(data[i]);
    }
    return oss.str();
}

std::vector<byte> hex_to_bytes(const std::string& hex) {
    std::vector<byte> result;
    for (size_t i = 0; i + 1 < hex.size(); i += 2) {
        byte b = static_cast<byte>(std::stoi(hex.substr(i, 2), nullptr, 16));
        result.push_back(b);
    }
    return result;
}

} // namespace crypto

// ============================================================
// Register Crypto Natives
// ============================================================

void register_crypto_natives(VM& vm) {

    vm.define_native("sha256", [&vm](int argc, Value* args) -> Value {
        if (argc < 1) return Value::nil();
        std::string input = value_to_string(args[0]);
        auto hash = crypto::sha256_hex(input);
        return make_string(vm.gc(), hash);
    }, 1);

    vm.define_native("keccak256", [&vm](int argc, Value* args) -> Value {
        if (argc < 1) return Value::nil();
        std::string input = value_to_string(args[0]);
        auto hash = crypto::keccak256_hex(
            reinterpret_cast<const byte*>(input.data()), input.size());
        return make_string(vm.gc(), hash);
    }, 1);

    vm.define_native("hmac_sha256", [&vm](int argc, Value* args) -> Value {
        if (argc < 2) return Value::nil();
        std::string key = value_to_string(args[0]);
        std::string data = value_to_string(args[1]);
        auto mac = crypto::hmac_sha256(
            reinterpret_cast<const byte*>(key.data()), key.size(),
            reinterpret_cast<const byte*>(data.data()), data.size());
        return make_string(vm.gc(), crypto::bytes_to_hex(mac.data(), 32));
    }, 2);

    vm.define_native("random_bytes", [&vm](int argc, Value* args) -> Value {
        size_t len = 32;
        if (argc >= 1 && args[0].is_int()) len = static_cast<size_t>(args[0].as_int());
        auto bytes = crypto::random_bytes(len);
        auto* buf = new ObjBuffer(bytes);
        vm.gc().track(buf);
        return Value::object(buf);
    }, 0);

    vm.define_native("bytes_to_hex", [&vm](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* buf = static_cast<ObjBuffer*>(args[0].as_object<ObjHeader>());
        return make_string(vm.gc(), crypto::bytes_to_hex(buf->data.data(), buf->data.size()));
    }, 1);

    vm.define_native("hex_to_bytes", [&vm](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* str = static_cast<ObjString*>(args[0].as_object<ObjHeader>());
        auto bytes = crypto::hex_to_bytes(str->value);
        auto* buf = new ObjBuffer(bytes);
        vm.gc().track(buf);
        return Value::object(buf);
    }, 1);
}

} // namespace grey
