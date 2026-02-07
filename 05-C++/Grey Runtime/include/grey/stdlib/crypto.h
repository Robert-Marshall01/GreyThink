// Grey Runtime - Standard Library: Crypto
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"
#include <vector>
#include <string>
#include <array>

namespace grey {

class VM;

// ============================================================
// Hashing
// ============================================================

namespace crypto {

// SHA-256
std::array<byte, 32> sha256(const byte* data, size_t len);
std::array<byte, 32> sha256(const std::string& str);
std::string sha256_hex(const byte* data, size_t len);
std::string sha256_hex(const std::string& str);

// Keccak-256 (for Solidity/Ethereum compatibility)
std::array<byte, 32> keccak256(const byte* data, size_t len);
std::string keccak256_hex(const byte* data, size_t len);

// HMAC-SHA256
std::array<byte, 32> hmac_sha256(const byte* key, size_t key_len,
                                  const byte* data, size_t data_len);

// ============================================================
// Symmetric Encryption (AES-256-GCM simplified)
// ============================================================

struct EncryptResult {
    std::vector<byte> ciphertext;
    std::array<byte, 12> nonce;
    std::array<byte, 16> tag;
    bool success;
};

EncryptResult aes256_encrypt(const byte* key, size_t key_len,
                              const byte* plaintext, size_t len);
std::vector<byte> aes256_decrypt(const byte* key, size_t key_len,
                                  const byte* ciphertext, size_t len,
                                  const byte* nonce, const byte* tag);

// ============================================================
// Digital Signatures (Ed25519 simplified)
// ============================================================

struct KeyPair {
    std::array<byte, 32> public_key;
    std::array<byte, 64> secret_key;
};

KeyPair generate_keypair();

std::array<byte, 64> sign(const byte* secret_key,
                           const byte* message, size_t msg_len);

bool verify(const byte* public_key,
            const byte* message, size_t msg_len,
            const byte* signature);

// ============================================================
// Random bytes (cryptographically secure)
// ============================================================

void random_bytes(byte* buffer, size_t len);
std::vector<byte> random_bytes(size_t len);

// ============================================================
// Utility
// ============================================================

std::string bytes_to_hex(const byte* data, size_t len);
std::vector<byte> hex_to_bytes(const std::string& hex);

} // namespace crypto

// ============================================================
// Register crypto natives
// ============================================================

void register_crypto_natives(VM& vm);

} // namespace grey
