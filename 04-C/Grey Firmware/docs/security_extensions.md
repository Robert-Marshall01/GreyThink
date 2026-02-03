# Security Extensions

## Overview

Security extensions provide cryptographic primitives and key management
for secure embedded applications. Built on industry-standard algorithms
with hardware acceleration support.

## Module Components

```
┌─────────────────────────────────────────────────────────────────────┐
│                       Security Extensions                           │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌──────────────┐   ┌──────────────┐   ┌──────────────────────┐    │
│  │     HMAC     │   │     AES      │   │     Key Vault        │    │
│  │              │   │              │   │                      │    │
│  │ • SHA-256    │   │ • 128/192/256│   │ • Secure storage     │    │
│  │ • SHA-384    │   │ • ECB/CBC/CTR│   │ • Access control     │    │
│  │ • Streaming  │   │ • GCM (AEAD) │   │ • Key derivation     │    │
│  │ • Verify     │   │ • HW accel   │   │ • Usage policies     │    │
│  └──────────────┘   └──────────────┘   └──────────────────────┘    │
│                                                                     │
│  Also see: Secure Bootloader (secure_boot.h)                        │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## HMAC Authentication

HMAC (Hash-based Message Authentication Code) provides data integrity
and authentication.

### Basic Usage

```c
#include "security/hmac.h"

uint8_t key[] = "secret_key_here";
uint8_t message[] = "data to authenticate";
uint8_t mac[GF_HMAC_SHA256_LEN];
size_t mac_len = sizeof(mac);

// Compute HMAC
gf_hmac(GF_HMAC_SHA256,
        key, sizeof(key) - 1,
        message, sizeof(message) - 1,
        mac, &mac_len);
```

### Verification (Constant-Time)

```c
// Verify received MAC
bool valid = gf_hmac_verify(GF_HMAC_SHA256,
                            key, key_len,
                            message, message_len,
                            received_mac, mac_len);

if (!valid) {
    // Authentication failed - reject message
}
```

### Streaming API (Large Messages)

```c
gf_hmac_ctx_t ctx;
gf_hmac_init(&ctx, GF_HMAC_SHA256, key, key_len);

// Process in chunks
gf_hmac_update(&ctx, chunk1, chunk1_len);
gf_hmac_update(&ctx, chunk2, chunk2_len);
gf_hmac_update(&ctx, chunk3, chunk3_len);

// Finalize
gf_hmac_final(&ctx, mac, &mac_len);
```

## AES Encryption

AES symmetric encryption with multiple modes.

### AES-GCM (Recommended for Most Uses)

GCM provides both encryption and authentication in one operation.

```c
#include "security/aes.h"

uint8_t key[32];    // AES-256 key
uint8_t nonce[12];  // MUST be unique for each encryption
uint8_t plaintext[] = "sensitive data";
uint8_t ciphertext[sizeof(plaintext)];
uint8_t tag[16];

// Encrypt with authentication
gf_aes_gcm_encrypt(key, GF_AES_256,
                   nonce,
                   NULL, 0,         // Additional auth data
                   plaintext, sizeof(plaintext),
                   ciphertext,
                   tag);

// Decrypt with verification
uint8_t decrypted[sizeof(plaintext)];
gf_aes_error_t err = gf_aes_gcm_decrypt(key, GF_AES_256,
                                        nonce,
                                        NULL, 0,
                                        ciphertext, sizeof(ciphertext),
                                        tag,
                                        decrypted);

if (err == GF_AES_ERR_TAG) {
    // Authentication failed - data tampered!
}
```

### AES-CTR (Stream Cipher Mode)

```c
gf_aes_ctx_t ctx;
uint8_t iv[16] = {0};  // Counter initial value

gf_aes_init(&ctx, GF_AES_CTR, key, GF_AES_128, iv);
gf_aes_encrypt(&ctx, plaintext, len, ciphertext);
```

### AES-CBC (Block Mode)

```c
uint8_t iv[16];  // Random IV
// Note: Plaintext length must be multiple of 16

gf_aes_init(&ctx, GF_AES_CBC, key, GF_AES_256, iv);
gf_aes_encrypt(&ctx, plaintext, len, ciphertext);
```

## Key Vault

Secure key storage with hardware protection when available.

### Key Generation

```c
#include "security/key_vault.h"

gf_vault_init();

// Generate device-unique AES key
gf_key_policy_t policy = {
    .type = GF_KEY_TYPE_AES,
    .size_bits = 256,
    .usage = GF_KEY_USAGE_ENCRYPT | GF_KEY_USAGE_DECRYPT,
    .exportable = false,    // Never exposed
    .lifetime_days = 0      // Permanent
};

gf_vault_generate("device_key", &policy);
```

### Key Import

```c
// Import provisioned key
uint8_t imported_key[32] = { /* from secure channel */ };

gf_key_policy_t policy = {
    .type = GF_KEY_TYPE_AES,
    .size_bits = 256,
    .usage = GF_KEY_USAGE_ENCRYPT | GF_KEY_USAGE_DECRYPT,
    .exportable = false
};

gf_vault_import("cloud_key", &policy, imported_key, 32);

// Overwrite source immediately
memset(imported_key, 0, sizeof(imported_key));
```

### Using Keys (Key Never Leaves Vault)

```c
// Encrypt using vault key (key material not exposed)
uint8_t plaintext[] = "secret data";
uint8_t ciphertext[256];
size_t cipher_len = sizeof(ciphertext);

gf_vault_encrypt("device_key",
                 plaintext, sizeof(plaintext),
                 ciphertext, &cipher_len);
```

### Key Derivation

```c
// Derive session key from master key
uint8_t context[] = "session_2024_001";

gf_key_policy_t session_policy = {
    .type = GF_KEY_TYPE_AES,
    .size_bits = 128,
    .usage = GF_KEY_USAGE_ENCRYPT | GF_KEY_USAGE_DECRYPT,
    .exportable = false,
    .lifetime_days = 1  // Auto-delete after 1 day
};

gf_vault_derive("master_key", "session_key",
                context, sizeof(context),
                &session_policy);
```

## Security Best Practices

### Nonce Management

```c
// CRITICAL: Never reuse nonces with the same key
typedef struct {
    uint8_t device_id[4];
    uint32_t counter;
    uint8_t random[4];
} nonce_t;

static uint32_t nonce_counter = 0;

void generate_nonce(uint8_t nonce[12]) {
    nonce_t n;
    memcpy(n.device_id, device_unique_id, 4);
    n.counter = nonce_counter++;
    get_random_bytes(n.random, 4);
    memcpy(nonce, &n, 12);
}
```

### Key Rotation

```c
void rotate_session_key(void) {
    // Generate new key
    gf_vault_generate("session_key_new", &session_policy);
    
    // Re-encrypt active secrets with new key
    // ...
    
    // Delete old key
    gf_vault_delete("session_key_old");
    
    // Rename keys
    // ...
}
```

### Secure Comparison

```c
// Always use constant-time comparison for secrets
bool check_password(const uint8_t *input, size_t len,
                   const uint8_t *stored, size_t stored_len) {
    if (len != stored_len) {
        // Still do comparison to prevent timing attack
        gf_secure_compare(input, stored, stored_len);
        return false;
    }
    return gf_secure_compare(input, stored, len);
}
```

## Hardware Acceleration

```c
// Check for crypto hardware
if (gf_aes_has_hw_accel()) {
    printf("AES hardware acceleration available\n");
    // All operations automatically use hardware
}
```

## Algorithm Selection Guide

| Use Case | Recommended Algorithm |
|----------|----------------------|
| Message authentication | HMAC-SHA256 |
| Authenticated encryption | AES-256-GCM |
| Bulk data encryption | AES-CTR or AES-GCM |
| Key derivation | HKDF (via vault_derive) |
| Firmware signatures | ECDSA P-256 (secure_boot) |

## Security Threat Mitigation

| Threat | Mitigation |
|--------|------------|
| Key extraction | Hardware-protected vault |
| Timing attacks | Constant-time operations |
| Nonce reuse | Counter + random nonce generation |
| Key compromise | Key rotation, derived keys |
| Memory attacks | Key zeroing after use |

## Integration with Other Modules

```c
// Secure MQTT messages
void publish_secure(const char *topic, const void *data, size_t len) {
    uint8_t nonce[12];
    generate_nonce(nonce);
    
    uint8_t ciphertext[len];
    uint8_t tag[16];
    size_t out_len;
    
    // Encrypt using vault key
    gf_vault_encrypt("mqtt_key", data, len, ciphertext, &out_len);
    
    // Publish with nonce prepended
    mqtt_publish(topic, nonce, 12);
    mqtt_publish_more(ciphertext, out_len);
}
```

## Related Modules

- [Secure Bootloader](secure_boot.md) - ECDSA firmware verification
- [Storage](storage.md) - Encrypted configuration storage
- [MQTT](mqtt.md) - Secure cloud communication
