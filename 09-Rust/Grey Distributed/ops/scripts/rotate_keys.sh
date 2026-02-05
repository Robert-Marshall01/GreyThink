#!/usr/bin/env bash
# =============================================================================
# Grey Distributed — Key Rotation Script
# =============================================================================
#
# PURPOSE:
#   Rotate cryptographic keys used by Grey Distributed for:
#   - Node identity (mTLS certificates)
#   - TEE attestation keys
#   - Data encryption keys
#   - JWT signing keys
#
# USAGE:
#   ./rotate_keys.sh {identity|attestation|encryption|jwt|all} [options]
#
# EXAMPLES:
#   ./rotate_keys.sh identity                    # Rotate mTLS certificates
#   ./rotate_keys.sh attestation --node=grey-coordinator-0
#   ./rotate_keys.sh encryption --key-id=dek-123
#   ./rotate_keys.sh all --rolling              # Rotate all keys with rolling update
#
# OPTIONS:
#   --node=NODE       Target specific node (for per-node keys)
#   --key-id=ID       Target specific key ID
#   --rolling         Use rolling rotation (no downtime)
#   --immediate       Rotate immediately (may cause brief disruption)
#   --validity=DAYS   Validity period for new keys (default: 365)
#   --dry-run         Show what would be rotated
#   --force           Skip confirmation prompts
#   --backup          Backup old keys before rotation
#   --namespace=NS    Kubernetes namespace
#
# KEY TYPES:
#
#   IDENTITY KEYS (mTLS):
#     - Purpose: Node-to-node authentication
#     - Format: X.509 certificates
#     - Storage: Kubernetes secrets
#     - Rotation: Rolling, one node at a time
#
#   ATTESTATION KEYS:
#     - Purpose: TEE attestation and verification
#     - Format: Varies by platform (SGX, SEV, etc.)
#     - Storage: Hardware security module or enclave
#     - Rotation: Requires enclave restart
#
#   ENCRYPTION KEYS:
#     - Purpose: Data encryption at rest
#     - Format: AES-256, wrapped by KEK
#     - Storage: Key management service
#     - Rotation: Re-encrypt data with new key
#
#   JWT SIGNING KEYS:
#     - Purpose: API authentication tokens
#     - Format: RSA/EC key pairs
#     - Storage: Kubernetes secrets
#     - Rotation: Dual-publish for seamless transition
#
# SECURITY CONSIDERATIONS:
#   - Old keys are revoked after grace period
#   - Backup old keys to secure storage
#   - Audit all rotation events
#   - Monitor for rotation failures
#
# DEPENDENCIES:
#   - kubectl
#   - openssl
#   - jq
#   - curl
#
# =============================================================================

set -euo pipefail

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
NAMESPACE="${GREY_NAMESPACE:-grey-system}"
API_URL="${GREY_API_URL:-http://localhost:8080}"
DRY_RUN=false
FORCE=false
ROLLING=true
BACKUP=true
VALIDITY_DAYS=365
NODE=""
KEY_ID=""
GRACE_PERIOD_HOURS=24

# Paths
CA_SECRET="grey-ca"
TLS_SECRET_PREFIX="grey-tls-"
JWT_SECRET="grey-jwt-keys"
ENCRYPTION_SECRET="grey-encryption-keys"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
NC='\033[0m'

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[OK]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

log_key() {
    echo -e "${MAGENTA}[KEY]${NC} $*"
}

die() {
    log_error "$@"
    exit 1
}

kctl() {
    kubectl --namespace="$NAMESPACE" "$@"
}

require_cmd() {
    command -v "$1" >/dev/null 2>&1 || die "Required command not found: $1"
}

timestamp() {
    date -u +"%Y-%m-%dT%H:%M:%SZ"
}

# Create a temporary directory for key operations
setup_temp() {
    TEMP_DIR=$(mktemp -d)
    trap 'rm -rf "$TEMP_DIR"' EXIT
}

# Securely wipe a file
secure_delete() {
    local file="$1"
    if [[ -f "$file" ]]; then
        shred -u "$file" 2>/dev/null || rm -f "$file"
    fi
}

# API helper
api_call() {
    local method="$1"
    local endpoint="$2"
    shift 2
    
    curl -sf -X "$method" "${API_URL}${endpoint}" \
        -H "Content-Type: application/json" \
        "$@"
}

# -----------------------------------------------------------------------------
# Backup Operations
# -----------------------------------------------------------------------------

backup_secret() {
    local secret_name="$1"
    local backup_dir="${2:-/var/lib/grey/key-backups}"
    
    if [[ "$BACKUP" != "true" ]]; then
        return 0
    fi
    
    log_info "Backing up secret: $secret_name"
    
    local backup_file="${backup_dir}/${secret_name}-$(date +%Y%m%d%H%M%S).yaml"
    
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would backup to: $backup_file"
        return 0
    fi
    
    # Ensure backup directory exists
    mkdir -p "$backup_dir"
    chmod 700 "$backup_dir"
    
    # Export secret
    kctl get secret "$secret_name" -o yaml > "$backup_file"
    chmod 600 "$backup_file"
    
    log_success "Backed up to: $backup_file"
}

# -----------------------------------------------------------------------------
# Identity Key Rotation (mTLS Certificates)
# -----------------------------------------------------------------------------

rotate_identity_keys() {
    log_key "Starting identity key rotation..."
    
    # Get list of nodes
    local nodes
    if [[ -n "$NODE" ]]; then
        nodes="$NODE"
    else
        nodes=$(kctl get pods -l app.kubernetes.io/name=grey -o jsonpath='{.items[*].metadata.name}')
    fi
    
    local node_count
    node_count=$(echo "$nodes" | wc -w)
    log_info "Rotating certificates for $node_count node(s)"
    
    # Backup CA and existing certs
    backup_secret "$CA_SECRET"
    
    # Check if CA needs rotation (older than half validity period)
    local ca_needs_rotation=false
    if should_rotate_ca; then
        ca_needs_rotation=true
        log_warn "CA certificate approaching expiry — will be rotated"
    fi
    
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would rotate:"
        if [[ "$ca_needs_rotation" == "true" ]]; then
            log_info "  - CA certificate"
        fi
        for node in $nodes; do
            log_info "  - $node certificate"
        done
        return 0
    fi
    
    # Rotate CA if needed
    if [[ "$ca_needs_rotation" == "true" ]]; then
        rotate_ca_certificate
    fi
    
    # Rotate node certificates
    if [[ "$ROLLING" == "true" ]]; then
        rotate_node_certs_rolling "$nodes"
    else
        rotate_node_certs_immediate "$nodes"
    fi
    
    log_success "Identity key rotation complete"
}

# Check if CA should be rotated
should_rotate_ca() {
    local ca_cert
    ca_cert=$(kctl get secret "$CA_SECRET" -o jsonpath='{.data.tls\.crt}' | base64 -d)
    
    local expiry
    expiry=$(echo "$ca_cert" | openssl x509 -noout -enddate | cut -d= -f2)
    
    local expiry_epoch
    expiry_epoch=$(date -d "$expiry" +%s)
    
    local now_epoch
    now_epoch=$(date +%s)
    
    local days_remaining=$(( (expiry_epoch - now_epoch) / 86400 ))
    
    log_info "CA certificate expires in $days_remaining days"
    
    # Rotate if less than half of validity remains
    (( days_remaining < VALIDITY_DAYS / 2 ))
}

# Rotate CA certificate
rotate_ca_certificate() {
    log_key "Rotating CA certificate..."
    
    setup_temp
    
    # Generate new CA
    openssl genrsa -out "$TEMP_DIR/ca.key" 4096
    
    openssl req -x509 -new -nodes \
        -key "$TEMP_DIR/ca.key" \
        -sha256 \
        -days "$VALIDITY_DAYS" \
        -out "$TEMP_DIR/ca.crt" \
        -subj "/CN=Grey Distributed CA/O=Grey/OU=Security"
    
    # Update secret (keep old CA for transition)
    local old_ca
    old_ca=$(kctl get secret "$CA_SECRET" -o jsonpath='{.data.tls\.crt}' | base64 -d)
    
    # Create combined CA bundle
    cat "$TEMP_DIR/ca.crt" > "$TEMP_DIR/ca-bundle.crt"
    echo "$old_ca" >> "$TEMP_DIR/ca-bundle.crt"
    
    # Update secret
    kctl create secret generic "${CA_SECRET}-new" \
        --from-file=tls.crt="$TEMP_DIR/ca.crt" \
        --from-file=tls.key="$TEMP_DIR/ca.key" \
        --from-file=ca-bundle.crt="$TEMP_DIR/ca-bundle.crt" \
        --dry-run=client -o yaml | kctl apply -f -
    
    # Swap secrets
    kctl delete secret "$CA_SECRET" --wait=false
    kctl patch secret "${CA_SECRET}-new" -p '{"metadata":{"name":"'"$CA_SECRET"'"}}'
    
    log_success "CA certificate rotated"
}

# Rolling rotation of node certificates
rotate_node_certs_rolling() {
    local nodes="$1"
    
    for node in $nodes; do
        log_key "Rotating certificate for: $node"
        
        # Backup current cert
        local secret_name="${TLS_SECRET_PREFIX}${node}"
        backup_secret "$secret_name"
        
        # Generate new certificate
        generate_node_certificate "$node"
        
        # Trigger pod restart to pick up new cert
        kctl delete pod "$node" --wait=true
        
        # Wait for pod to be ready
        wait_for_pod_ready "$node"
        
        # Verify new certificate is in use
        verify_node_certificate "$node"
        
        log_success "Certificate rotated for: $node"
        
        # Brief pause between nodes
        sleep 5
    done
}

# Immediate rotation of all node certificates
rotate_node_certs_immediate() {
    local nodes="$1"
    
    log_warn "Immediate rotation may cause brief disruption"
    
    if [[ "$FORCE" != "true" ]]; then
        read -rp "Continue? [y/N] " confirm
        [[ "$confirm" =~ ^[Yy]$ ]] || die "Aborted"
    fi
    
    # Generate all certificates first
    for node in $nodes; do
        local secret_name="${TLS_SECRET_PREFIX}${node}"
        backup_secret "$secret_name"
        generate_node_certificate "$node"
    done
    
    # Trigger rolling restart
    kctl rollout restart deployment/grey-worker
    kctl rollout restart statefulset/grey-coordinator
    
    # Wait for rollout
    kctl rollout status deployment/grey-worker --timeout=300s
    kctl rollout status statefulset/grey-coordinator --timeout=300s
    
    log_success "All certificates rotated"
}

# Generate certificate for a specific node
generate_node_certificate() {
    local node="$1"
    
    setup_temp
    
    # Get CA key and cert
    kctl get secret "$CA_SECRET" -o jsonpath='{.data.tls\.key}' | base64 -d > "$TEMP_DIR/ca.key"
    kctl get secret "$CA_SECRET" -o jsonpath='{.data.tls\.crt}' | base64 -d > "$TEMP_DIR/ca.crt"
    
    # Generate node key
    openssl genrsa -out "$TEMP_DIR/node.key" 2048
    
    # Create CSR config
    cat > "$TEMP_DIR/csr.conf" <<EOF
[req]
default_bits = 2048
prompt = no
default_md = sha256
distinguished_name = dn
req_extensions = req_ext

[dn]
CN = ${node}
O = Grey
OU = Cluster

[req_ext]
subjectAltName = @alt_names

[alt_names]
DNS.1 = ${node}
DNS.2 = ${node}.grey-headless
DNS.3 = ${node}.grey-headless.${NAMESPACE}.svc.cluster.local
DNS.4 = *.grey-headless.${NAMESPACE}.svc.cluster.local
EOF

    # Generate CSR
    openssl req -new \
        -key "$TEMP_DIR/node.key" \
        -out "$TEMP_DIR/node.csr" \
        -config "$TEMP_DIR/csr.conf"
    
    # Sign certificate
    openssl x509 -req \
        -in "$TEMP_DIR/node.csr" \
        -CA "$TEMP_DIR/ca.crt" \
        -CAkey "$TEMP_DIR/ca.key" \
        -CAcreateserial \
        -out "$TEMP_DIR/node.crt" \
        -days "$VALIDITY_DAYS" \
        -sha256 \
        -extensions req_ext \
        -extfile "$TEMP_DIR/csr.conf"
    
    # Update/create secret
    local secret_name="${TLS_SECRET_PREFIX}${node}"
    kctl create secret tls "$secret_name" \
        --cert="$TEMP_DIR/node.crt" \
        --key="$TEMP_DIR/node.key" \
        --dry-run=client -o yaml | kctl apply -f -
    
    # Clean up sensitive material
    secure_delete "$TEMP_DIR/ca.key"
    secure_delete "$TEMP_DIR/node.key"
}

# Wait for pod to be ready
wait_for_pod_ready() {
    local pod="$1"
    local timeout=120
    
    log_info "Waiting for $pod to be ready..."
    kctl wait --for=condition=Ready pod/"$pod" --timeout="${timeout}s"
}

# Verify node is using new certificate
verify_node_certificate() {
    local node="$1"
    
    local pod_ip
    pod_ip=$(kctl get pod "$node" -o jsonpath='{.status.podIP}')
    
    # Connect and check certificate
    local cert_cn
    cert_cn=$(echo | openssl s_client -connect "$pod_ip:4100" 2>/dev/null | \
        openssl x509 -noout -subject 2>/dev/null | grep -o "CN=[^/]*" || echo "")
    
    if [[ "$cert_cn" == "CN=$node" ]]; then
        log_success "Certificate verified for $node"
    else
        log_warn "Could not verify certificate for $node"
    fi
}

# -----------------------------------------------------------------------------
# Attestation Key Rotation
# -----------------------------------------------------------------------------

rotate_attestation_keys() {
    log_key "Starting attestation key rotation..."
    
    local nodes
    if [[ -n "$NODE" ]]; then
        nodes="$NODE"
    else
        nodes=$(kctl get pods -l grey.io/tee=enabled -o jsonpath='{.items[*].metadata.name}')
    fi
    
    if [[ -z "$nodes" ]]; then
        log_warn "No TEE-enabled nodes found"
        return 0
    fi
    
    log_info "Rotating attestation keys for $(echo "$nodes" | wc -w) node(s)"
    
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would rotate attestation keys for:"
        for node in $nodes; do
            log_info "  - $node"
        done
        return 0
    fi
    
    # Attestation keys are stored in the enclave
    # Rotation requires enclave restart
    for node in $nodes; do
        log_key "Rotating attestation key for: $node"
        
        # Trigger attestation key regeneration via API
        api_call POST "/v1/security/attestation/rotate" \
            -d '{"node_id": "'"$node"'"}' || \
            log_warn "API call failed for $node"
        
        # Restart enclave container
        if [[ "$ROLLING" == "true" ]]; then
            kctl delete pod "$node" --wait=true
            wait_for_pod_ready "$node"
            
            # Verify new attestation
            verify_attestation "$node"
        fi
        
        log_success "Attestation key rotated for: $node"
    done
    
    log_success "Attestation key rotation complete"
}

# Verify node attestation
verify_attestation() {
    local node="$1"
    
    local result
    result=$(api_call GET "/v1/security/attestation/verify?node_id=$node" 2>/dev/null)
    
    local valid
    valid=$(echo "$result" | jq -r '.valid // false')
    
    if [[ "$valid" == "true" ]]; then
        log_success "Attestation verified for $node"
    else
        log_error "Attestation verification failed for $node"
    fi
}

# -----------------------------------------------------------------------------
# Encryption Key Rotation
# -----------------------------------------------------------------------------

rotate_encryption_keys() {
    log_key "Starting encryption key rotation..."
    
    # Get current key info
    local keys
    keys=$(api_call GET "/v1/security/encryption/keys" 2>/dev/null)
    
    if [[ -n "$KEY_ID" ]]; then
        log_info "Rotating specific key: $KEY_ID"
        rotate_single_encryption_key "$KEY_ID"
    else
        log_info "Rotating all encryption keys"
        
        local key_ids
        key_ids=$(echo "$keys" | jq -r '.keys[].id')
        
        for key_id in $key_ids; do
            rotate_single_encryption_key "$key_id"
        done
    fi
    
    log_success "Encryption key rotation complete"
}

rotate_single_encryption_key() {
    local key_id="$1"
    
    log_key "Rotating encryption key: $key_id"
    
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would rotate key: $key_id"
        return 0
    fi
    
    # Create new key version
    local result
    result=$(api_call POST "/v1/security/encryption/keys/$key_id/rotate" \
        -d '{"validity_days": '"$VALIDITY_DAYS"'}')
    
    local new_version
    new_version=$(echo "$result" | jq -r '.new_version')
    
    log_info "New key version: $new_version"
    
    # Trigger re-encryption of data
    log_info "Initiating data re-encryption..."
    api_call POST "/v1/security/encryption/reencrypt" \
        -d '{"key_id": "'"$key_id"'", "new_version": "'"$new_version"'"}'
    
    # Wait for re-encryption to complete
    local status="in_progress"
    while [[ "$status" == "in_progress" ]]; do
        sleep 10
        status=$(api_call GET "/v1/security/encryption/reencrypt/status" 2>/dev/null | jq -r '.status')
        log_info "Re-encryption status: $status"
    done
    
    if [[ "$status" == "completed" ]]; then
        log_success "Key $key_id rotated and data re-encrypted"
        
        # Revoke old version after grace period
        log_info "Old key version will be revoked in $GRACE_PERIOD_HOURS hours"
    else
        log_error "Re-encryption failed: $status"
    fi
}

# -----------------------------------------------------------------------------
# JWT Signing Key Rotation
# -----------------------------------------------------------------------------

rotate_jwt_keys() {
    log_key "Starting JWT signing key rotation..."
    
    backup_secret "$JWT_SECRET"
    
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would rotate JWT signing keys"
        return 0
    fi
    
    setup_temp
    
    # Generate new key pair
    openssl genrsa -out "$TEMP_DIR/jwt.key" 4096
    openssl rsa -in "$TEMP_DIR/jwt.key" -pubout -out "$TEMP_DIR/jwt.pub"
    
    # Get existing keys for dual-publish period
    local old_pub
    old_pub=$(kctl get secret "$JWT_SECRET" -o jsonpath='{.data.jwt\.pub}' | base64 -d 2>/dev/null || echo "")
    
    # Create JWKS with both old and new keys
    local new_kid
    new_kid="grey-jwt-$(date +%s)"
    
    # Update secret with both keys
    # The new key is primary, old key is for verification only
    kctl create secret generic "$JWT_SECRET" \
        --from-file=jwt.key="$TEMP_DIR/jwt.key" \
        --from-file=jwt.pub="$TEMP_DIR/jwt.pub" \
        --from-literal=jwt.kid="$new_kid" \
        --from-literal=jwt.pub.old="$old_pub" \
        --dry-run=client -o yaml | kctl apply -f -
    
    # Notify API servers to reload keys
    api_call POST "/v1/security/jwt/reload" || log_warn "JWT reload notification failed"
    
    # Schedule old key removal
    log_info "Old JWT key will be removed after $GRACE_PERIOD_HOURS hours"
    
    # Clean up
    secure_delete "$TEMP_DIR/jwt.key"
    
    log_success "JWT signing key rotated (kid: $new_kid)"
}

# -----------------------------------------------------------------------------
# Rotate All Keys
# -----------------------------------------------------------------------------

rotate_all_keys() {
    log_key "Starting full key rotation..."
    
    if [[ "$FORCE" != "true" ]]; then
        log_warn "This will rotate ALL keys in the cluster"
        read -rp "Are you sure? [yes/NO] " confirm
        [[ "$confirm" == "yes" ]] || die "Aborted"
    fi
    
    log_info "Step 1/4: Rotating identity keys..."
    rotate_identity_keys
    
    log_info "Step 2/4: Rotating attestation keys..."
    rotate_attestation_keys
    
    log_info "Step 3/4: Rotating encryption keys..."
    rotate_encryption_keys
    
    log_info "Step 4/4: Rotating JWT keys..."
    rotate_jwt_keys
    
    log_success "Full key rotation complete"
    
    # Record rotation event
    api_call POST "/v1/audit/events" \
        -d '{
            "type": "key_rotation",
            "scope": "all",
            "timestamp": "'"$(timestamp)"'",
            "operator": "'"${USER:-unknown}"'"
        }' || true
}

# -----------------------------------------------------------------------------
# Status and Reporting
# -----------------------------------------------------------------------------

show_key_status() {
    echo ""
    echo "Grey Distributed Key Status"
    echo "============================"
    echo ""
    
    # CA Certificate
    echo "CA Certificate:"
    if kctl get secret "$CA_SECRET" >/dev/null 2>&1; then
        local ca_expiry
        ca_expiry=$(kctl get secret "$CA_SECRET" -o jsonpath='{.data.tls\.crt}' | \
            base64 -d | openssl x509 -noout -enddate | cut -d= -f2)
        echo "  Expires: $ca_expiry"
    else
        echo "  Not found"
    fi
    echo ""
    
    # Node Certificates
    echo "Node Certificates:"
    for secret in $(kctl get secrets -l app.kubernetes.io/name=grey -o jsonpath='{.items[*].metadata.name}' | tr ' ' '\n' | grep "^grey-tls-"); do
        local node_expiry
        node_expiry=$(kctl get secret "$secret" -o jsonpath='{.data.tls\.crt}' | \
            base64 -d 2>/dev/null | openssl x509 -noout -enddate 2>/dev/null | cut -d= -f2 || echo "Invalid")
        echo "  $secret: $node_expiry"
    done
    echo ""
    
    # JWT Keys
    echo "JWT Signing Key:"
    if kctl get secret "$JWT_SECRET" >/dev/null 2>&1; then
        local jwt_kid
        jwt_kid=$(kctl get secret "$JWT_SECRET" -o jsonpath='{.data.jwt\.kid}' | base64 -d 2>/dev/null || echo "Unknown")
        echo "  Current KID: $jwt_kid"
    else
        echo "  Not found"
    fi
    echo ""
    
    # Encryption Keys
    echo "Encryption Keys:"
    local enc_keys
    enc_keys=$(api_call GET "/v1/security/encryption/keys" 2>/dev/null)
    if [[ -n "$enc_keys" ]]; then
        echo "$enc_keys" | jq -r '.keys[] | "  \(.id): v\(.version) (expires: \(.expires_at))"'
    else
        echo "  Could not retrieve"
    fi
    echo ""
}

# -----------------------------------------------------------------------------
# Argument Parsing
# -----------------------------------------------------------------------------

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --node=*)
                NODE="${1#*=}"
                shift
                ;;
            --key-id=*)
                KEY_ID="${1#*=}"
                shift
                ;;
            --rolling)
                ROLLING=true
                shift
                ;;
            --immediate)
                ROLLING=false
                shift
                ;;
            --validity=*)
                VALIDITY_DAYS="${1#*=}"
                shift
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --force)
                FORCE=true
                shift
                ;;
            --backup)
                BACKUP=true
                shift
                ;;
            --no-backup)
                BACKUP=false
                shift
                ;;
            --namespace=*)
                NAMESPACE="${1#*=}"
                shift
                ;;
            *)
                ARGS+=("$1")
                shift
                ;;
        esac
    done
}

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

usage() {
    cat <<EOF
Usage: $0 {identity|attestation|encryption|jwt|all|status} [options]

Key Types:
  identity      Rotate mTLS certificates
  attestation   Rotate TEE attestation keys
  encryption    Rotate data encryption keys
  jwt           Rotate JWT signing keys
  all           Rotate all key types
  status        Show current key status

Options:
  --node=NODE       Target specific node
  --key-id=ID       Target specific encryption key
  --rolling         Rolling rotation (default, no downtime)
  --immediate       Immediate rotation (brief disruption)
  --validity=DAYS   New key validity (default: 365)
  --dry-run         Show what would be rotated
  --force           Skip confirmations
  --backup          Backup old keys (default: true)
  --no-backup       Skip backup
  --namespace=NS    Kubernetes namespace

Examples:
  $0 status                               Show all key expirations
  $0 identity                             Rotate all node certificates
  $0 identity --node=grey-coordinator-0   Rotate specific node cert
  $0 encryption --key-id=dek-data         Rotate specific encryption key
  $0 all --rolling --force                Rotate everything

EOF
    exit 1
}

main() {
    ARGS=()
    parse_args "$@"
    
    require_cmd kubectl
    require_cmd openssl
    require_cmd jq
    require_cmd curl
    
    local action="${ARGS[0]:-}"
    
    case "$action" in
        identity)
            rotate_identity_keys
            ;;
        attestation)
            rotate_attestation_keys
            ;;
        encryption)
            rotate_encryption_keys
            ;;
        jwt)
            rotate_jwt_keys
            ;;
        all)
            rotate_all_keys
            ;;
        status)
            show_key_status
            ;;
        *)
            usage
            ;;
    esac
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
