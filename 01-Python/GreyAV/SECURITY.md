# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 5.x.x   | :white_check_mark: |
| 4.x.x   | :white_check_mark: |
| 3.x.x   | :x:                |
| < 3.0   | :x:                |

## Reporting a Vulnerability

We take the security of GreyAV seriously. If you believe you have found a security vulnerability, please report it to us responsibly.

### How to Report

1. **Do NOT** open a public GitHub issue for security vulnerabilities
2. Email your findings to: security@greyav.io (or open a private security advisory on GitHub)
3. Include the following information:
   - Type of vulnerability (e.g., remote code execution, privilege escalation, bypass)
   - Full paths of source file(s) related to the vulnerability
   - Step-by-step instructions to reproduce the issue
   - Proof-of-concept or exploit code (if possible)
   - Impact assessment of the vulnerability

### What to Expect

- **Acknowledgment**: We will acknowledge receipt of your report within 48 hours
- **Assessment**: We will investigate and validate the vulnerability within 7 days
- **Resolution**: We aim to release a fix within 30 days for critical vulnerabilities
- **Credit**: We will credit you in the release notes (unless you prefer to remain anonymous)

### Scope

The following are in scope for security reports:

- GreyAV core scanner (`greyav.py`)
- All subsystem modules (threat engines, deception, mesh network, etc.)
- Quarantine encryption and file handling
- Real-time monitoring components
- Docker and Kubernetes configurations
- Installation scripts

### Out of Scope

- Denial of service attacks that require excessive resources
- Social engineering attacks
- Physical attacks
- Issues in dependencies (report these to the respective projects)

## Security Best Practices for Users

### Running GreyAV Securely

1. **Principle of Least Privilege**
   ```bash
   # Run scans as non-root when possible
   python3 greyav.py scan /path/to/scan
   
   # Only use root for operations that require it
   sudo python3 greyav.py rootkit
   sudo python3 greyav.py bootscan
   ```

2. **Protect Quarantine Directory**
   ```bash
   # Quarantine is encrypted, but also restrict access
   chmod 700 /path/to/greyav/quarantine
   ```

3. **Secure Configuration Files**
   ```bash
   # Protect signature and config files
   chmod 600 signatures.json exclusions.json
   ```

4. **Monitor Logs**
   ```bash
   # Regularly review scan logs for anomalies
   tail -f logs/greyav_service.log
   ```

5. **Keep Updated**
   ```bash
   # Check for updates regularly
   python3 greyav.py update
   ```

### Docker Security

```bash
# Run container with minimal privileges
docker run --read-only \
  --cap-drop=ALL \
  --cap-add=SYS_PTRACE \
  --security-opt=no-new-privileges:true \
  greyav:latest scan /scan
```

### Kubernetes Security

- Use the provided RBAC configurations in `k8s/rbac.yaml`
- Run pods with `securityContext` restrictions
- Use network policies to limit pod communication
- Store sensitive data in Kubernetes Secrets

## Security Features

GreyAV includes multiple security hardening features:

- **AES-256 Encryption**: Quarantined files are encrypted with PBKDF2-derived keys
- **Input Validation**: All paths and inputs are sanitized to prevent injection
- **Atomic Operations**: File writes use atomic rename to prevent corruption
- **Rate Limiting**: Built-in protection against abuse and log flooding
- **Secure Logging**: Automatic redaction of passwords, tokens, and keys
- **Circuit Breakers**: Automatic failure isolation to prevent cascade failures
- **Constant-Time Comparison**: HMAC verification uses timing-safe comparison

## Acknowledgments

We thank the following security researchers for responsibly disclosing vulnerabilities:

- *No disclosures yet - be the first!*

---

Thank you for helping keep GreyAV and its users safe!
