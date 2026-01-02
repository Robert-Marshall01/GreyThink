#!/usr/bin/env python3
"""
Port Manager for GreyAV
=======================

Centralized port handling, categorization, and security analysis.
Provides comprehensive port classification, risk assessment, and service detection.

Author: GreyAV Team
License: MIT
"""

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Dict, Set, List, Optional, Tuple, Any
import re


class PortCategory(Enum):
    """Port category classification."""
    SYSTEM = auto()          # Well-known ports (0-1023)
    REGISTERED = auto()      # Registered ports (1024-49151)
    DYNAMIC = auto()         # Dynamic/Private ports (49152-65535)
    PRIVILEGED = auto()      # Requires root (0-1023)


class PortRisk(Enum):
    """Port risk level classification."""
    SAFE = 0                 # Known safe, encrypted, or internal
    LOW = 1                  # Generally safe but monitor
    MEDIUM = 2               # Could be abused, needs attention
    HIGH = 3                 # Known dangerous or legacy protocol
    CRITICAL = 4             # Active threat indicator, C2, or backdoor


class ProtocolSecurity(Enum):
    """Protocol security classification."""
    ENCRYPTED = auto()       # Uses TLS/SSL or native encryption
    PLAINTEXT = auto()       # Transmits data in cleartext
    MIXED = auto()           # Can be either depending on config
    UNKNOWN = auto()         # Security status unknown


@dataclass
class PortDefinition:
    """Complete definition of a port and its characteristics."""
    port: int
    name: str
    protocol: str = "tcp"
    description: str = ""
    risk: PortRisk = PortRisk.LOW
    security: ProtocolSecurity = ProtocolSecurity.UNKNOWN
    common_process: Optional[str] = None
    mitre_attack_id: Optional[str] = None
    alternatives: List[int] = field(default_factory=list)
    secure_alternative: Optional[int] = None
    tags: Set[str] = field(default_factory=set)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "port": self.port,
            "name": self.name,
            "protocol": self.protocol,
            "description": self.description,
            "risk": self.risk.name,
            "security": self.security.name,
            "common_process": self.common_process,
            "mitre_attack_id": self.mitre_attack_id,
            "alternatives": self.alternatives,
            "secure_alternative": self.secure_alternative,
            "tags": list(self.tags)
        }


class PortManager:
    """
    Centralized port management and security analysis.
    
    Provides:
    - Comprehensive port database with risk levels
    - Port categorization and classification
    - Service detection and process mapping
    - Security recommendations
    - C2 and backdoor detection
    """
    
    # ==================== STANDARD SAFE PORTS ====================
    # Ports that are generally safe and expected on most systems
    
    SAFE_PORTS: Dict[int, PortDefinition] = {
        # SSH
        22: PortDefinition(
            port=22, name="ssh", protocol="tcp",
            description="Secure Shell - encrypted remote access",
            risk=PortRisk.SAFE, security=ProtocolSecurity.ENCRYPTED,
            common_process="sshd", tags={"remote-access", "encrypted", "admin"}
        ),
        
        # DNS
        53: PortDefinition(
            port=53, name="dns", protocol="tcp/udp",
            description="Domain Name System",
            risk=PortRisk.SAFE, security=ProtocolSecurity.MIXED,
            common_process="named,systemd-resolved,dnsmasq",
            tags={"network-infrastructure", "essential"}
        ),
        
        # HTTP/HTTPS
        80: PortDefinition(
            port=80, name="http", protocol="tcp",
            description="Hypertext Transfer Protocol",
            risk=PortRisk.LOW, security=ProtocolSecurity.PLAINTEXT,
            common_process="nginx,apache2,httpd",
            secure_alternative=443,
            tags={"web", "common"}
        ),
        443: PortDefinition(
            port=443, name="https", protocol="tcp",
            description="HTTP Secure (TLS/SSL)",
            risk=PortRisk.SAFE, security=ProtocolSecurity.ENCRYPTED,
            common_process="nginx,apache2,httpd",
            tags={"web", "encrypted", "common"}
        ),
        
        # Email (Secure)
        465: PortDefinition(
            port=465, name="smtps", protocol="tcp",
            description="SMTP over TLS",
            risk=PortRisk.SAFE, security=ProtocolSecurity.ENCRYPTED,
            common_process="postfix,exim",
            tags={"email", "encrypted"}
        ),
        587: PortDefinition(
            port=587, name="submission", protocol="tcp",
            description="Email message submission",
            risk=PortRisk.SAFE, security=ProtocolSecurity.ENCRYPTED,
            common_process="postfix,exim",
            tags={"email", "encrypted"}
        ),
        993: PortDefinition(
            port=993, name="imaps", protocol="tcp",
            description="IMAP over TLS",
            risk=PortRisk.SAFE, security=ProtocolSecurity.ENCRYPTED,
            common_process="dovecot",
            tags={"email", "encrypted"}
        ),
        995: PortDefinition(
            port=995, name="pop3s", protocol="tcp",
            description="POP3 over TLS",
            risk=PortRisk.SAFE, security=ProtocolSecurity.ENCRYPTED,
            common_process="dovecot",
            tags={"email", "encrypted"}
        ),
        
        # LDAP Secure
        636: PortDefinition(
            port=636, name="ldaps", protocol="tcp",
            description="LDAP over TLS",
            risk=PortRisk.SAFE, security=ProtocolSecurity.ENCRYPTED,
            common_process="slapd",
            tags={"directory", "encrypted", "auth"}
        ),
        
        # Time Sync
        123: PortDefinition(
            port=123, name="ntp", protocol="udp",
            description="Network Time Protocol",
            risk=PortRisk.SAFE, security=ProtocolSecurity.PLAINTEXT,
            common_process="ntpd,chronyd,systemd-timesyncd",
            tags={"time", "essential"}
        ),
        
        # DHCP
        67: PortDefinition(
            port=67, name="dhcp-server", protocol="udp",
            description="DHCP Server",
            risk=PortRisk.SAFE, security=ProtocolSecurity.PLAINTEXT,
            common_process="dhcpd,dnsmasq",
            tags={"network-infrastructure"}
        ),
        68: PortDefinition(
            port=68, name="dhcp-client", protocol="udp",
            description="DHCP Client",
            risk=PortRisk.SAFE, security=ProtocolSecurity.PLAINTEXT,
            common_process="dhclient,NetworkManager",
            tags={"network-infrastructure"}
        ),
        
        # Printing (Local)
        631: PortDefinition(
            port=631, name="ipp", protocol="tcp",
            description="Internet Printing Protocol (CUPS)",
            risk=PortRisk.SAFE, security=ProtocolSecurity.MIXED,
            common_process="cupsd",
            tags={"printing", "local"}
        ),
        
        # Local services
        5353: PortDefinition(
            port=5353, name="mdns", protocol="udp",
            description="Multicast DNS (Avahi/Bonjour)",
            risk=PortRisk.SAFE, security=ProtocolSecurity.PLAINTEXT,
            common_process="avahi-daemon",
            tags={"discovery", "local"}
        ),
    }
    
    # ==================== COMMON SERVICE PORTS ====================
    # Ports commonly used by services, need monitoring
    
    COMMON_PORTS: Dict[int, PortDefinition] = {
        # Alternative web ports
        8000: PortDefinition(
            port=8000, name="http-alt", protocol="tcp",
            description="Alternative HTTP (development)",
            risk=PortRisk.LOW, security=ProtocolSecurity.PLAINTEXT,
            common_process="python,node,java",
            tags={"web", "development", "common"}
        ),
        8080: PortDefinition(
            port=8080, name="http-proxy", protocol="tcp",
            description="HTTP Proxy / Alternative HTTP",
            risk=PortRisk.LOW, security=ProtocolSecurity.PLAINTEXT,
            common_process="java,nginx,squid",
            tags={"web", "proxy", "common"}
        ),
        8443: PortDefinition(
            port=8443, name="https-alt", protocol="tcp",
            description="Alternative HTTPS",
            risk=PortRisk.SAFE, security=ProtocolSecurity.ENCRYPTED,
            common_process="java,tomcat",
            tags={"web", "encrypted", "common"}
        ),
        
        # Databases
        3306: PortDefinition(
            port=3306, name="mysql", protocol="tcp",
            description="MySQL/MariaDB Database",
            risk=PortRisk.MEDIUM, security=ProtocolSecurity.MIXED,
            common_process="mysqld,mariadbd",
            tags={"database", "should-be-local"}
        ),
        5432: PortDefinition(
            port=5432, name="postgresql", protocol="tcp",
            description="PostgreSQL Database",
            risk=PortRisk.MEDIUM, security=ProtocolSecurity.MIXED,
            common_process="postgres",
            tags={"database", "should-be-local"}
        ),
        6379: PortDefinition(
            port=6379, name="redis", protocol="tcp",
            description="Redis Key-Value Store",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="redis-server",
            mitre_attack_id="T1190",
            tags={"database", "cache", "no-auth-by-default"}
        ),
        27017: PortDefinition(
            port=27017, name="mongodb", protocol="tcp",
            description="MongoDB Database",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="mongod",
            mitre_attack_id="T1190",
            tags={"database", "no-auth-by-default"}
        ),
        11211: PortDefinition(
            port=11211, name="memcached", protocol="tcp/udp",
            description="Memcached",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="memcached",
            mitre_attack_id="T1498",
            tags={"cache", "no-auth-by-default"}
        ),
        9200: PortDefinition(
            port=9200, name="elasticsearch", protocol="tcp",
            description="Elasticsearch HTTP API",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="java",
            tags={"database", "search", "no-auth-by-default"}
        ),
        
        # Message Queues
        5672: PortDefinition(
            port=5672, name="amqp", protocol="tcp",
            description="RabbitMQ/AMQP",
            risk=PortRisk.MEDIUM, security=ProtocolSecurity.MIXED,
            common_process="rabbitmq",
            tags={"messaging"}
        ),
        9092: PortDefinition(
            port=9092, name="kafka", protocol="tcp",
            description="Apache Kafka",
            risk=PortRisk.MEDIUM, security=ProtocolSecurity.PLAINTEXT,
            common_process="java",
            tags={"messaging"}
        ),
        
        # Application Servers
        9000: PortDefinition(
            port=9000, name="php-fpm", protocol="tcp",
            description="PHP-FPM FastCGI",
            risk=PortRisk.MEDIUM, security=ProtocolSecurity.PLAINTEXT,
            common_process="php-fpm",
            tags={"application", "php"}
        ),
        3000: PortDefinition(
            port=3000, name="node-default", protocol="tcp",
            description="Node.js default development port",
            risk=PortRisk.LOW, security=ProtocolSecurity.PLAINTEXT,
            common_process="node",
            tags={"development", "node"}
        ),
        4200: PortDefinition(
            port=4200, name="angular", protocol="tcp",
            description="Angular development server",
            risk=PortRisk.LOW, security=ProtocolSecurity.PLAINTEXT,
            common_process="node,ng",
            tags={"development", "angular"}
        ),
        5000: PortDefinition(
            port=5000, name="flask", protocol="tcp",
            description="Flask/Python development server",
            risk=PortRisk.LOW, security=ProtocolSecurity.PLAINTEXT,
            common_process="python",
            tags={"development", "python"}
        ),
        8888: PortDefinition(
            port=8888, name="jupyter", protocol="tcp",
            description="Jupyter Notebook",
            risk=PortRisk.MEDIUM, security=ProtocolSecurity.PLAINTEXT,
            common_process="python",
            tags={"development", "data-science"}
        ),
        
        # Container/Orchestration
        2375: PortDefinition(
            port=2375, name="docker", protocol="tcp",
            description="Docker daemon (unencrypted)",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.PLAINTEXT,
            common_process="dockerd",
            mitre_attack_id="T1610",
            tags={"container", "dangerous-if-exposed"}
        ),
        2376: PortDefinition(
            port=2376, name="docker-tls", protocol="tcp",
            description="Docker daemon (TLS)",
            risk=PortRisk.MEDIUM, security=ProtocolSecurity.ENCRYPTED,
            common_process="dockerd",
            tags={"container", "encrypted"}
        ),
        6443: PortDefinition(
            port=6443, name="kubernetes-api", protocol="tcp",
            description="Kubernetes API Server",
            risk=PortRisk.MEDIUM, security=ProtocolSecurity.ENCRYPTED,
            common_process="kube-apiserver",
            tags={"container", "orchestration"}
        ),
        10250: PortDefinition(
            port=10250, name="kubelet", protocol="tcp",
            description="Kubernetes Kubelet API",
            risk=PortRisk.HIGH, security=ProtocolSecurity.ENCRYPTED,
            common_process="kubelet",
            mitre_attack_id="T1609",
            tags={"container", "orchestration"}
        ),
        
        # Monitoring
        9090: PortDefinition(
            port=9090, name="prometheus", protocol="tcp",
            description="Prometheus/Cockpit",
            risk=PortRisk.LOW, security=ProtocolSecurity.PLAINTEXT,
            common_process="prometheus",
            tags={"monitoring"}
        ),
        3100: PortDefinition(
            port=3100, name="loki", protocol="tcp",
            description="Grafana Loki",
            risk=PortRisk.LOW, security=ProtocolSecurity.PLAINTEXT,
            common_process="loki",
            tags={"monitoring", "logging"}
        ),
        
        # Remote Access
        2222: PortDefinition(
            port=2222, name="ssh-alt", protocol="tcp",
            description="Alternative SSH port",
            risk=PortRisk.SAFE, security=ProtocolSecurity.ENCRYPTED,
            common_process="sshd",
            tags={"remote-access", "encrypted"}
        ),
        
        # VPN
        1194: PortDefinition(
            port=1194, name="openvpn", protocol="udp",
            description="OpenVPN",
            risk=PortRisk.SAFE, security=ProtocolSecurity.ENCRYPTED,
            common_process="openvpn",
            tags={"vpn", "encrypted"}
        ),
        51820: PortDefinition(
            port=51820, name="wireguard", protocol="udp",
            description="WireGuard VPN",
            risk=PortRisk.SAFE, security=ProtocolSecurity.ENCRYPTED,
            common_process="wg",
            tags={"vpn", "encrypted"}
        ),
    }
    
    # ==================== DANGEROUS PORTS ====================
    # Ports that are inherently dangerous or indicators of compromise
    
    DANGEROUS_PORTS: Dict[int, PortDefinition] = {
        # Unencrypted legacy protocols
        20: PortDefinition(
            port=20, name="ftp-data", protocol="tcp",
            description="FTP Data (plaintext)",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="vsftpd,proftpd",
            secure_alternative=22,
            tags={"legacy", "plaintext", "deprecated"}
        ),
        21: PortDefinition(
            port=21, name="ftp", protocol="tcp",
            description="FTP Control (plaintext)",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="vsftpd,proftpd",
            secure_alternative=22,
            tags={"legacy", "plaintext", "deprecated"}
        ),
        23: PortDefinition(
            port=23, name="telnet", protocol="tcp",
            description="Telnet (plaintext)",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.PLAINTEXT,
            common_process="telnetd,inetd",
            secure_alternative=22,
            mitre_attack_id="T1021.002",
            tags={"legacy", "plaintext", "deprecated", "never-use"}
        ),
        25: PortDefinition(
            port=25, name="smtp", protocol="tcp",
            description="SMTP (can be relay/spam)",
            risk=PortRisk.MEDIUM, security=ProtocolSecurity.PLAINTEXT,
            common_process="postfix,exim,sendmail",
            secure_alternative=587,
            tags={"email", "requires-config"}
        ),
        69: PortDefinition(
            port=69, name="tftp", protocol="udp",
            description="TFTP (no authentication)",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="tftpd",
            mitre_attack_id="T1187",
            tags={"legacy", "no-auth"}
        ),
        110: PortDefinition(
            port=110, name="pop3", protocol="tcp",
            description="POP3 (plaintext)",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="dovecot",
            secure_alternative=995,
            tags={"email", "plaintext", "deprecated"}
        ),
        143: PortDefinition(
            port=143, name="imap", protocol="tcp",
            description="IMAP (plaintext)",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="dovecot",
            secure_alternative=993,
            tags={"email", "plaintext", "deprecated"}
        ),
        
        # Windows services on Linux = suspicious
        135: PortDefinition(
            port=135, name="msrpc", protocol="tcp",
            description="Windows RPC",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            mitre_attack_id="T1021.006",
            tags={"windows", "suspicious-on-linux"}
        ),
        137: PortDefinition(
            port=137, name="netbios-ns", protocol="udp",
            description="NetBIOS Name Service",
            risk=PortRisk.MEDIUM, security=ProtocolSecurity.PLAINTEXT,
            common_process="nmbd,samba",
            tags={"windows", "smb"}
        ),
        138: PortDefinition(
            port=138, name="netbios-dgm", protocol="udp",
            description="NetBIOS Datagram",
            risk=PortRisk.MEDIUM, security=ProtocolSecurity.PLAINTEXT,
            common_process="nmbd,samba",
            tags={"windows", "smb"}
        ),
        139: PortDefinition(
            port=139, name="netbios-ssn", protocol="tcp",
            description="NetBIOS Session",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="smbd,samba",
            mitre_attack_id="T1021.002",
            tags={"windows", "smb", "legacy"}
        ),
        445: PortDefinition(
            port=445, name="microsoft-ds", protocol="tcp",
            description="SMB/CIFS",
            risk=PortRisk.HIGH, security=ProtocolSecurity.MIXED,
            common_process="smbd,samba",
            mitre_attack_id="T1021.002",
            tags={"windows", "smb", "ransomware-vector"}
        ),
        3389: PortDefinition(
            port=3389, name="rdp", protocol="tcp",
            description="Remote Desktop Protocol",
            risk=PortRisk.HIGH, security=ProtocolSecurity.ENCRYPTED,
            common_process="xrdp",
            mitre_attack_id="T1021.001",
            tags={"remote-access", "windows", "brute-force-target"}
        ),
        
        # Dangerous services
        111: PortDefinition(
            port=111, name="rpcbind", protocol="tcp/udp",
            description="RPC Portmapper",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="rpcbind",
            mitre_attack_id="T1210",
            tags={"legacy", "rpc"}
        ),
        512: PortDefinition(
            port=512, name="rexec", protocol="tcp",
            description="Remote Execution",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.PLAINTEXT,
            mitre_attack_id="T1021",
            tags={"legacy", "never-use", "deprecated"}
        ),
        513: PortDefinition(
            port=513, name="rlogin", protocol="tcp",
            description="Remote Login",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.PLAINTEXT,
            secure_alternative=22,
            mitre_attack_id="T1021",
            tags={"legacy", "never-use", "deprecated"}
        ),
        514: PortDefinition(
            port=514, name="rsh", protocol="tcp",
            description="Remote Shell",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.PLAINTEXT,
            secure_alternative=22,
            mitre_attack_id="T1021",
            tags={"legacy", "never-use", "deprecated"}
        ),
        2049: PortDefinition(
            port=2049, name="nfs", protocol="tcp/udp",
            description="Network File System",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="nfsd",
            mitre_attack_id="T1021.002",
            tags={"file-sharing", "requires-config"}
        ),
        
        # Remote desktop/VNC
        5900: PortDefinition(
            port=5900, name="vnc", protocol="tcp",
            description="VNC Remote Desktop",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="vncserver,x11vnc",
            mitre_attack_id="T1021.005",
            tags={"remote-access", "brute-force-target"}
        ),
        5901: PortDefinition(
            port=5901, name="vnc-1", protocol="tcp",
            description="VNC Display :1",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="vncserver",
            tags={"remote-access"}
        ),
        
        # IRC (often used by botnets)
        6667: PortDefinition(
            port=6667, name="irc", protocol="tcp",
            description="IRC (often botnet C2)",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            common_process="ircd",
            mitre_attack_id="T1071.001",
            tags={"chat", "botnet-indicator"}
        ),
        6668: PortDefinition(
            port=6668, name="irc-alt", protocol="tcp",
            description="IRC Alternative",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            tags={"chat", "botnet-indicator"}
        ),
        6669: PortDefinition(
            port=6669, name="irc-alt", protocol="tcp",
            description="IRC Alternative",
            risk=PortRisk.HIGH, security=ProtocolSecurity.PLAINTEXT,
            tags={"chat", "botnet-indicator"}
        ),
    }
    
    # ==================== C2 AND BACKDOOR PORTS ====================
    # Known Command & Control and backdoor ports
    
    C2_BACKDOOR_PORTS: Dict[int, PortDefinition] = {
        1234: PortDefinition(
            port=1234, name="common-backdoor", protocol="tcp",
            description="Common backdoor port",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.UNKNOWN,
            mitre_attack_id="T1571",
            tags={"backdoor", "c2", "malware"}
        ),
        4444: PortDefinition(
            port=4444, name="metasploit-default", protocol="tcp",
            description="Metasploit Meterpreter default",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.UNKNOWN,
            mitre_attack_id="T1571",
            tags={"backdoor", "c2", "metasploit", "pentest"}
        ),
        4445: PortDefinition(
            port=4445, name="metasploit-alt", protocol="tcp",
            description="Metasploit alternative",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.UNKNOWN,
            mitre_attack_id="T1571",
            tags={"backdoor", "c2", "metasploit"}
        ),
        5555: PortDefinition(
            port=5555, name="android-adb", protocol="tcp",
            description="Android ADB / Various backdoors",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.PLAINTEXT,
            mitre_attack_id="T1571",
            tags={"backdoor", "android", "debug"}
        ),
        6666: PortDefinition(
            port=6666, name="backdoor-common", protocol="tcp",
            description="Common backdoor/IRC port",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.UNKNOWN,
            mitre_attack_id="T1571",
            tags={"backdoor", "c2"}
        ),
        7777: PortDefinition(
            port=7777, name="backdoor-alt", protocol="tcp",
            description="Common backdoor port",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.UNKNOWN,
            mitre_attack_id="T1571",
            tags={"backdoor", "c2"}
        ),
        8888: PortDefinition(
            port=8888, name="sun-proxy-admin", protocol="tcp",
            description="Sun Proxy Admin / Jupyter / Backdoors",
            risk=PortRisk.MEDIUM, security=ProtocolSecurity.UNKNOWN,
            tags={"backdoor", "c2", "development"}
        ),
        9999: PortDefinition(
            port=9999, name="abyss", protocol="tcp",
            description="Abyss Web Server / Backdoors",
            risk=PortRisk.HIGH, security=ProtocolSecurity.UNKNOWN,
            mitre_attack_id="T1571",
            tags={"backdoor", "c2"}
        ),
        12345: PortDefinition(
            port=12345, name="netbus", protocol="tcp",
            description="NetBus Trojan",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.UNKNOWN,
            mitre_attack_id="T1571",
            tags={"trojan", "backdoor", "malware"}
        ),
        27374: PortDefinition(
            port=27374, name="subseven", protocol="tcp",
            description="SubSeven Trojan",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.UNKNOWN,
            mitre_attack_id="T1571",
            tags={"trojan", "backdoor", "malware"}
        ),
        31337: PortDefinition(
            port=31337, name="elite", protocol="tcp",
            description="Elite/Back Orifice",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.UNKNOWN,
            mitre_attack_id="T1571",
            tags={"trojan", "backdoor", "malware", "classic"}
        ),
        31338: PortDefinition(
            port=31338, name="back-orifice", protocol="tcp",
            description="Back Orifice alternate",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.UNKNOWN,
            mitre_attack_id="T1571",
            tags={"trojan", "backdoor", "malware"}
        ),
        
        # Cobalt Strike indicators
        50050: PortDefinition(
            port=50050, name="cobalt-strike", protocol="tcp",
            description="Cobalt Strike Team Server",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.ENCRYPTED,
            mitre_attack_id="S0154",
            tags={"c2", "apt", "cobalt-strike"}
        ),
        
        # Note: 8443 removed as it's commonly used for legitimate HTTPS
        # Empire C2 detection is done through behavior analysis
        
        # Note: Port 443 is NOT included here - it's essential
        # Reverse shells using 443 are detected by behavior, not port blocking
        
        # Tor
        9001: PortDefinition(
            port=9001, name="tor-relay", protocol="tcp",
            description="Tor Relay",
            risk=PortRisk.HIGH, security=ProtocolSecurity.ENCRYPTED,
            common_process="tor",
            mitre_attack_id="T1090",
            tags={"anonymization", "tor", "suspicious"}
        ),
        9050: PortDefinition(
            port=9050, name="tor-socks", protocol="tcp",
            description="Tor SOCKS proxy",
            risk=PortRisk.HIGH, security=ProtocolSecurity.ENCRYPTED,
            common_process="tor",
            tags={"anonymization", "tor", "proxy"}
        ),
        
        # Cryptominer indicators
        3333: PortDefinition(
            port=3333, name="stratum", protocol="tcp",
            description="Mining Stratum protocol",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.MIXED,
            mitre_attack_id="T1496",
            tags={"cryptominer", "malware"}
        ),
        14444: PortDefinition(
            port=14444, name="monero-stratum", protocol="tcp",
            description="Monero Stratum mining",
            risk=PortRisk.CRITICAL, security=ProtocolSecurity.MIXED,
            mitre_attack_id="T1496",
            tags={"cryptominer", "malware", "monero"}
        ),
    }
    
    def __init__(self):
        """Initialize PortManager with combined port database."""
        self._all_ports: Dict[int, PortDefinition] = {}
        self._build_port_database()
        
        # Build lookup sets for quick queries
        self._safe_ports: Set[int] = set(self.SAFE_PORTS.keys())
        self._common_ports: Set[int] = set(self.COMMON_PORTS.keys())
        self._dangerous_ports: Set[int] = set(self.DANGEROUS_PORTS.keys())
        self._c2_ports: Set[int] = set(self.C2_BACKDOOR_PORTS.keys())
        
        # All known ports
        self._known_ports: Set[int] = (
            self._safe_ports | 
            self._common_ports | 
            self._dangerous_ports | 
            self._c2_ports
        )
    
    def _build_port_database(self):
        """Build complete port database from all categories."""
        # Priority: C2 > Dangerous > Common > Safe
        self._all_ports.update(self.SAFE_PORTS)
        self._all_ports.update(self.COMMON_PORTS)
        self._all_ports.update(self.DANGEROUS_PORTS)
        self._all_ports.update(self.C2_BACKDOOR_PORTS)
    
    def get_port_info(self, port: int) -> Optional[PortDefinition]:
        """Get information about a specific port."""
        return self._all_ports.get(port)
    
    def get_service_name(self, port: int) -> str:
        """Get the service name for a port."""
        info = self.get_port_info(port)
        return info.name if info else "unknown"
    
    def get_port_risk(self, port: int) -> PortRisk:
        """Get the risk level for a port."""
        info = self.get_port_info(port)
        if info:
            return info.risk
        
        # Unknown ports - assess by range
        if port < 1024:
            return PortRisk.MEDIUM  # System port, unexpected
        elif port > 49151:
            return PortRisk.LOW  # Dynamic range
        else:
            return PortRisk.MEDIUM  # Unknown registered port
    
    def is_safe_port(self, port: int) -> bool:
        """Check if port is considered safe."""
        return port in self._safe_ports
    
    def is_dangerous_port(self, port: int) -> bool:
        """Check if port is considered dangerous."""
        return port in self._dangerous_ports or port in self._c2_ports
    
    def is_c2_indicator(self, port: int) -> bool:
        """Check if port is a known C2/backdoor indicator."""
        return port in self._c2_ports
    
    def is_known_port(self, port: int) -> bool:
        """Check if port is in our database."""
        return port in self._known_ports
    
    def get_safe_ports(self) -> Set[int]:
        """Get set of all known safe ports."""
        return self._safe_ports.copy()
    
    def get_common_ports(self) -> Set[int]:
        """Get set of all common service ports."""
        return self._common_ports.copy()
    
    def get_dangerous_ports(self) -> Set[int]:
        """Get set of all dangerous ports."""
        return self._dangerous_ports.copy()
    
    def get_c2_ports(self) -> Set[int]:
        """Get set of all C2/backdoor ports."""
        return self._c2_ports.copy()
    
    def get_all_known_ports(self) -> Set[int]:
        """Get set of all known ports."""
        return self._known_ports.copy()
    
    def analyze_port(self, port: int, process: str = "",
                     listening_all_interfaces: bool = False,
                     platform: str = "linux") -> Dict[str, Any]:
        """
        Comprehensive port analysis.
        
        Args:
            port: Port number to analyze
            process: Process name using the port
            listening_all_interfaces: Whether port is exposed on all interfaces
            platform: Operating system platform
            
        Returns:
            Dict with analysis results including risk, recommendations
        """
        info = self.get_port_info(port)
        
        result = {
            "port": port,
            "service": self.get_service_name(port),
            "risk_level": PortRisk.MEDIUM.name,
            "risk_score": 50,
            "issues": [],
            "recommendations": [],
            "mitre_attack_id": None,
            "is_encrypted": False,
            "is_c2_indicator": False,
            "category": "unknown"
        }
        
        if info:
            result["service"] = info.name
            result["description"] = info.description
            result["risk_level"] = info.risk.name
            result["risk_score"] = info.risk.value * 25
            result["mitre_attack_id"] = info.mitre_attack_id
            result["is_encrypted"] = info.security == ProtocolSecurity.ENCRYPTED
            result["tags"] = list(info.tags) if info.tags else []
            
            if info.secure_alternative:
                result["recommendations"].append(
                    f"Consider using port {info.secure_alternative} ({self.get_service_name(info.secure_alternative)}) instead"
                )
        
        # Category determination
        if port in self._c2_ports:
            result["category"] = "c2_indicator"
            result["is_c2_indicator"] = True
            result["issues"].append("Known C2/backdoor port detected!")
        elif port in self._dangerous_ports:
            result["category"] = "dangerous"
            result["issues"].append("Port uses insecure/legacy protocol")
        elif port in self._common_ports:
            result["category"] = "common"
        elif port in self._safe_ports:
            result["category"] = "safe"
        
        # Exposure analysis
        if listening_all_interfaces:
            if result["category"] in ["c2_indicator", "dangerous"]:
                result["risk_score"] += 25
                result["issues"].append("Dangerous port exposed on all interfaces!")
                result["recommendations"].append("Restrict to localhost or internal network")
            elif result["category"] == "common":
                if "database" in (info.tags if info else set()):
                    result["issues"].append("Database exposed on all interfaces")
                    result["recommendations"].append("Databases should only listen on localhost")
                    result["risk_score"] += 15
        
        # Platform-specific checks
        if platform.lower() == "linux":
            windows_ports = {135, 137, 138, 139, 445, 3389}
            if port in windows_ports:
                result["issues"].append("Windows-specific port on Linux (suspicious)")
                result["risk_score"] += 20
        
        # Process validation
        if process and info and info.common_process:
            expected_processes = info.common_process.split(",")
            process_lower = process.lower()
            if not any(exp.lower() in process_lower for exp in expected_processes):
                result["issues"].append(
                    f"Unexpected process '{process}' on port {port} (expected: {info.common_process})"
                )
                result["risk_score"] += 15
        
        # Ensure risk score stays in bounds
        result["risk_score"] = min(100, max(0, result["risk_score"]))
        
        # Update risk level based on final score
        if result["risk_score"] >= 75:
            result["risk_level"] = PortRisk.CRITICAL.name
        elif result["risk_score"] >= 50:
            result["risk_level"] = PortRisk.HIGH.name
        elif result["risk_score"] >= 25:
            result["risk_level"] = PortRisk.MEDIUM.name
        else:
            result["risk_level"] = PortRisk.LOW.name
        
        return result
    
    def get_behavioral_safe_ports(self) -> Set[int]:
        """
        Get extended set of safe ports for behavioral analysis.
        Includes common legitimate service ports.
        """
        safe = self._safe_ports.copy()
        
        # Add common legitimate ports
        safe.update({
            # Web development
            3000, 4200, 5000, 8000, 8080, 8443,
            # Databases (when local)
            3306, 5432, 6379, 27017,
            # Other common
            1194,  # OpenVPN
            51820,  # WireGuard
            2222,   # Alt SSH
        })
        
        return safe
    
    def generate_firewall_recommendations(self, 
                                          exposed_ports: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Generate firewall recommendations based on exposed ports."""
        recommendations = []
        
        for port_info in exposed_ports:
            port = port_info.get("port")
            addr = port_info.get("address", "")
            
            analysis = self.analyze_port(
                port,
                process=port_info.get("process", ""),
                listening_all_interfaces=port_info.get("all_interfaces", False)
            )
            
            if analysis["is_c2_indicator"]:
                recommendations.append({
                    "priority": "CRITICAL",
                    "port": port,
                    "action": f"BLOCK port {port} immediately - known C2/backdoor indicator",
                    "command": f"iptables -A INPUT -p tcp --dport {port} -j DROP"
                })
            elif analysis["risk_level"] == "CRITICAL":
                recommendations.append({
                    "priority": "HIGH",
                    "port": port,
                    "action": f"Block or restrict port {port} ({analysis['service']})",
                    "command": f"iptables -A INPUT -p tcp --dport {port} -j DROP"
                })
            elif analysis["risk_level"] == "HIGH" and port_info.get("all_interfaces"):
                recommendations.append({
                    "priority": "MEDIUM",
                    "port": port,
                    "action": f"Restrict port {port} to localhost: {'; '.join(analysis.get('recommendations', []))}",
                    "command": f"iptables -A INPUT -p tcp --dport {port} ! -s 127.0.0.1 -j DROP"
                })
        
        return recommendations
    
    def export_port_database(self) -> Dict[str, Any]:
        """Export the complete port database."""
        return {
            "safe_ports": {p: d.to_dict() for p, d in self.SAFE_PORTS.items()},
            "common_ports": {p: d.to_dict() for p, d in self.COMMON_PORTS.items()},
            "dangerous_ports": {p: d.to_dict() for p, d in self.DANGEROUS_PORTS.items()},
            "c2_ports": {p: d.to_dict() for p, d in self.C2_BACKDOOR_PORTS.items()}
        }


# Singleton instance
_port_manager: Optional[PortManager] = None


def get_port_manager() -> PortManager:
    """Get the singleton PortManager instance."""
    global _port_manager
    if _port_manager is None:
        _port_manager = PortManager()
    return _port_manager


# Convenience functions
def get_service_name(port: int) -> str:
    """Get service name for a port."""
    return get_port_manager().get_service_name(port)


def is_safe_port(port: int) -> bool:
    """Check if port is safe."""
    return get_port_manager().is_safe_port(port)


def is_dangerous_port(port: int) -> bool:
    """Check if port is dangerous."""
    return get_port_manager().is_dangerous_port(port)


def analyze_port(port: int, **kwargs) -> Dict[str, Any]:
    """Analyze a port for security risks."""
    return get_port_manager().analyze_port(port, **kwargs)


if __name__ == "__main__":
    # Demo usage
    pm = PortManager()
    
    print("Port Manager Demo")
    print("=" * 60)
    
    # Test some ports
    test_ports = [22, 80, 443, 8000, 8080, 3306, 4444, 31337, 12345, 9999]
    
    for port in test_ports:
        analysis = pm.analyze_port(port, listening_all_interfaces=True)
        print(f"\nPort {port}: {analysis['service']}")
        print(f"  Risk: {analysis['risk_level']} (Score: {analysis['risk_score']})")
        print(f"  Category: {analysis['category']}")
        if analysis['issues']:
            print(f"  Issues: {', '.join(analysis['issues'])}")
        if analysis['recommendations']:
            print(f"  Recommendations: {', '.join(analysis['recommendations'])}")
