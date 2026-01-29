"""Grey SDK Configuration Options.

Configuration options for the Grey Multi-Tenant SDK client.
"""


@value
struct Options:
    """Configuration options for the Grey SDK client."""

    var host: String
    var port: Int
    var use_tls: Bool
    var timeout_ms: Int
    var auth_token: String
    var tenant_id: String

    fn __init__(
        inout self,
        host: String = "localhost",
        port: Int = 50051,
        use_tls: Bool = False,
        timeout_ms: Int = 30000,
        auth_token: String = "",
        tenant_id: String = "",
    ):
        self.host = host
        self.port = port
        self.use_tls = use_tls
        self.timeout_ms = timeout_ms
        self.auth_token = auth_token
        self.tenant_id = tenant_id

    fn endpoint(self) -> String:
        """Get the full endpoint URL."""
        var scheme = "http"
        if self.use_tls:
            scheme = "https"
        return scheme + "://" + self.host + ":" + str(self.port)

    fn grpc_target(self) -> String:
        """Get the gRPC target string."""
        return self.host + ":" + str(self.port)

    fn with_auth_token(self, token: String) -> Options:
        """Return a copy with the auth token set."""
        return Options(
            self.host,
            self.port,
            self.use_tls,
            self.timeout_ms,
            token,
            self.tenant_id,
        )

    fn with_tenant(self, tenant_id: String) -> Options:
        """Return a copy with the tenant ID set."""
        return Options(
            self.host,
            self.port,
            self.use_tls,
            self.timeout_ms,
            self.auth_token,
            tenant_id,
        )

    fn with_timeout(self, timeout_ms: Int) -> Options:
        """Return a copy with the timeout set."""
        return Options(
            self.host,
            self.port,
            self.use_tls,
            timeout_ms,
            self.auth_token,
            self.tenant_id,
        )

    @staticmethod
    fn local() -> Options:
        """Create options for local development."""
        return Options(
            host="localhost",
            port=50051,
            use_tls=False,
            timeout_ms=30000,
        )

    @staticmethod
    fn local_with_port(port: Int) -> Options:
        """Create options for local development with custom port."""
        return Options(
            host="localhost",
            port=port,
            use_tls=False,
            timeout_ms=30000,
        )

    @staticmethod
    fn production(host: String) -> Options:
        """Create options for production."""
        return Options(
            host=host,
            port=443,
            use_tls=True,
            timeout_ms=30000,
        )

    @staticmethod
    fn production_with_port(host: String, port: Int) -> Options:
        """Create options for production with custom port."""
        return Options(
            host=host,
            port=port,
            use_tls=True,
            timeout_ms=30000,
        )
