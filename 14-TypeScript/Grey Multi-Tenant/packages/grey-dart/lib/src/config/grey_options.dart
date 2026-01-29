/// Configuration options for the Grey SDK client.
library;

import 'package:grpc/grpc.dart';

/// Configuration options for connecting to the Grey API.
class GreyOptions {
  /// Creates new Grey client options.
  const GreyOptions({
    required this.host,
    this.port = 443,
    this.useTls = true,
    this.timeoutMs = 30000,
    this.channelOptions,
  });

  /// The host address of the Grey API server.
  final String host;

  /// The port to connect to. Defaults to 443.
  final int port;

  /// Whether to use TLS for the connection. Defaults to true.
  final bool useTls;

  /// Request timeout in milliseconds. Defaults to 30000 (30 seconds).
  final int timeoutMs;

  /// Optional custom channel options for gRPC.
  final ChannelOptions? channelOptions;

  /// Returns timeout as a Duration.
  Duration get timeout => Duration(milliseconds: timeoutMs);

  /// Creates channel credentials based on TLS setting.
  ChannelCredentials get credentials {
    return useTls
        ? const ChannelCredentials.secure()
        : const ChannelCredentials.insecure();
  }

  /// Creates a copy with optional modifications.
  GreyOptions copyWith({
    String? host,
    int? port,
    bool? useTls,
    int? timeoutMs,
    ChannelOptions? channelOptions,
  }) {
    return GreyOptions(
      host: host ?? this.host,
      port: port ?? this.port,
      useTls: useTls ?? this.useTls,
      timeoutMs: timeoutMs ?? this.timeoutMs,
      channelOptions: channelOptions ?? this.channelOptions,
    );
  }

  @override
  String toString() {
    return 'GreyOptions(host: $host, port: $port, useTls: $useTls, timeoutMs: $timeoutMs)';
  }
}
