/// gRPC channel management for Grey SDK.
library;

import 'package:grpc/grpc.dart';

import '../config/grey_options.dart';

/// Manages the gRPC client channel connection.
class GreyChannel {
  /// Creates a new channel manager with the given options.
  GreyChannel(this.options) {
    _channel = ClientChannel(
      options.host,
      port: options.port,
      options: options.channelOptions ??
          ChannelOptions(credentials: options.credentials),
    );
  }

  /// The configuration options.
  final GreyOptions options;

  late final ClientChannel _channel;

  String? _accessToken;

  /// Gets the underlying gRPC channel.
  ClientChannel get channel => _channel;

  /// Sets the access token for authenticated requests.
  void setAccessToken(String? token) {
    _accessToken = token;
  }

  /// Gets the current access token.
  String? get accessToken => _accessToken;

  /// Whether the client has a valid access token.
  bool get isAuthenticated => _accessToken != null;

  /// Creates call options with authentication and timeout.
  CallOptions get callOptions {
    return CallOptions(
      timeout: options.timeout,
      metadata: _buildMetadata(),
    );
  }

  /// Creates call options without authentication requirement.
  CallOptions get publicCallOptions {
    return CallOptions(
      timeout: options.timeout,
    );
  }

  /// Builds metadata map with authorization header.
  Map<String, String> _buildMetadata() {
    final metadata = <String, String>{};
    if (_accessToken != null) {
      metadata['authorization'] = 'Bearer $_accessToken';
    }
    return metadata;
  }

  /// Shuts down the channel.
  Future<void> shutdown() async {
    await _channel.shutdown();
  }

  /// Terminates the channel immediately.
  Future<void> terminate() async {
    await _channel.terminate();
  }
}
