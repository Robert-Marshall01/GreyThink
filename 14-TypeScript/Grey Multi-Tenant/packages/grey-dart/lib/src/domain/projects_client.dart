/// Projects domain client for Grey SDK.
library;

import 'dart:async';

import 'package:grpc/grpc.dart';

import '../error/grey_error.dart';
import '../grpc/grey_channel.dart';
import '../grpc/projects_grpc_service.dart';
import '../grpc/types.dart';

/// Client for project operations.
class ProjectsClient {
  /// Creates a new projects client.
  ProjectsClient(this._channel, this._service);

  final GreyChannel _channel;
  final ProjectsGrpcService _service;

  /// Lists projects with pagination.
  ///
  /// [page] is the page number (1-indexed).
  /// [pageSize] is the number of items per page.
  ///
  /// Throws [GreyError] if not authenticated or on failure.
  Future<ProjectsData> listProjects({
    int page = 1,
    int pageSize = 20,
  }) async {
    if (!_channel.isAuthenticated) {
      throw GreyError(
        code: GreyError.unauthorized,
        message: 'Not authenticated',
      );
    }

    // Validate pagination
    if (page < 1) {
      throw GreyError(
        code: GreyError.validationError,
        message: 'Page must be >= 1',
      );
    }
    if (pageSize < 1 || pageSize > 100) {
      throw GreyError(
        code: GreyError.validationError,
        message: 'Page size must be between 1 and 100',
      );
    }

    try {
      return await _service.listProjects(
        page: page,
        pageSize: pageSize,
      );
    } on GrpcError catch (e) {
      throw GreyError.fromGrpcError(e);
    } catch (e) {
      throw GreyError.fromException(e);
    }
  }

  /// Creates a new project.
  ///
  /// [name] is required and must not be empty.
  /// [description] is optional.
  ///
  /// Throws [GreyError] if not authenticated or on failure.
  Future<Project> createProject({
    required String name,
    String? description,
  }) async {
    if (!_channel.isAuthenticated) {
      throw GreyError(
        code: GreyError.unauthorized,
        message: 'Not authenticated',
      );
    }

    // Validate inputs
    if (name.isEmpty) {
      throw GreyError(
        code: GreyError.validationError,
        message: 'Project name is required',
      );
    }
    if (name.length > 255) {
      throw GreyError(
        code: GreyError.validationError,
        message: 'Project name must be <= 255 characters',
      );
    }

    try {
      return await _service.createProject(
        name: name,
        description: description,
      );
    } on GrpcError catch (e) {
      throw GreyError.fromGrpcError(e);
    } catch (e) {
      throw GreyError.fromException(e);
    }
  }
}
