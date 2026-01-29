/// Projects gRPC service stub.
library;

import 'dart:async';

import 'package:grpc/grpc.dart';

import 'grey_channel.dart';
import 'types.dart';

/// gRPC service for project operations.
///
/// This is a placeholder stub. In a real implementation, this would
/// use generated protobuf client stubs from .proto files.
class ProjectsGrpcService {
  /// Creates a new projects gRPC service.
  ProjectsGrpcService(this._channel);

  final GreyChannel _channel;

  /// Lists projects via gRPC.
  Future<ProjectsData> listProjects({
    int page = 1,
    int pageSize = 20,
  }) async {
    // In a real implementation:
    // final request = ListProjectsRequest()
    //   ..page = page
    //   ..pageSize = pageSize;
    // final response = await _stub.listProjects(request, options: _channel.callOptions);
    // return ProjectsData.fromMessage(response);

    return _simulateGrpcCall<ProjectsData>(
      'projects.ListProjects',
      () => ProjectsData(
        projects: [
          const Project(
            id: 'project_1',
            name: 'Stub Project 1',
            description: 'A stub project for testing',
          ),
          const Project(
            id: 'project_2',
            name: 'Stub Project 2',
          ),
        ],
        total: 2,
        page: page,
        pageSize: pageSize,
      ),
    );
  }

  /// Creates a project via gRPC.
  Future<Project> createProject({
    required String name,
    String? description,
  }) async {
    // In a real implementation:
    // final request = CreateProjectRequest()
    //   ..name = name
    //   ..description = description ?? '';
    // final response = await _stub.createProject(request, options: _channel.callOptions);
    // return Project.fromMessage(response);

    return _simulateGrpcCall<Project>(
      'projects.CreateProject',
      () => Project(
        id: 'new_project_id',
        name: name,
        description: description,
        createdAt: DateTime.now().toIso8601String(),
      ),
    );
  }

  /// Simulates a gRPC call for stub implementation.
  Future<T> _simulateGrpcCall<T>(String method, T Function() stubResult) async {
    // Verify authentication
    if (!_channel.isAuthenticated) {
      throw GrpcError.unauthenticated('Not authenticated');
    }

    final _ = _channel.callOptions;

    // Simulate network delay
    await Future<void>.delayed(const Duration(milliseconds: 1));

    return stubResult();
  }
}
