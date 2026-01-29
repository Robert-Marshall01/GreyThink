/// Types for gRPC services.
library;

/// Authentication response data.
class AuthData {
  /// Creates new auth data.
  const AuthData({
    required this.accessToken,
    required this.refreshToken,
    required this.expiresIn,
  });

  /// Creates auth data from a map.
  factory AuthData.fromMap(Map<String, dynamic> map) {
    return AuthData(
      accessToken: map['accessToken'] as String? ?? '',
      refreshToken: map['refreshToken'] as String? ?? '',
      expiresIn: map['expiresIn'] as int? ?? 0,
    );
  }

  /// The access token for API requests.
  final String accessToken;

  /// The refresh token for obtaining new access tokens.
  final String refreshToken;

  /// Token expiration time in seconds.
  final int expiresIn;

  /// Converts to a map.
  Map<String, dynamic> toMap() {
    return {
      'accessToken': accessToken,
      'refreshToken': refreshToken,
      'expiresIn': expiresIn,
    };
  }
}

/// User data.
class User {
  /// Creates new user data.
  const User({
    required this.id,
    required this.email,
    required this.name,
    this.avatar,
    this.metadata,
  });

  /// Creates user data from a map.
  factory User.fromMap(Map<String, dynamic> map) {
    return User(
      id: map['id'] as String? ?? '',
      email: map['email'] as String? ?? '',
      name: map['name'] as String? ?? '',
      avatar: map['avatar'] as String?,
      metadata: map['metadata'] as Map<String, dynamic>?,
    );
  }

  /// The user's unique identifier.
  final String id;

  /// The user's email address.
  final String email;

  /// The user's display name.
  final String name;

  /// The user's avatar URL.
  final String? avatar;

  /// Additional user metadata.
  final Map<String, dynamic>? metadata;

  /// Converts to a map.
  Map<String, dynamic> toMap() {
    return {
      'id': id,
      'email': email,
      'name': name,
      if (avatar != null) 'avatar': avatar,
      if (metadata != null) 'metadata': metadata,
    };
  }
}

/// Project data.
class Project {
  /// Creates new project data.
  const Project({
    required this.id,
    required this.name,
    this.description,
    this.createdAt,
    this.updatedAt,
    this.metadata,
  });

  /// Creates project data from a map.
  factory Project.fromMap(Map<String, dynamic> map) {
    return Project(
      id: map['id'] as String? ?? '',
      name: map['name'] as String? ?? '',
      description: map['description'] as String?,
      createdAt: map['createdAt'] as String?,
      updatedAt: map['updatedAt'] as String?,
      metadata: map['metadata'] as Map<String, dynamic>?,
    );
  }

  /// The project's unique identifier.
  final String id;

  /// The project's name.
  final String name;

  /// The project's description.
  final String? description;

  /// When the project was created.
  final String? createdAt;

  /// When the project was last updated.
  final String? updatedAt;

  /// Additional project metadata.
  final Map<String, dynamic>? metadata;

  /// Converts to a map.
  Map<String, dynamic> toMap() {
    return {
      'id': id,
      'name': name,
      if (description != null) 'description': description,
      if (createdAt != null) 'createdAt': createdAt,
      if (updatedAt != null) 'updatedAt': updatedAt,
      if (metadata != null) 'metadata': metadata,
    };
  }
}

/// Projects list response.
class ProjectsData {
  /// Creates new projects data.
  const ProjectsData({
    required this.projects,
    required this.total,
    required this.page,
    required this.pageSize,
  });

  /// Creates projects data from a map.
  factory ProjectsData.fromMap(Map<String, dynamic> map) {
    final projectsList = map['projects'] as List<dynamic>? ?? [];
    return ProjectsData(
      projects: projectsList
          .map((p) => Project.fromMap(p as Map<String, dynamic>))
          .toList(),
      total: map['total'] as int? ?? 0,
      page: map['page'] as int? ?? 1,
      pageSize: map['pageSize'] as int? ?? 20,
    );
  }

  /// The list of projects.
  final List<Project> projects;

  /// Total number of projects.
  final int total;

  /// Current page number.
  final int page;

  /// Page size.
  final int pageSize;

  /// Converts to a map.
  Map<String, dynamic> toMap() {
    return {
      'projects': projects.map((p) => p.toMap()).toList(),
      'total': total,
      'page': page,
      'pageSize': pageSize,
    };
  }
}

/// Query response data.
class QueryData {
  /// Creates new query data.
  const QueryData({
    required this.data,
    this.metadata,
  });

  /// Creates query data from a map.
  factory QueryData.fromMap(Map<String, dynamic> map) {
    return QueryData(
      data: map['data'],
      metadata: map['metadata'] as Map<String, dynamic>?,
    );
  }

  /// The query result data.
  final dynamic data;

  /// Additional response metadata.
  final Map<String, dynamic>? metadata;

  /// Converts to a map.
  Map<String, dynamic> toMap() {
    return {
      'data': data,
      if (metadata != null) 'metadata': metadata,
    };
  }
}

/// Mutation response data.
class MutationData {
  /// Creates new mutation data.
  const MutationData({
    required this.success,
    this.data,
    this.metadata,
  });

  /// Creates mutation data from a map.
  factory MutationData.fromMap(Map<String, dynamic> map) {
    return MutationData(
      success: map['success'] as bool? ?? false,
      data: map['data'],
      metadata: map['metadata'] as Map<String, dynamic>?,
    );
  }

  /// Whether the mutation was successful.
  final bool success;

  /// The mutation result data.
  final dynamic data;

  /// Additional response metadata.
  final Map<String, dynamic>? metadata;

  /// Converts to a map.
  Map<String, dynamic> toMap() {
    return {
      'success': success,
      if (data != null) 'data': data,
      if (metadata != null) 'metadata': metadata,
    };
  }
}
