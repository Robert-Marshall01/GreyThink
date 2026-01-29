package com.grey.sdk.grpc

/** Data types for gRPC services. */

/** Authentication response data.
  *
  * @param accessToken
  *   The access token for API requests
  * @param refreshToken
  *   The refresh token for obtaining new access tokens
  * @param expiresIn
  *   Token expiration time in seconds
  */
final case class AuthData(
    accessToken: String,
    refreshToken: String,
    expiresIn: Int
)

/** User data.
  *
  * @param id
  *   The user's unique identifier
  * @param email
  *   The user's email address
  * @param name
  *   The user's display name
  * @param avatar
  *   The user's avatar URL
  * @param metadata
  *   Additional user metadata
  */
final case class User(
    id: String,
    email: String,
    name: String,
    avatar: Option[String] = None,
    metadata: Option[Map[String, Any]] = None
)

/** Project data.
  *
  * @param id
  *   The project's unique identifier
  * @param name
  *   The project's name
  * @param description
  *   The project's description
  * @param createdAt
  *   When the project was created
  * @param updatedAt
  *   When the project was last updated
  * @param metadata
  *   Additional project metadata
  */
final case class Project(
    id: String,
    name: String,
    description: Option[String] = None,
    createdAt: Option[String] = None,
    updatedAt: Option[String] = None,
    metadata: Option[Map[String, Any]] = None
)

/** Projects list response.
  *
  * @param projects
  *   The list of projects
  * @param total
  *   Total number of projects
  * @param page
  *   Current page number
  * @param pageSize
  *   Page size
  */
final case class ProjectsData(
    projects: List[Project],
    total: Int,
    page: Int,
    pageSize: Int
)

/** Query response data.
  *
  * @param data
  *   The query result data
  * @param metadata
  *   Additional response metadata
  */
final case class QueryData(
    data: Any,
    metadata: Option[Map[String, Any]] = None
)

/** Mutation response data.
  *
  * @param success
  *   Whether the mutation was successful
  * @param data
  *   The mutation result data
  * @param metadata
  *   Additional response metadata
  */
final case class MutationData(
    success: Boolean,
    data: Option[Any] = None,
    metadata: Option[Map[String, Any]] = None
)
