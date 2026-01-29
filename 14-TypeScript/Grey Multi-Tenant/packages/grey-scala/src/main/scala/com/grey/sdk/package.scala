/** Grey Multi-Tenant SDK for Scala.
  *
  * A Scala SDK for the Grey Multi-Tenant platform using gRPC transport.
  *
  * == Getting Started ==
  *
  * {{{
  * import com.grey.sdk._
  * import com.grey.sdk.config.GreyOptions
  * import scala.concurrent.ExecutionContext.Implicits.global
  * import scala.util.{Success, Failure}
  *
  * val client = GreyClient(GreyOptions(host = "api.grey.example.com"))
  *
  * client.auth.login("user@example.com", "password").onComplete {
  *   case Success(auth) =>
  *     println(s"Logged in: ${auth.accessToken}")
  *
  *     client.user.getUser().onComplete {
  *       case Success(user) => println(s"User: ${user.name}")
  *       case Failure(e: GreyError) => println(s"Error: ${e.code}")
  *       case Failure(e) => println(s"Unexpected: ${e.getMessage}")
  *     }
  *
  *   case Failure(e: GreyError) =>
  *     println(s"Login failed: [${e.code}] ${e.message}")
  *   case Failure(e) =>
  *     println(s"Unexpected error: ${e.getMessage}")
  * }
  *
  * // Clean up
  * sys.addShutdownHook(client.shutdown())
  * }}}
  *
  * == Safe API (Either-based) ==
  *
  * All domain client methods have `*Safe` variants that return
  * `Future[Either[GreyError, T]]` instead of throwing:
  *
  * {{{
  * client.auth.loginSafe("user@example.com", "password").flatMap {
  *   case Right(_) => client.user.getUserSafe()
  *   case Left(error) => Future.successful(Left(error))
  * }
  * }}}
  */
package object sdk {
  // Re-export main types for convenient imports
  export com.grey.sdk.error.{GreyError, GreyResult}
  export com.grey.sdk.config.GreyOptions
  export com.grey.sdk.grpc.{AuthData, User, Project, ProjectsData, QueryData, MutationData}
  export com.grey.sdk.GreyClient
  export com.grey.sdk.domain.{AuthClient, UserClient, ProjectsClient, QueryClient, MutationClient}
}
