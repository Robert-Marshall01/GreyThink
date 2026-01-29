# Grey Multi-Tenant SDK - Scala

Scala SDK for the Grey Multi-Tenant platform using gRPC transport.

## Installation

Add the dependency to your `build.sbt`:

```scala
libraryDependencies += "com.grey" %% "grey-sdk" % "0.1.0"
```

## Usage

```scala
import com.grey.sdk._
import com.grey.sdk.config.GreyOptions
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

object Main extends App {
  // Create client
  val options = GreyOptions(
    host = "api.grey.example.com",
    port = 443,
    useTls = true
  )
  val client = GreyClient(options)

  // Login
  client.auth.login("user@example.com", "password").onComplete {
    case Success(auth) =>
      println(s"Logged in: ${auth.accessToken}")
      
      // Fetch user
      client.user.getUser().onComplete {
        case Success(user) =>
          println(s"User: ${user.name}")
        case Failure(e: GreyError) =>
          println(s"Error: [${e.code}] ${e.message}")
        case Failure(e) =>
          println(s"Unexpected error: ${e.getMessage}")
      }
      
    case Failure(e: GreyError) =>
      println(s"Login failed: [${e.code}] ${e.message}")
    case Failure(e) =>
      println(s"Unexpected error: ${e.getMessage}")
  }
  
  // Cleanup on shutdown
  sys.addShutdownHook {
    client.shutdown()
  }
}
```

## Using Either for Error Handling

```scala
import com.grey.sdk._
import scala.concurrent.{Future, ExecutionContext}

def example(client: GreyClient)(using ExecutionContext): Future[Either[GreyError, User]] = {
  client.auth.loginSafe("user@example.com", "password").flatMap {
    case Right(_) => client.user.getUserSafe()
    case Left(error) => Future.successful(Left(error))
  }
}
```

## Features

- **gRPC Transport**: Fast, type-safe binary protocol
- **Scala 3**: Modern Scala with improved syntax
- **Futures**: Non-blocking async operations
- **Error Normalization**: Consistent `GreyError` across all operations
- **Domain Clients**: Auth, User, Projects, Query, Mutation

## API

### GreyClient

Main entry point providing access to all domain clients.

### AuthClient

- `login(email: String, password: String): Future[AuthData]`
- `logout(): Future[Unit]`
- `refresh(refreshToken: Option[String]): Future[AuthData]`

### UserClient

- `getUser(forceRefresh: Boolean = false): Future[User]`

### ProjectsClient

- `listProjects(page: Int = 1, pageSize: Int = 20): Future[ProjectsData]`
- `createProject(name: String, description: Option[String] = None): Future[Project]`

### QueryClient

- `query(endpoint: String, params: Map[String, String] = Map.empty, requireAuth: Boolean = true): Future[QueryData]`

### MutationClient

- `mutate(endpoint: String, method: String = "POST", body: Option[Any] = None): Future[MutationData]`

## License

MIT
