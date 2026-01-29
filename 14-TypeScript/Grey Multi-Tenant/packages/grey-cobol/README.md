# Grey SDK for COBOL

A COBOL SDK for the Grey Multi-Tenant platform using HTTP JSON transport.

## Requirements

- GnuCOBOL 3.0+ or IBM Enterprise COBOL
- libcurl for HTTP operations (or platform HTTP library)
- JSON parsing library (or built-in JSON support)

## Structure

```
grey-cobol/
├── copybooks/           # COBOL copybooks (shared data structures)
│   ├── GREYCONF.cpy     # Configuration options
│   ├── GREYERR.cpy      # Error structure
│   ├── GREYRES.cpy      # Result structure
│   ├── GREYAUTH.cpy     # Auth request/response
│   ├── GREYUSER.cpy     # User data
│   └── GREYPROJ.cpy     # Project data
├── src/                 # Source programs
│   ├── grey_error.cbl   # Error handling routines
│   ├── http_client.cbl  # HTTP client wrapper
│   ├── auth_client.cbl  # Authentication client
│   ├── user_client.cbl  # User client
│   ├── projects_client.cbl # Projects client
│   ├── query_client.cbl # Query client
│   ├── mutation_client.cbl # Mutation client
│   └── grey_sdk.cbl     # Main SDK entry point
└── tests/               # Test programs
```

## Compilation

Using GnuCOBOL:

```bash
# Compile all modules
cobc -x -I copybooks src/grey_sdk.cbl src/http_client.cbl \
     src/auth_client.cbl src/user_client.cbl src/projects_client.cbl \
     src/query_client.cbl src/mutation_client.cbl src/grey_error.cbl
```

## Usage

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-PROGRAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY GREYCONF.
       COPY GREYERR.
       COPY GREYRES.
       COPY GREYAUTH.
       
       PROCEDURE DIVISION.
           PERFORM INITIALIZE-SDK
           PERFORM LOGIN-USER
           PERFORM LIST-PROJECTS
           STOP RUN.
       
       INITIALIZE-SDK.
      *    Configure for local development
           MOVE "localhost" TO WS-HOST
           MOVE 8080 TO WS-PORT
           MOVE "N" TO WS-USE-TLS.
       
       LOGIN-USER.
           MOVE "username" TO WS-AUTH-USERNAME
           MOVE "password" TO WS-AUTH-PASSWORD
           CALL "AUTH-LOGIN" USING WS-AUTH-REQUEST
                                   WS-AUTH-RESPONSE
                                   WS-RESULT
           IF WS-RESULT-OK = "Y"
               MOVE WS-ACCESS-TOKEN TO WS-AUTH-TOKEN
           END-IF.
       
       LIST-PROJECTS.
           CALL "PROJECTS-LIST" USING WS-PROJECTS-REQUEST
                                      WS-PROJECTS-RESPONSE
                                      WS-RESULT
           IF WS-RESULT-OK = "Y"
               PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-PROJECT-COUNT
                   DISPLAY WS-PROJECT-NAME(WS-IDX)
               END-PERFORM
           END-IF.
```

## API Reference

### Configuration (GREYCONF.cpy)

- `WS-HOST` - Server hostname (PIC X(256))
- `WS-PORT` - Server port (PIC 9(5))
- `WS-USE-TLS` - Use HTTPS flag (PIC X, "Y"/"N")
- `WS-TIMEOUT` - Request timeout seconds (PIC 9(3))
- `WS-AUTH-TOKEN` - Bearer token (PIC X(2048))

### Result Structure (GREYRES.cpy)

- `WS-RESULT-OK` - Success flag (PIC X, "Y"/"N")
- `WS-RESULT-DATA` - Response data (PIC X(32000))
- `WS-RESULT-ERROR` - Error structure (uses GREYERR.cpy)

### Error Structure (GREYERR.cpy)

- `WS-ERROR-CODE` - Error code (PIC X(32))
- `WS-ERROR-MESSAGE` - Error message (PIC X(256))
- `WS-ERROR-DETAILS` - JSON details (PIC X(1024))

### Available Programs

| Program | Description |
|---------|-------------|
| AUTH-LOGIN | Authenticate with username/password |
| AUTH-LOGOUT | End current session |
| AUTH-REFRESH | Refresh access token |
| USER-GET | Get user by ID |
| USER-GET-CURRENT | Get current user |
| PROJECTS-LIST | List all projects |
| PROJECTS-GET | Get project by ID |
| PROJECTS-CREATE | Create new project |
| QUERY-EXECUTE | Execute a query |
| QUERY-BATCH | Execute batch queries |
| MUTATION-EXECUTE | Execute a mutation |
| MUTATION-BATCH | Execute batch mutations |

## Error Codes

- `UNAUTHORIZED` - Authentication required (HTTP 401)
- `FORBIDDEN` - Permission denied (HTTP 403)
- `NOT-FOUND` - Resource not found (HTTP 404)
- `VALIDATION-ERROR` - Invalid input (HTTP 400/422)
- `NETWORK-ERROR` - Connection failed
- `TIMEOUT` - Request timeout (HTTP 408/504)
- `SERVER-ERROR` - Server error (HTTP 5xx)
- `UNKNOWN` - Unknown error

## License

MIT License
