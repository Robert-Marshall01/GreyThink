      ******************************************************************
      * http_client.cbl - Grey SDK HTTP Client Module
      * Provides HTTP request routines using simple CALL interface.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HTTP-CLIENT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY GREYCONF.
       COPY GREYERR.
       COPY GREYRES.
       
       01  WS-URL                      PIC X(1024).
       01  WS-FULL-URL                 PIC X(1024).
       01  WS-METHOD                   PIC X(8).
       01  WS-REQUEST-BODY             PIC X(32000).
       01  WS-REQUEST-BODY-LEN         PIC 9(5).
       01  WS-TEMP-STRING              PIC X(256).
       01  WS-PORT-STRING              PIC X(5).
       01  WS-IDX                      PIC 9(3).
       01  WS-SCHEME                   PIC X(8).
       
      ******************************************************************
      * HTTP library interface variables
      * These would interface with libcurl or platform HTTP library
      ******************************************************************
       01  HTTP-HANDLE                 PIC 9(8) COMP.
       01  HTTP-RETURN-CODE            PIC S9(8) COMP.
       
       LINKAGE SECTION.
       01  LS-CONFIG.
           05  LS-HOST                 PIC X(256).
           05  LS-PORT                 PIC 9(5).
           05  LS-USE-TLS              PIC X.
           05  LS-TIMEOUT              PIC 9(3).
           05  LS-AUTH-TOKEN           PIC X(2048).
       
       01  LS-PATH                     PIC X(512).
       01  LS-BODY                     PIC X(32000).
       01  LS-QUERY-PARAMS             PIC X(1024).
       
       01  LS-RESULT.
           05  LS-RESULT-OK            PIC X.
           05  LS-RESULT-DATA          PIC X(32000).
           05  LS-RESULT-DATA-LEN      PIC 9(5).
           05  LS-RESULT-ERROR.
               10  LS-RES-ERR-CODE     PIC X(32).
               10  LS-RES-ERR-MSG      PIC X(256).
               10  LS-RES-ERR-DETAILS  PIC X(1024).
       
       PROCEDURE DIVISION.
      
      ******************************************************************
      * HTTP-GET: Execute GET request
      * Input:  LS-CONFIG, LS-PATH, LS-QUERY-PARAMS
      * Output: LS-RESULT
      ******************************************************************
       ENTRY "HTTP-GET" USING LS-CONFIG LS-PATH LS-QUERY-PARAMS
                              LS-RESULT.
           MOVE "GET" TO WS-METHOD
           MOVE SPACES TO WS-REQUEST-BODY
           PERFORM BUILD-URL
           PERFORM EXECUTE-REQUEST
           GOBACK.
      
      ******************************************************************
      * HTTP-POST: Execute POST request
      * Input:  LS-CONFIG, LS-PATH, LS-BODY
      * Output: LS-RESULT
      ******************************************************************
       ENTRY "HTTP-POST" USING LS-CONFIG LS-PATH LS-BODY LS-RESULT.
           MOVE "POST" TO WS-METHOD
           MOVE LS-BODY TO WS-REQUEST-BODY
           MOVE SPACES TO LS-QUERY-PARAMS
           PERFORM BUILD-URL
           PERFORM EXECUTE-REQUEST
           GOBACK.
      
      ******************************************************************
      * HTTP-PUT: Execute PUT request
      * Input:  LS-CONFIG, LS-PATH, LS-BODY
      * Output: LS-RESULT
      ******************************************************************
       ENTRY "HTTP-PUT" USING LS-CONFIG LS-PATH LS-BODY LS-RESULT.
           MOVE "PUT" TO WS-METHOD
           MOVE LS-BODY TO WS-REQUEST-BODY
           MOVE SPACES TO LS-QUERY-PARAMS
           PERFORM BUILD-URL
           PERFORM EXECUTE-REQUEST
           GOBACK.
      
      ******************************************************************
      * HTTP-PATCH: Execute PATCH request
      * Input:  LS-CONFIG, LS-PATH, LS-BODY
      * Output: LS-RESULT
      ******************************************************************
       ENTRY "HTTP-PATCH" USING LS-CONFIG LS-PATH LS-BODY LS-RESULT.
           MOVE "PATCH" TO WS-METHOD
           MOVE LS-BODY TO WS-REQUEST-BODY
           MOVE SPACES TO LS-QUERY-PARAMS
           PERFORM BUILD-URL
           PERFORM EXECUTE-REQUEST
           GOBACK.
      
      ******************************************************************
      * HTTP-DELETE: Execute DELETE request
      * Input:  LS-CONFIG, LS-PATH
      * Output: LS-RESULT
      ******************************************************************
       ENTRY "HTTP-DELETE" USING LS-CONFIG LS-PATH LS-RESULT.
           MOVE "DELETE" TO WS-METHOD
           MOVE SPACES TO WS-REQUEST-BODY
           MOVE SPACES TO LS-QUERY-PARAMS
           PERFORM BUILD-URL
           PERFORM EXECUTE-REQUEST
           GOBACK.
      
      ******************************************************************
      * BUILD-URL: Construct full URL from config and path
      ******************************************************************
       BUILD-URL.
           MOVE SPACES TO WS-FULL-URL
           
      *    Determine scheme
           IF LS-USE-TLS = "Y"
               MOVE "https://" TO WS-SCHEME
           ELSE
               MOVE "http://" TO WS-SCHEME
           END-IF
           
      *    Build URL: scheme://host:port/path
           MOVE LS-PORT TO WS-PORT-STRING
           STRING WS-SCHEME DELIMITED BY SPACE
                  LS-HOST DELIMITED BY SPACE
                  ":" DELIMITED BY SIZE
                  WS-PORT-STRING DELIMITED BY SPACE
                  INTO WS-FULL-URL
           
      *    Append path (ensure leading slash)
           IF LS-PATH(1:1) NOT = "/"
               STRING WS-FULL-URL DELIMITED BY SPACE
                      "/" DELIMITED BY SIZE
                      LS-PATH DELIMITED BY SPACE
                      INTO WS-FULL-URL
           ELSE
               STRING WS-FULL-URL DELIMITED BY SPACE
                      LS-PATH DELIMITED BY SPACE
                      INTO WS-FULL-URL
           END-IF
           
      *    Append query params if present
           IF LS-QUERY-PARAMS NOT = SPACES
               STRING WS-FULL-URL DELIMITED BY SPACE
                      "?" DELIMITED BY SIZE
                      LS-QUERY-PARAMS DELIMITED BY SPACE
                      INTO WS-FULL-URL
           END-IF.
      
      ******************************************************************
      * EXECUTE-REQUEST: Execute HTTP request using platform library
      ******************************************************************
       EXECUTE-REQUEST.
      *    Initialize result
           MOVE "N" TO LS-RESULT-OK
           MOVE SPACES TO LS-RESULT-DATA
           MOVE 0 TO LS-RESULT-DATA-LEN
           MOVE SPACES TO LS-RES-ERR-CODE
           MOVE SPACES TO LS-RES-ERR-MSG
           MOVE SPACES TO LS-RES-ERR-DETAILS
           
      *    Initialize HTTP library
           CALL "HTTP-INIT" USING HTTP-HANDLE
                                  HTTP-RETURN-CODE
           
           IF HTTP-RETURN-CODE NOT = 0
               CALL "ERROR-NETWORK" USING 
                   "Failed to initialize HTTP library"
                   LS-RESULT-ERROR
               GOBACK
           END-IF
           
      *    Set URL
           CALL "HTTP-SET-URL" USING HTTP-HANDLE
                                     WS-FULL-URL
                                     HTTP-RETURN-CODE
           
      *    Set method
           CALL "HTTP-SET-METHOD" USING HTTP-HANDLE
                                        WS-METHOD
                                        HTTP-RETURN-CODE
           
      *    Set timeout
           CALL "HTTP-SET-TIMEOUT" USING HTTP-HANDLE
                                         LS-TIMEOUT
                                         HTTP-RETURN-CODE
           
      *    Set headers
           PERFORM SET-HEADERS
           
      *    Set request body if present
           IF WS-REQUEST-BODY NOT = SPACES
               CALL "HTTP-SET-BODY" USING HTTP-HANDLE
                                          WS-REQUEST-BODY
                                          HTTP-RETURN-CODE
           END-IF
           
      *    Execute request
           CALL "HTTP-EXECUTE" USING HTTP-HANDLE
                                     WS-HTTP-STATUS
                                     WS-HTTP-BODY
                                     WS-HTTP-BODY-LEN
                                     HTTP-RETURN-CODE
           
      *    Check for network error
           IF HTTP-RETURN-CODE NOT = 0
               EVALUATE HTTP-RETURN-CODE
                   WHEN 28
      *                CURLE_OPERATION_TIMEDOUT
                       CALL "ERROR-TIMEOUT" USING
                           "Request timed out"
                           LS-RESULT-ERROR
                   WHEN OTHER
                       CALL "ERROR-NETWORK" USING
                           "Network error occurred"
                           LS-RESULT-ERROR
               END-EVALUATE
               PERFORM CLEANUP-HTTP
               GOBACK
           END-IF
           
      *    Process response based on status code
           IF WS-HTTP-STATUS >= 200 AND WS-HTTP-STATUS < 300
               MOVE "Y" TO LS-RESULT-OK
               MOVE WS-HTTP-BODY TO LS-RESULT-DATA
               MOVE WS-HTTP-BODY-LEN TO LS-RESULT-DATA-LEN
           ELSE
      *        Error response - create normalized error
               MOVE SPACES TO WS-TEMP-STRING
               CALL "ERROR-FROM-HTTP-STATUS" USING
                   WS-HTTP-STATUS
                   WS-TEMP-STRING
                   WS-HTTP-BODY
                   LS-RESULT-ERROR
           END-IF
           
           PERFORM CLEANUP-HTTP.
      
      ******************************************************************
      * SET-HEADERS: Set HTTP request headers
      ******************************************************************
       SET-HEADERS.
      *    Content-Type header
           CALL "HTTP-ADD-HEADER" USING HTTP-HANDLE
                                        "Content-Type"
                                        "application/json"
                                        HTTP-RETURN-CODE
           
      *    Accept header
           CALL "HTTP-ADD-HEADER" USING HTTP-HANDLE
                                        "Accept"
                                        "application/json"
                                        HTTP-RETURN-CODE
           
      *    Authorization header if token present
           IF LS-AUTH-TOKEN NOT = SPACES
               STRING "Bearer " DELIMITED BY SIZE
                      LS-AUTH-TOKEN DELIMITED BY SPACE
                      INTO WS-TEMP-STRING
               CALL "HTTP-ADD-HEADER" USING HTTP-HANDLE
                                            "Authorization"
                                            WS-TEMP-STRING
                                            HTTP-RETURN-CODE
           END-IF.
      
      ******************************************************************
      * CLEANUP-HTTP: Clean up HTTP resources
      ******************************************************************
       CLEANUP-HTTP.
           CALL "HTTP-CLEANUP" USING HTTP-HANDLE
                                     HTTP-RETURN-CODE.
       
       END PROGRAM HTTP-CLIENT.
