      ******************************************************************
      * query_client.cbl - Grey SDK Query Client
      * Provides GraphQL-style query operations.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUERY-CLIENT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY GREYCONF.
       COPY GREYERR.
       COPY GREYRES.
       COPY GREYQRY.
       
       01  WS-JSON-BODY                PIC X(16000).
       01  WS-PATH                     PIC X(512).
       01  WS-IDX                      PIC 9(3).
       
       LINKAGE SECTION.
       01  LS-CONFIG.
           05  LS-HOST                 PIC X(256).
           05  LS-PORT                 PIC 9(5).
           05  LS-USE-TLS              PIC X.
           05  LS-TIMEOUT              PIC 9(3).
           05  LS-AUTH-TOKEN           PIC X(2048).
       
       01  LS-QUERY-REQUEST.
           05  LS-QRY-STRING           PIC X(8000).
           05  LS-QRY-VARIABLES        PIC X(4000).
           05  LS-QRY-OPERATION-NAME   PIC X(256).
       
       01  LS-QUERY-RESPONSE.
           05  LS-QRY-DATA             PIC X(32000).
           05  LS-QRY-DATA-LEN         PIC 9(5).
           05  LS-QRY-ERRORS           PIC X(4000).
           05  LS-QRY-HAS-ERRORS       PIC X.
       
       01  LS-BATCH-REQUEST.
           05  LS-BATCH-COUNT          PIC 9(3).
           05  LS-BATCH-ITEMS OCCURS 50 TIMES.
               10  LS-BATCH-STRING     PIC X(4000).
               10  LS-BATCH-VARS       PIC X(2000).
               10  LS-BATCH-OP-NAME    PIC X(128).
       
       01  LS-BATCH-RESPONSE.
           05  LS-BATCH-RESULT-COUNT   PIC 9(3).
           05  LS-BATCH-RESULTS OCCURS 50 TIMES.
               10  LS-BATCH-RES-DATA   PIC X(8000).
               10  LS-BATCH-RES-OK     PIC X.
       
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
      * QUERY-EXECUTE: Execute a single query
      * Input:  LS-CONFIG, LS-QUERY-REQUEST
      * Output: LS-QUERY-RESPONSE, LS-RESULT
      ******************************************************************
       ENTRY "QUERY-EXECUTE" USING LS-CONFIG LS-QUERY-REQUEST
                                   LS-QUERY-RESPONSE LS-RESULT.
           PERFORM INIT-RESULT
           PERFORM INIT-QUERY-RESPONSE
           
      *    Validate inputs
           IF LS-QRY-STRING = SPACES
               MOVE "N" TO LS-RESULT-OK
               CALL "ERROR-VALIDATION" USING
                   "Query string is required"
                   SPACES
                   LS-RESULT-ERROR
               GOBACK
           END-IF
           
      *    Build JSON request body
           MOVE SPACES TO WS-JSON-BODY
           STRING '{"query":"' DELIMITED BY SIZE
                  LS-QRY-STRING DELIMITED BY SPACE
                  '"' DELIMITED BY SIZE
                  INTO WS-JSON-BODY
           
           IF LS-QRY-VARIABLES NOT = SPACES
               STRING WS-JSON-BODY DELIMITED BY SPACE
                      ',"variables":' DELIMITED BY SIZE
                      LS-QRY-VARIABLES DELIMITED BY SPACE
                      INTO WS-JSON-BODY
           END-IF
           
           IF LS-QRY-OPERATION-NAME NOT = SPACES
               STRING WS-JSON-BODY DELIMITED BY SPACE
                      ',"operationName":"' DELIMITED BY SIZE
                      LS-QRY-OPERATION-NAME DELIMITED BY SPACE
                      '"' DELIMITED BY SIZE
                      INTO WS-JSON-BODY
           END-IF
           
           STRING WS-JSON-BODY DELIMITED BY SPACE
                  '}' DELIMITED BY SIZE
                  INTO WS-JSON-BODY
           
      *    Make HTTP request
           MOVE "/graphql" TO WS-PATH
           CALL "HTTP-POST" USING LS-CONFIG WS-PATH WS-JSON-BODY
                                  LS-RESULT
           
      *    Parse response if successful
           IF LS-RESULT-OK = "Y"
               PERFORM PARSE-QUERY-RESPONSE
           END-IF
           
           GOBACK.
      
      ******************************************************************
      * QUERY-BATCH: Execute multiple queries
      * Input:  LS-CONFIG, LS-BATCH-REQUEST
      * Output: LS-BATCH-RESPONSE, LS-RESULT
      ******************************************************************
       ENTRY "QUERY-BATCH" USING LS-CONFIG LS-BATCH-REQUEST
                                 LS-BATCH-RESPONSE LS-RESULT.
           PERFORM INIT-RESULT
           PERFORM INIT-BATCH-RESPONSE
           
      *    Validate inputs
           IF LS-BATCH-COUNT < 1
               MOVE "N" TO LS-RESULT-OK
               CALL "ERROR-VALIDATION" USING
                   "At least one query is required"
                   SPACES
                   LS-RESULT-ERROR
               GOBACK
           END-IF
           
      *    Validate each query
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > LS-BATCH-COUNT
               IF LS-BATCH-STRING(WS-IDX) = SPACES
                   MOVE "N" TO LS-RESULT-OK
                   CALL "ERROR-VALIDATION" USING
                       "Query in batch cannot be empty"
                       SPACES
                       LS-RESULT-ERROR
                   GOBACK
               END-IF
           END-PERFORM
           
      *    Build JSON request body
           MOVE SPACES TO WS-JSON-BODY
           MOVE '{"queries":[' TO WS-JSON-BODY
           
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > LS-BATCH-COUNT
               IF WS-IDX > 1
                   STRING WS-JSON-BODY DELIMITED BY SPACE
                          ',' DELIMITED BY SIZE
                          INTO WS-JSON-BODY
               END-IF
               STRING WS-JSON-BODY DELIMITED BY SPACE
                      '{"query":"' DELIMITED BY SIZE
                      LS-BATCH-STRING(WS-IDX) DELIMITED BY SPACE
                      '"}' DELIMITED BY SIZE
                      INTO WS-JSON-BODY
           END-PERFORM
           
           STRING WS-JSON-BODY DELIMITED BY SPACE
                  ']}' DELIMITED BY SIZE
                  INTO WS-JSON-BODY
           
      *    Make HTTP request
           MOVE "/graphql/batch" TO WS-PATH
           CALL "HTTP-POST" USING LS-CONFIG WS-PATH WS-JSON-BODY
                                  LS-RESULT
           
      *    Parse response if successful
           IF LS-RESULT-OK = "Y"
               PERFORM PARSE-BATCH-RESPONSE
           END-IF
           
           GOBACK.
      
      ******************************************************************
      * Internal: Initialize result structure
      ******************************************************************
       INIT-RESULT.
           MOVE "N" TO LS-RESULT-OK
           MOVE SPACES TO LS-RESULT-DATA
           MOVE 0 TO LS-RESULT-DATA-LEN
           MOVE SPACES TO LS-RES-ERR-CODE
           MOVE SPACES TO LS-RES-ERR-MSG
           MOVE SPACES TO LS-RES-ERR-DETAILS.
      
      ******************************************************************
      * Internal: Initialize query response
      ******************************************************************
       INIT-QUERY-RESPONSE.
           MOVE SPACES TO LS-QRY-DATA
           MOVE 0 TO LS-QRY-DATA-LEN
           MOVE SPACES TO LS-QRY-ERRORS
           MOVE "N" TO LS-QRY-HAS-ERRORS.
      
      ******************************************************************
      * Internal: Initialize batch response
      ******************************************************************
       INIT-BATCH-RESPONSE.
           MOVE 0 TO LS-BATCH-RESULT-COUNT.
      
      ******************************************************************
      * Internal: Parse query response JSON
      ******************************************************************
       PARSE-QUERY-RESPONSE.
           MOVE LS-RESULT-DATA TO LS-QRY-DATA
           MOVE LS-RESULT-DATA-LEN TO LS-QRY-DATA-LEN
           
      *    Check for errors in response
           CALL "JSON-HAS-KEY" USING LS-RESULT-DATA
                                     "errors"
                                     LS-QRY-HAS-ERRORS.
      
      ******************************************************************
      * Internal: Parse batch response JSON
      ******************************************************************
       PARSE-BATCH-RESPONSE.
           CALL "JSON-GET-ARRAY-LENGTH" USING LS-RESULT-DATA
                                              "results"
                                              LS-BATCH-RESULT-COUNT.
       
       END PROGRAM QUERY-CLIENT.
