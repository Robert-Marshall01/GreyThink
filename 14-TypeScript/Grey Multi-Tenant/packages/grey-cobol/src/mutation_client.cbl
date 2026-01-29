      ******************************************************************
      * mutation_client.cbl - Grey SDK Mutation Client
      * Provides GraphQL-style mutation operations.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MUTATION-CLIENT.
       
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
       
       01  LS-MUTATION-REQUEST.
           05  LS-MUT-STRING           PIC X(8000).
           05  LS-MUT-VARIABLES        PIC X(4000).
           05  LS-MUT-OPERATION-NAME   PIC X(256).
       
       01  LS-MUTATION-RESPONSE.
           05  LS-MUT-DATA             PIC X(32000).
           05  LS-MUT-DATA-LEN         PIC 9(5).
           05  LS-MUT-ERRORS           PIC X(4000).
           05  LS-MUT-HAS-ERRORS       PIC X.
       
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
      * MUTATION-EXECUTE: Execute a single mutation
      * Input:  LS-CONFIG, LS-MUTATION-REQUEST
      * Output: LS-MUTATION-RESPONSE, LS-RESULT
      ******************************************************************
       ENTRY "MUTATION-EXECUTE" USING LS-CONFIG LS-MUTATION-REQUEST
                                      LS-MUTATION-RESPONSE LS-RESULT.
           PERFORM INIT-RESULT
           PERFORM INIT-MUTATION-RESPONSE
           
      *    Validate inputs
           IF LS-MUT-STRING = SPACES
               MOVE "N" TO LS-RESULT-OK
               CALL "ERROR-VALIDATION" USING
                   "Mutation string is required"
                   SPACES
                   LS-RESULT-ERROR
               GOBACK
           END-IF
           
      *    Build JSON request body
           MOVE SPACES TO WS-JSON-BODY
           STRING '{"mutation":"' DELIMITED BY SIZE
                  LS-MUT-STRING DELIMITED BY SPACE
                  '"' DELIMITED BY SIZE
                  INTO WS-JSON-BODY
           
           IF LS-MUT-VARIABLES NOT = SPACES
               STRING WS-JSON-BODY DELIMITED BY SPACE
                      ',"variables":' DELIMITED BY SIZE
                      LS-MUT-VARIABLES DELIMITED BY SPACE
                      INTO WS-JSON-BODY
           END-IF
           
           IF LS-MUT-OPERATION-NAME NOT = SPACES
               STRING WS-JSON-BODY DELIMITED BY SPACE
                      ',"operationName":"' DELIMITED BY SIZE
                      LS-MUT-OPERATION-NAME DELIMITED BY SPACE
                      '"' DELIMITED BY SIZE
                      INTO WS-JSON-BODY
           END-IF
           
           STRING WS-JSON-BODY DELIMITED BY SPACE
                  '}' DELIMITED BY SIZE
                  INTO WS-JSON-BODY
           
      *    Make HTTP request
           MOVE "/graphql/mutate" TO WS-PATH
           CALL "HTTP-POST" USING LS-CONFIG WS-PATH WS-JSON-BODY
                                  LS-RESULT
           
      *    Parse response if successful
           IF LS-RESULT-OK = "Y"
               PERFORM PARSE-MUTATION-RESPONSE
           END-IF
           
           GOBACK.
      
      ******************************************************************
      * MUTATION-BATCH: Execute multiple mutations
      * Input:  LS-CONFIG, LS-BATCH-REQUEST
      * Output: LS-BATCH-RESPONSE, LS-RESULT
      ******************************************************************
       ENTRY "MUTATION-BATCH" USING LS-CONFIG LS-BATCH-REQUEST
                                    LS-BATCH-RESPONSE LS-RESULT.
           PERFORM INIT-RESULT
           PERFORM INIT-BATCH-RESPONSE
           
      *    Validate inputs
           IF LS-BATCH-COUNT < 1
               MOVE "N" TO LS-RESULT-OK
               CALL "ERROR-VALIDATION" USING
                   "At least one mutation is required"
                   SPACES
                   LS-RESULT-ERROR
               GOBACK
           END-IF
           
      *    Validate each mutation
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > LS-BATCH-COUNT
               IF LS-BATCH-STRING(WS-IDX) = SPACES
                   MOVE "N" TO LS-RESULT-OK
                   CALL "ERROR-VALIDATION" USING
                       "Mutation in batch cannot be empty"
                       SPACES
                       LS-RESULT-ERROR
                   GOBACK
               END-IF
           END-PERFORM
           
      *    Build JSON request body
           MOVE SPACES TO WS-JSON-BODY
           MOVE '{"mutations":[' TO WS-JSON-BODY
           
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > LS-BATCH-COUNT
               IF WS-IDX > 1
                   STRING WS-JSON-BODY DELIMITED BY SPACE
                          ',' DELIMITED BY SIZE
                          INTO WS-JSON-BODY
               END-IF
               STRING WS-JSON-BODY DELIMITED BY SPACE
                      '{"mutation":"' DELIMITED BY SIZE
                      LS-BATCH-STRING(WS-IDX) DELIMITED BY SPACE
                      '"}' DELIMITED BY SIZE
                      INTO WS-JSON-BODY
           END-PERFORM
           
           STRING WS-JSON-BODY DELIMITED BY SPACE
                  ']}' DELIMITED BY SIZE
                  INTO WS-JSON-BODY
           
      *    Make HTTP request
           MOVE "/graphql/mutate/batch" TO WS-PATH
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
      * Internal: Initialize mutation response
      ******************************************************************
       INIT-MUTATION-RESPONSE.
           MOVE SPACES TO LS-MUT-DATA
           MOVE 0 TO LS-MUT-DATA-LEN
           MOVE SPACES TO LS-MUT-ERRORS
           MOVE "N" TO LS-MUT-HAS-ERRORS.
      
      ******************************************************************
      * Internal: Initialize batch response
      ******************************************************************
       INIT-BATCH-RESPONSE.
           MOVE 0 TO LS-BATCH-RESULT-COUNT.
      
      ******************************************************************
      * Internal: Parse mutation response JSON
      ******************************************************************
       PARSE-MUTATION-RESPONSE.
           MOVE LS-RESULT-DATA TO LS-MUT-DATA
           MOVE LS-RESULT-DATA-LEN TO LS-MUT-DATA-LEN
           
      *    Check for errors in response
           CALL "JSON-HAS-KEY" USING LS-RESULT-DATA
                                     "errors"
                                     LS-MUT-HAS-ERRORS.
      
      ******************************************************************
      * Internal: Parse batch response JSON
      ******************************************************************
       PARSE-BATCH-RESPONSE.
           CALL "JSON-GET-ARRAY-LENGTH" USING LS-RESULT-DATA
                                              "results"
                                              LS-BATCH-RESULT-COUNT.
       
       END PROGRAM MUTATION-CLIENT.
