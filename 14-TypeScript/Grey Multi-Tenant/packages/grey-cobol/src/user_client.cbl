      ******************************************************************
      * user_client.cbl - Grey SDK User Client
      * Provides user-related operations.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. USER-CLIENT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY GREYCONF.
       COPY GREYERR.
       COPY GREYRES.
       COPY GREYUSER.
       
       01  WS-PATH                     PIC X(512).
       01  WS-QUERY                    PIC X(1024).
       
       LINKAGE SECTION.
       01  LS-CONFIG.
           05  LS-HOST                 PIC X(256).
           05  LS-PORT                 PIC 9(5).
           05  LS-USE-TLS              PIC X.
           05  LS-TIMEOUT              PIC 9(3).
           05  LS-AUTH-TOKEN           PIC X(2048).
       
       01  LS-USER-ID-IN               PIC X(64).
       
       01  LS-USER-RESPONSE.
           05  LS-USER-ID              PIC X(64).
           05  LS-USER-USERNAME        PIC X(256).
           05  LS-USER-EMAIL           PIC X(256).
           05  LS-USER-FIRST-NAME      PIC X(128).
           05  LS-USER-LAST-NAME       PIC X(128).
           05  LS-USER-CREATED-AT      PIC X(32).
           05  LS-USER-UPDATED-AT      PIC X(32).
           05  LS-USER-STATUS          PIC X(32).
       
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
      * USER-GET: Get user by ID
      * Input:  LS-CONFIG, LS-USER-ID-IN
      * Output: LS-USER-RESPONSE, LS-RESULT
      ******************************************************************
       ENTRY "USER-GET" USING LS-CONFIG LS-USER-ID-IN
                              LS-USER-RESPONSE LS-RESULT.
           PERFORM INIT-RESULT
           PERFORM INIT-USER-RESPONSE
           
      *    Validate inputs
           IF LS-USER-ID-IN = SPACES
               MOVE "N" TO LS-RESULT-OK
               CALL "ERROR-VALIDATION" USING
                   "User ID is required"
                   SPACES
                   LS-RESULT-ERROR
               GOBACK
           END-IF
           
      *    Build path
           MOVE SPACES TO WS-PATH
           STRING "/users/" DELIMITED BY SIZE
                  LS-USER-ID-IN DELIMITED BY SPACE
                  INTO WS-PATH
           
      *    Make HTTP request
           MOVE SPACES TO WS-QUERY
           CALL "HTTP-GET" USING LS-CONFIG WS-PATH WS-QUERY
                                 LS-RESULT
           
      *    Parse response if successful
           IF LS-RESULT-OK = "Y"
               PERFORM PARSE-USER-RESPONSE
           END-IF
           
           GOBACK.
      
      ******************************************************************
      * USER-GET-CURRENT: Get current authenticated user
      * Input:  LS-CONFIG
      * Output: LS-USER-RESPONSE, LS-RESULT
      ******************************************************************
       ENTRY "USER-GET-CURRENT" USING LS-CONFIG
                                      LS-USER-RESPONSE LS-RESULT.
           PERFORM INIT-RESULT
           PERFORM INIT-USER-RESPONSE
           
      *    Make HTTP request
           MOVE "/users/me" TO WS-PATH
           MOVE SPACES TO WS-QUERY
           CALL "HTTP-GET" USING LS-CONFIG WS-PATH WS-QUERY
                                 LS-RESULT
           
      *    Parse response if successful
           IF LS-RESULT-OK = "Y"
               PERFORM PARSE-USER-RESPONSE
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
      * Internal: Initialize user response
      ******************************************************************
       INIT-USER-RESPONSE.
           MOVE SPACES TO LS-USER-ID
           MOVE SPACES TO LS-USER-USERNAME
           MOVE SPACES TO LS-USER-EMAIL
           MOVE SPACES TO LS-USER-FIRST-NAME
           MOVE SPACES TO LS-USER-LAST-NAME
           MOVE SPACES TO LS-USER-CREATED-AT
           MOVE SPACES TO LS-USER-UPDATED-AT
           MOVE SPACES TO LS-USER-STATUS.
      
      ******************************************************************
      * Internal: Parse user response JSON
      ******************************************************************
       PARSE-USER-RESPONSE.
           CALL "JSON-GET-STRING" USING LS-RESULT-DATA
                                        "id"
                                        LS-USER-ID
           
           CALL "JSON-GET-STRING" USING LS-RESULT-DATA
                                        "username"
                                        LS-USER-USERNAME
           
           CALL "JSON-GET-STRING" USING LS-RESULT-DATA
                                        "email"
                                        LS-USER-EMAIL
           
           CALL "JSON-GET-STRING" USING LS-RESULT-DATA
                                        "first_name"
                                        LS-USER-FIRST-NAME
           
           CALL "JSON-GET-STRING" USING LS-RESULT-DATA
                                        "last_name"
                                        LS-USER-LAST-NAME
           
           CALL "JSON-GET-STRING" USING LS-RESULT-DATA
                                        "created_at"
                                        LS-USER-CREATED-AT
           
           CALL "JSON-GET-STRING" USING LS-RESULT-DATA
                                        "updated_at"
                                        LS-USER-UPDATED-AT
           
           CALL "JSON-GET-STRING" USING LS-RESULT-DATA
                                        "status"
                                        LS-USER-STATUS.
       
       END PROGRAM USER-CLIENT.
