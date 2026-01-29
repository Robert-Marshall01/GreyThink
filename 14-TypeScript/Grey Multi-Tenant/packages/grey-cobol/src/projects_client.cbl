      ******************************************************************
      * projects_client.cbl - Grey SDK Projects Client
      * Provides project-related operations.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECTS-CLIENT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY GREYCONF.
       COPY GREYERR.
       COPY GREYRES.
       COPY GREYPROJ.
       
       01  WS-JSON-BODY                PIC X(8192).
       01  WS-PATH                     PIC X(512).
       01  WS-QUERY                    PIC X(1024).
       01  WS-PAGE-STR                 PIC X(10).
       01  WS-PER-PAGE-STR             PIC X(10).
       
       LINKAGE SECTION.
       01  LS-CONFIG.
           05  LS-HOST                 PIC X(256).
           05  LS-PORT                 PIC 9(5).
           05  LS-USE-TLS              PIC X.
           05  LS-TIMEOUT              PIC 9(3).
           05  LS-AUTH-TOKEN           PIC X(2048).
       
       01  LS-PROJECT-REQUEST.
           05  LS-PROJ-ID-REQ          PIC X(64).
           05  LS-PROJ-NAME-REQ        PIC X(256).
           05  LS-PROJ-DESC-REQ        PIC X(1024).
           05  LS-PROJ-METADATA-REQ    PIC X(2048).
       
       01  LS-PROJECT-LIST-REQUEST.
           05  LS-PROJ-PAGE            PIC 9(5).
           05  LS-PROJ-PER-PAGE        PIC 9(3).
           05  LS-PROJ-SORT-BY         PIC X(32).
           05  LS-PROJ-SORT-ORDER      PIC X(4).
       
       01  LS-PROJECT-RESPONSE.
           05  LS-PROJ-ID              PIC X(64).
           05  LS-PROJ-NAME            PIC X(256).
           05  LS-PROJ-DESCRIPTION     PIC X(1024).
           05  LS-PROJ-METADATA        PIC X(2048).
           05  LS-PROJ-CREATED-AT      PIC X(32).
           05  LS-PROJ-UPDATED-AT      PIC X(32).
           05  LS-PROJ-OWNER-ID        PIC X(64).
       
       01  LS-PROJECT-LIST-RESPONSE.
           05  LS-PROJ-TOTAL-COUNT     PIC 9(8).
           05  LS-PROJ-PAGE-COUNT      PIC 9(5).
           05  LS-PROJ-CURRENT-PAGE    PIC 9(5).
           05  LS-PROJECT-COUNT        PIC 9(3).
           05  LS-PROJECTS OCCURS 100 TIMES.
               10  LS-PROJ-ITEM-ID     PIC X(64).
               10  LS-PROJ-ITEM-NAME   PIC X(256).
               10  LS-PROJ-ITEM-DESC   PIC X(256).
               10  LS-PROJ-ITEM-CREATED PIC X(32).
       
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
      * PROJECTS-LIST: List all projects with pagination
      * Input:  LS-CONFIG, LS-PROJECT-LIST-REQUEST
      * Output: LS-PROJECT-LIST-RESPONSE, LS-RESULT
      ******************************************************************
       ENTRY "PROJECTS-LIST" USING LS-CONFIG LS-PROJECT-LIST-REQUEST
                                   LS-PROJECT-LIST-RESPONSE LS-RESULT.
           PERFORM INIT-RESULT
           PERFORM INIT-LIST-RESPONSE
           
      *    Validate inputs
           IF LS-PROJ-PAGE < 1
               MOVE "N" TO LS-RESULT-OK
               CALL "ERROR-VALIDATION" USING
                   "Page must be >= 1"
                   SPACES
                   LS-RESULT-ERROR
               GOBACK
           END-IF
           
           IF LS-PROJ-PER-PAGE < 1 OR LS-PROJ-PER-PAGE > 100
               MOVE "N" TO LS-RESULT-OK
               CALL "ERROR-VALIDATION" USING
                   "Per page must be between 1 and 100"
                   SPACES
                   LS-RESULT-ERROR
               GOBACK
           END-IF
           
      *    Build query parameters
           MOVE LS-PROJ-PAGE TO WS-PAGE-STR
           MOVE LS-PROJ-PER-PAGE TO WS-PER-PAGE-STR
           
           MOVE SPACES TO WS-QUERY
           STRING "page=" DELIMITED BY SIZE
                  WS-PAGE-STR DELIMITED BY SPACE
                  "&per_page=" DELIMITED BY SIZE
                  WS-PER-PAGE-STR DELIMITED BY SPACE
                  "&sort_by=" DELIMITED BY SIZE
                  LS-PROJ-SORT-BY DELIMITED BY SPACE
                  "&sort_order=" DELIMITED BY SIZE
                  LS-PROJ-SORT-ORDER DELIMITED BY SPACE
                  INTO WS-QUERY
           
      *    Make HTTP request
           MOVE "/projects" TO WS-PATH
           CALL "HTTP-GET" USING LS-CONFIG WS-PATH WS-QUERY
                                 LS-RESULT
           
      *    Parse response if successful
           IF LS-RESULT-OK = "Y"
               PERFORM PARSE-LIST-RESPONSE
           END-IF
           
           GOBACK.
      
      ******************************************************************
      * PROJECTS-GET: Get project by ID
      * Input:  LS-CONFIG, LS-PROJECT-REQUEST (id)
      * Output: LS-PROJECT-RESPONSE, LS-RESULT
      ******************************************************************
       ENTRY "PROJECTS-GET" USING LS-CONFIG LS-PROJECT-REQUEST
                                  LS-PROJECT-RESPONSE LS-RESULT.
           PERFORM INIT-RESULT
           PERFORM INIT-PROJECT-RESPONSE
           
      *    Validate inputs
           IF LS-PROJ-ID-REQ = SPACES
               MOVE "N" TO LS-RESULT-OK
               CALL "ERROR-VALIDATION" USING
                   "Project ID is required"
                   SPACES
                   LS-RESULT-ERROR
               GOBACK
           END-IF
           
      *    Build path
           MOVE SPACES TO WS-PATH
           STRING "/projects/" DELIMITED BY SIZE
                  LS-PROJ-ID-REQ DELIMITED BY SPACE
                  INTO WS-PATH
           
      *    Make HTTP request
           MOVE SPACES TO WS-QUERY
           CALL "HTTP-GET" USING LS-CONFIG WS-PATH WS-QUERY
                                 LS-RESULT
           
      *    Parse response if successful
           IF LS-RESULT-OK = "Y"
               PERFORM PARSE-PROJECT-RESPONSE
           END-IF
           
           GOBACK.
      
      ******************************************************************
      * PROJECTS-CREATE: Create a new project
      * Input:  LS-CONFIG, LS-PROJECT-REQUEST (name, desc, metadata)
      * Output: LS-PROJECT-RESPONSE, LS-RESULT
      ******************************************************************
       ENTRY "PROJECTS-CREATE" USING LS-CONFIG LS-PROJECT-REQUEST
                                     LS-PROJECT-RESPONSE LS-RESULT.
           PERFORM INIT-RESULT
           PERFORM INIT-PROJECT-RESPONSE
           
      *    Validate inputs
           IF LS-PROJ-NAME-REQ = SPACES
               MOVE "N" TO LS-RESULT-OK
               CALL "ERROR-VALIDATION" USING
                   "Project name is required"
                   SPACES
                   LS-RESULT-ERROR
               GOBACK
           END-IF
           
      *    Build JSON request body
           MOVE SPACES TO WS-JSON-BODY
           STRING '{"name":"' DELIMITED BY SIZE
                  LS-PROJ-NAME-REQ DELIMITED BY SPACE
                  '"' DELIMITED BY SIZE
                  INTO WS-JSON-BODY
           
           IF LS-PROJ-DESC-REQ NOT = SPACES
               STRING WS-JSON-BODY DELIMITED BY SPACE
                      ',"description":"' DELIMITED BY SIZE
                      LS-PROJ-DESC-REQ DELIMITED BY SPACE
                      '"' DELIMITED BY SIZE
                      INTO WS-JSON-BODY
           END-IF
           
           IF LS-PROJ-METADATA-REQ NOT = SPACES
               STRING WS-JSON-BODY DELIMITED BY SPACE
                      ',"metadata":' DELIMITED BY SIZE
                      LS-PROJ-METADATA-REQ DELIMITED BY SPACE
                      INTO WS-JSON-BODY
           END-IF
           
           STRING WS-JSON-BODY DELIMITED BY SPACE
                  '}' DELIMITED BY SIZE
                  INTO WS-JSON-BODY
           
      *    Make HTTP request
           MOVE "/projects" TO WS-PATH
           CALL "HTTP-POST" USING LS-CONFIG WS-PATH WS-JSON-BODY
                                  LS-RESULT
           
      *    Parse response if successful
           IF LS-RESULT-OK = "Y"
               PERFORM PARSE-PROJECT-RESPONSE
           END-IF
           
           GOBACK.
      
      ******************************************************************
      * PROJECTS-UPDATE: Update an existing project
      * Input:  LS-CONFIG, LS-PROJECT-REQUEST (id, name, desc, metadata)
      * Output: LS-PROJECT-RESPONSE, LS-RESULT
      ******************************************************************
       ENTRY "PROJECTS-UPDATE" USING LS-CONFIG LS-PROJECT-REQUEST
                                     LS-PROJECT-RESPONSE LS-RESULT.
           PERFORM INIT-RESULT
           PERFORM INIT-PROJECT-RESPONSE
           
      *    Validate inputs
           IF LS-PROJ-ID-REQ = SPACES
               MOVE "N" TO LS-RESULT-OK
               CALL "ERROR-VALIDATION" USING
                   "Project ID is required"
                   SPACES
                   LS-RESULT-ERROR
               GOBACK
           END-IF
           
      *    Build JSON request body - only include non-empty fields
           MOVE SPACES TO WS-JSON-BODY
           MOVE "{" TO WS-JSON-BODY
           
           IF LS-PROJ-NAME-REQ NOT = SPACES
               STRING WS-JSON-BODY DELIMITED BY SPACE
                      '"name":"' DELIMITED BY SIZE
                      LS-PROJ-NAME-REQ DELIMITED BY SPACE
                      '"' DELIMITED BY SIZE
                      INTO WS-JSON-BODY
           END-IF
           
           IF LS-PROJ-DESC-REQ NOT = SPACES
               IF WS-JSON-BODY NOT = "{"
                   STRING WS-JSON-BODY DELIMITED BY SPACE
                          ',' DELIMITED BY SIZE
                          INTO WS-JSON-BODY
               END-IF
               STRING WS-JSON-BODY DELIMITED BY SPACE
                      '"description":"' DELIMITED BY SIZE
                      LS-PROJ-DESC-REQ DELIMITED BY SPACE
                      '"' DELIMITED BY SIZE
                      INTO WS-JSON-BODY
           END-IF
           
           STRING WS-JSON-BODY DELIMITED BY SPACE
                  '}' DELIMITED BY SIZE
                  INTO WS-JSON-BODY
           
      *    Build path
           MOVE SPACES TO WS-PATH
           STRING "/projects/" DELIMITED BY SIZE
                  LS-PROJ-ID-REQ DELIMITED BY SPACE
                  INTO WS-PATH
           
      *    Make HTTP request
           CALL "HTTP-PATCH" USING LS-CONFIG WS-PATH WS-JSON-BODY
                                   LS-RESULT
           
      *    Parse response if successful
           IF LS-RESULT-OK = "Y"
               PERFORM PARSE-PROJECT-RESPONSE
           END-IF
           
           GOBACK.
      
      ******************************************************************
      * PROJECTS-DELETE: Delete a project
      * Input:  LS-CONFIG, LS-PROJECT-REQUEST (id)
      * Output: LS-RESULT
      ******************************************************************
       ENTRY "PROJECTS-DELETE" USING LS-CONFIG LS-PROJECT-REQUEST
                                     LS-RESULT.
           PERFORM INIT-RESULT
           
      *    Validate inputs
           IF LS-PROJ-ID-REQ = SPACES
               MOVE "N" TO LS-RESULT-OK
               CALL "ERROR-VALIDATION" USING
                   "Project ID is required"
                   SPACES
                   LS-RESULT-ERROR
               GOBACK
           END-IF
           
      *    Build path
           MOVE SPACES TO WS-PATH
           STRING "/projects/" DELIMITED BY SIZE
                  LS-PROJ-ID-REQ DELIMITED BY SPACE
                  INTO WS-PATH
           
      *    Make HTTP request
           CALL "HTTP-DELETE" USING LS-CONFIG WS-PATH
                                    LS-RESULT
           
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
      * Internal: Initialize project response
      ******************************************************************
       INIT-PROJECT-RESPONSE.
           MOVE SPACES TO LS-PROJ-ID
           MOVE SPACES TO LS-PROJ-NAME
           MOVE SPACES TO LS-PROJ-DESCRIPTION
           MOVE SPACES TO LS-PROJ-METADATA
           MOVE SPACES TO LS-PROJ-CREATED-AT
           MOVE SPACES TO LS-PROJ-UPDATED-AT
           MOVE SPACES TO LS-PROJ-OWNER-ID.
      
      ******************************************************************
      * Internal: Initialize list response
      ******************************************************************
       INIT-LIST-RESPONSE.
           MOVE 0 TO LS-PROJ-TOTAL-COUNT
           MOVE 0 TO LS-PROJ-PAGE-COUNT
           MOVE 0 TO LS-PROJ-CURRENT-PAGE
           MOVE 0 TO LS-PROJECT-COUNT.
      
      ******************************************************************
      * Internal: Parse project response JSON
      ******************************************************************
       PARSE-PROJECT-RESPONSE.
           CALL "JSON-GET-STRING" USING LS-RESULT-DATA
                                        "id"
                                        LS-PROJ-ID
           
           CALL "JSON-GET-STRING" USING LS-RESULT-DATA
                                        "name"
                                        LS-PROJ-NAME
           
           CALL "JSON-GET-STRING" USING LS-RESULT-DATA
                                        "description"
                                        LS-PROJ-DESCRIPTION
           
           CALL "JSON-GET-STRING" USING LS-RESULT-DATA
                                        "created_at"
                                        LS-PROJ-CREATED-AT
           
           CALL "JSON-GET-STRING" USING LS-RESULT-DATA
                                        "updated_at"
                                        LS-PROJ-UPDATED-AT
           
           CALL "JSON-GET-STRING" USING LS-RESULT-DATA
                                        "owner_id"
                                        LS-PROJ-OWNER-ID.
      
      ******************************************************************
      * Internal: Parse project list response JSON
      ******************************************************************
       PARSE-LIST-RESPONSE.
           CALL "JSON-GET-NUMBER" USING LS-RESULT-DATA
                                        "total_count"
                                        LS-PROJ-TOTAL-COUNT
           
           CALL "JSON-GET-NUMBER" USING LS-RESULT-DATA
                                        "page_count"
                                        LS-PROJ-PAGE-COUNT
           
           CALL "JSON-GET-NUMBER" USING LS-RESULT-DATA
                                        "current_page"
                                        LS-PROJ-CURRENT-PAGE
           
           CALL "JSON-GET-ARRAY-LENGTH" USING LS-RESULT-DATA
                                              "projects"
                                              LS-PROJECT-COUNT.
      *    Parse individual project items would require array iteration
       
       END PROGRAM PROJECTS-CLIENT.
