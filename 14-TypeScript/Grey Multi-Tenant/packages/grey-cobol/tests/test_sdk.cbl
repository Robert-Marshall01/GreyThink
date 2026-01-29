      ******************************************************************
      * test_sdk.cbl - Grey SDK Test Program
      * Demonstrates SDK usage patterns.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-SDK.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY GREYCONF.
       COPY GREYERR.
       COPY GREYRES.
       COPY GREYAUTH.
       COPY GREYUSER.
       COPY GREYPROJ.
       COPY GREYQRY.
       
       01  WS-TEST-COUNT               PIC 9(3)   VALUE 0.
       01  WS-PASS-COUNT               PIC 9(3)   VALUE 0.
       01  WS-FAIL-COUNT               PIC 9(3)   VALUE 0.
       01  WS-PORT                     PIC 9(5)   VALUE 8080.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "=== Grey SDK COBOL Test Suite ==="
           DISPLAY ""
           
           PERFORM TEST-SDK-INIT
           PERFORM TEST-AUTH-VALIDATION
           PERFORM TEST-USER-VALIDATION
           PERFORM TEST-PROJECTS-VALIDATION
           PERFORM TEST-QUERY-VALIDATION
           PERFORM TEST-MUTATION-VALIDATION
           
           DISPLAY ""
           DISPLAY "=== Test Results ==="
           DISPLAY "Total:  " WS-TEST-COUNT
           DISPLAY "Passed: " WS-PASS-COUNT
           DISPLAY "Failed: " WS-FAIL-COUNT
           
           STOP RUN.
      
      ******************************************************************
      * Test SDK Initialization
      ******************************************************************
       TEST-SDK-INIT.
           DISPLAY "Testing SDK Initialization..."
           
      *    Test local initialization
           MOVE 8080 TO WS-PORT
           CALL "SDK-INIT-LOCAL" USING WS-PORT WS-GREY-CONFIG
           
           ADD 1 TO WS-TEST-COUNT
           IF WS-HOST = "localhost" AND WS-PORT = 8080
               ADD 1 TO WS-PASS-COUNT
               DISPLAY "  PASS: SDK-INIT-LOCAL"
           ELSE
               ADD 1 TO WS-FAIL-COUNT
               DISPLAY "  FAIL: SDK-INIT-LOCAL"
           END-IF
           
      *    Test TLS flag
           ADD 1 TO WS-TEST-COUNT
           IF WS-USE-TLS = "N"
               ADD 1 TO WS-PASS-COUNT
               DISPLAY "  PASS: Local TLS disabled"
           ELSE
               ADD 1 TO WS-FAIL-COUNT
               DISPLAY "  FAIL: Local TLS should be disabled"
           END-IF.
      
      ******************************************************************
      * Test Auth Validation
      ******************************************************************
       TEST-AUTH-VALIDATION.
           DISPLAY "Testing Auth Validation..."
           
      *    Test empty username
           MOVE SPACES TO WS-AUTH-USERNAME
           MOVE "password" TO WS-AUTH-PASSWORD
           CALL "AUTH-LOGIN" USING WS-GREY-CONFIG WS-AUTH-REQUEST
                                   WS-AUTH-RESPONSE WS-GREY-RESULT
           
           ADD 1 TO WS-TEST-COUNT
           IF WS-RESULT-OK = "N" AND 
              WS-RES-ERR-CODE = "VALIDATION-ERROR"
               ADD 1 TO WS-PASS-COUNT
               DISPLAY "  PASS: Empty username validation"
           ELSE
               ADD 1 TO WS-FAIL-COUNT
               DISPLAY "  FAIL: Empty username validation"
           END-IF
           
      *    Test empty password
           MOVE "testuser" TO WS-AUTH-USERNAME
           MOVE SPACES TO WS-AUTH-PASSWORD
           CALL "AUTH-LOGIN" USING WS-GREY-CONFIG WS-AUTH-REQUEST
                                   WS-AUTH-RESPONSE WS-GREY-RESULT
           
           ADD 1 TO WS-TEST-COUNT
           IF WS-RESULT-OK = "N" AND 
              WS-RES-ERR-CODE = "VALIDATION-ERROR"
               ADD 1 TO WS-PASS-COUNT
               DISPLAY "  PASS: Empty password validation"
           ELSE
               ADD 1 TO WS-FAIL-COUNT
               DISPLAY "  FAIL: Empty password validation"
           END-IF
           
      *    Test empty refresh token
           MOVE SPACES TO WS-AUTH-REFRESH-TOKEN
           CALL "AUTH-REFRESH" USING WS-GREY-CONFIG WS-AUTH-REQUEST
                                     WS-AUTH-RESPONSE WS-GREY-RESULT
           
           ADD 1 TO WS-TEST-COUNT
           IF WS-RESULT-OK = "N" AND 
              WS-RES-ERR-CODE = "VALIDATION-ERROR"
               ADD 1 TO WS-PASS-COUNT
               DISPLAY "  PASS: Empty refresh token validation"
           ELSE
               ADD 1 TO WS-FAIL-COUNT
               DISPLAY "  FAIL: Empty refresh token validation"
           END-IF.
      
      ******************************************************************
      * Test User Validation
      ******************************************************************
       TEST-USER-VALIDATION.
           DISPLAY "Testing User Validation..."
           
      *    Test empty user ID
           MOVE SPACES TO WS-USER-ID-REQ
           CALL "USER-GET" USING WS-GREY-CONFIG WS-USER-ID-REQ
                                 WS-USER-RESPONSE WS-GREY-RESULT
           
           ADD 1 TO WS-TEST-COUNT
           IF WS-RESULT-OK = "N" AND 
              WS-RES-ERR-CODE = "VALIDATION-ERROR"
               ADD 1 TO WS-PASS-COUNT
               DISPLAY "  PASS: Empty user ID validation"
           ELSE
               ADD 1 TO WS-FAIL-COUNT
               DISPLAY "  FAIL: Empty user ID validation"
           END-IF.
      
      ******************************************************************
      * Test Projects Validation
      ******************************************************************
       TEST-PROJECTS-VALIDATION.
           DISPLAY "Testing Projects Validation..."
           
      *    Test empty project ID for get
           MOVE SPACES TO WS-PROJ-ID-REQ
           CALL "PROJECTS-GET" USING WS-GREY-CONFIG WS-PROJECT-REQUEST
                                     WS-PROJECT-RESPONSE WS-GREY-RESULT
           
           ADD 1 TO WS-TEST-COUNT
           IF WS-RESULT-OK = "N" AND 
              WS-RES-ERR-CODE = "VALIDATION-ERROR"
               ADD 1 TO WS-PASS-COUNT
               DISPLAY "  PASS: Empty project ID validation (get)"
           ELSE
               ADD 1 TO WS-FAIL-COUNT
               DISPLAY "  FAIL: Empty project ID validation (get)"
           END-IF
           
      *    Test empty project name for create
           MOVE SPACES TO WS-PROJ-NAME-REQ
           CALL "PROJECTS-CREATE" USING WS-GREY-CONFIG 
                                        WS-PROJECT-REQUEST
                                        WS-PROJECT-RESPONSE 
                                        WS-GREY-RESULT
           
           ADD 1 TO WS-TEST-COUNT
           IF WS-RESULT-OK = "N" AND 
              WS-RES-ERR-CODE = "VALIDATION-ERROR"
               ADD 1 TO WS-PASS-COUNT
               DISPLAY "  PASS: Empty project name validation"
           ELSE
               ADD 1 TO WS-FAIL-COUNT
               DISPLAY "  FAIL: Empty project name validation"
           END-IF
           
      *    Test invalid page number
           MOVE 0 TO WS-PROJ-PAGE
           MOVE 20 TO WS-PROJ-PER-PAGE
           CALL "PROJECTS-LIST" USING WS-GREY-CONFIG 
                                      WS-PROJECT-LIST-REQUEST
                                      WS-PROJECT-LIST-RESPONSE 
                                      WS-GREY-RESULT
           
           ADD 1 TO WS-TEST-COUNT
           IF WS-RESULT-OK = "N" AND 
              WS-RES-ERR-CODE = "VALIDATION-ERROR"
               ADD 1 TO WS-PASS-COUNT
               DISPLAY "  PASS: Invalid page validation"
           ELSE
               ADD 1 TO WS-FAIL-COUNT
               DISPLAY "  FAIL: Invalid page validation"
           END-IF.
      
      ******************************************************************
      * Test Query Validation
      ******************************************************************
       TEST-QUERY-VALIDATION.
           DISPLAY "Testing Query Validation..."
           
      *    Test empty query string
           MOVE SPACES TO WS-QRY-STRING
           CALL "QUERY-EXECUTE" USING WS-GREY-CONFIG WS-QUERY-REQUEST
                                      WS-QUERY-RESPONSE WS-GREY-RESULT
           
           ADD 1 TO WS-TEST-COUNT
           IF WS-RESULT-OK = "N" AND 
              WS-RES-ERR-CODE = "VALIDATION-ERROR"
               ADD 1 TO WS-PASS-COUNT
               DISPLAY "  PASS: Empty query string validation"
           ELSE
               ADD 1 TO WS-FAIL-COUNT
               DISPLAY "  FAIL: Empty query string validation"
           END-IF
           
      *    Test empty batch
           MOVE 0 TO WS-BATCH-COUNT
           CALL "QUERY-BATCH" USING WS-GREY-CONFIG WS-BATCH-REQUEST
                                    WS-BATCH-RESPONSE WS-GREY-RESULT
           
           ADD 1 TO WS-TEST-COUNT
           IF WS-RESULT-OK = "N" AND 
              WS-RES-ERR-CODE = "VALIDATION-ERROR"
               ADD 1 TO WS-PASS-COUNT
               DISPLAY "  PASS: Empty batch validation"
           ELSE
               ADD 1 TO WS-FAIL-COUNT
               DISPLAY "  FAIL: Empty batch validation"
           END-IF.
      
      ******************************************************************
      * Test Mutation Validation
      ******************************************************************
       TEST-MUTATION-VALIDATION.
           DISPLAY "Testing Mutation Validation..."
           
      *    Test empty mutation string
           MOVE SPACES TO WS-MUT-STRING
           CALL "MUTATION-EXECUTE" USING WS-GREY-CONFIG 
                                         WS-MUTATION-REQUEST
                                         WS-MUTATION-RESPONSE 
                                         WS-GREY-RESULT
           
           ADD 1 TO WS-TEST-COUNT
           IF WS-RESULT-OK = "N" AND 
              WS-RES-ERR-CODE = "VALIDATION-ERROR"
               ADD 1 TO WS-PASS-COUNT
               DISPLAY "  PASS: Empty mutation string validation"
           ELSE
               ADD 1 TO WS-FAIL-COUNT
               DISPLAY "  FAIL: Empty mutation string validation"
           END-IF
           
      *    Test empty mutation batch
           MOVE 0 TO WS-BATCH-COUNT
           CALL "MUTATION-BATCH" USING WS-GREY-CONFIG WS-BATCH-REQUEST
                                       WS-BATCH-RESPONSE WS-GREY-RESULT
           
           ADD 1 TO WS-TEST-COUNT
           IF WS-RESULT-OK = "N" AND 
              WS-RES-ERR-CODE = "VALIDATION-ERROR"
               ADD 1 TO WS-PASS-COUNT
               DISPLAY "  PASS: Empty mutation batch validation"
           ELSE
               ADD 1 TO WS-FAIL-COUNT
               DISPLAY "  FAIL: Empty mutation batch validation"
           END-IF.
       
       END PROGRAM TEST-SDK.
