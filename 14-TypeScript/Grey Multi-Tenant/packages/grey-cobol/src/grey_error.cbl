      ******************************************************************
      * grey_error.cbl - Grey SDK Error Handling Module
      * Provides error creation and normalization routines.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GREY-ERROR.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY GREYERR.
       
       01  WS-HTTP-STATUS-IN           PIC 9(3).
       01  WS-MESSAGE-IN               PIC X(256).
       01  WS-DETAILS-IN               PIC X(1024).
       
       LINKAGE SECTION.
       01  LS-HTTP-STATUS              PIC 9(3).
       01  LS-MESSAGE                  PIC X(256).
       01  LS-DETAILS                  PIC X(1024).
       01  LS-ERROR.
           05  LS-ERR-CODE             PIC X(32).
           05  LS-ERR-MESSAGE          PIC X(256).
           05  LS-ERR-DETAILS          PIC X(1024).
       
       PROCEDURE DIVISION.
      
      ******************************************************************
      * ERROR-FROM-HTTP-STATUS: Convert HTTP status to error code
      * Input:  LS-HTTP-STATUS
      * Output: LS-ERROR
      ******************************************************************
       ENTRY "ERROR-FROM-HTTP-STATUS" USING LS-HTTP-STATUS
                                            LS-MESSAGE
                                            LS-DETAILS
                                            LS-ERROR.
           PERFORM INIT-ERROR
           MOVE LS-MESSAGE TO LS-ERR-MESSAGE
           MOVE LS-DETAILS TO LS-ERR-DETAILS
           
           EVALUATE TRUE
               WHEN LS-HTTP-STATUS = 401
                   MOVE EC-UNAUTHORIZED TO LS-ERR-CODE
                   IF LS-ERR-MESSAGE = SPACES
                       MOVE "Authentication required" TO LS-ERR-MESSAGE
                   END-IF
               
               WHEN LS-HTTP-STATUS = 403
                   MOVE EC-FORBIDDEN TO LS-ERR-CODE
                   IF LS-ERR-MESSAGE = SPACES
                       MOVE "Permission denied" TO LS-ERR-MESSAGE
                   END-IF
               
               WHEN LS-HTTP-STATUS = 404
                   MOVE EC-NOT-FOUND TO LS-ERR-CODE
                   IF LS-ERR-MESSAGE = SPACES
                       MOVE "Resource not found" TO LS-ERR-MESSAGE
                   END-IF
               
               WHEN LS-HTTP-STATUS = 400 OR LS-HTTP-STATUS = 422
                   MOVE EC-VALIDATION-ERROR TO LS-ERR-CODE
                   IF LS-ERR-MESSAGE = SPACES
                       MOVE "Validation error" TO LS-ERR-MESSAGE
                   END-IF
               
               WHEN LS-HTTP-STATUS = 408 OR LS-HTTP-STATUS = 504
                   MOVE EC-TIMEOUT TO LS-ERR-CODE
                   IF LS-ERR-MESSAGE = SPACES
                       MOVE "Request timed out" TO LS-ERR-MESSAGE
                   END-IF
               
               WHEN LS-HTTP-STATUS >= 500 AND LS-HTTP-STATUS < 600
                   MOVE EC-SERVER-ERROR TO LS-ERR-CODE
                   IF LS-ERR-MESSAGE = SPACES
                       MOVE "Server error occurred" TO LS-ERR-MESSAGE
                   END-IF
               
               WHEN OTHER
                   MOVE EC-UNKNOWN TO LS-ERR-CODE
                   IF LS-ERR-MESSAGE = SPACES
                       STRING "HTTP error " LS-HTTP-STATUS
                           DELIMITED BY SIZE INTO LS-ERR-MESSAGE
                   END-IF
           END-EVALUATE
           
           GOBACK.
      
      ******************************************************************
      * ERROR-UNAUTHORIZED: Create unauthorized error
      ******************************************************************
       ENTRY "ERROR-UNAUTHORIZED" USING LS-MESSAGE LS-ERROR.
           PERFORM INIT-ERROR
           MOVE EC-UNAUTHORIZED TO LS-ERR-CODE
           IF LS-MESSAGE NOT = SPACES
               MOVE LS-MESSAGE TO LS-ERR-MESSAGE
           ELSE
               MOVE "Authentication required" TO LS-ERR-MESSAGE
           END-IF
           GOBACK.
      
      ******************************************************************
      * ERROR-FORBIDDEN: Create forbidden error
      ******************************************************************
       ENTRY "ERROR-FORBIDDEN" USING LS-MESSAGE LS-ERROR.
           PERFORM INIT-ERROR
           MOVE EC-FORBIDDEN TO LS-ERR-CODE
           IF LS-MESSAGE NOT = SPACES
               MOVE LS-MESSAGE TO LS-ERR-MESSAGE
           ELSE
               MOVE "Permission denied" TO LS-ERR-MESSAGE
           END-IF
           GOBACK.
      
      ******************************************************************
      * ERROR-NOT-FOUND: Create not found error
      ******************************************************************
       ENTRY "ERROR-NOT-FOUND" USING LS-MESSAGE LS-ERROR.
           PERFORM INIT-ERROR
           MOVE EC-NOT-FOUND TO LS-ERR-CODE
           IF LS-MESSAGE NOT = SPACES
               MOVE LS-MESSAGE TO LS-ERR-MESSAGE
           ELSE
               MOVE "Resource not found" TO LS-ERR-MESSAGE
           END-IF
           GOBACK.
      
      ******************************************************************
      * ERROR-VALIDATION: Create validation error
      ******************************************************************
       ENTRY "ERROR-VALIDATION" USING LS-MESSAGE LS-DETAILS LS-ERROR.
           PERFORM INIT-ERROR
           MOVE EC-VALIDATION-ERROR TO LS-ERR-CODE
           MOVE LS-MESSAGE TO LS-ERR-MESSAGE
           MOVE LS-DETAILS TO LS-ERR-DETAILS
           GOBACK.
      
      ******************************************************************
      * ERROR-NETWORK: Create network error
      ******************************************************************
       ENTRY "ERROR-NETWORK" USING LS-MESSAGE LS-ERROR.
           PERFORM INIT-ERROR
           MOVE EC-NETWORK-ERROR TO LS-ERR-CODE
           IF LS-MESSAGE NOT = SPACES
               MOVE LS-MESSAGE TO LS-ERR-MESSAGE
           ELSE
               MOVE "Network error occurred" TO LS-ERR-MESSAGE
           END-IF
           GOBACK.
      
      ******************************************************************
      * ERROR-TIMEOUT: Create timeout error
      ******************************************************************
       ENTRY "ERROR-TIMEOUT" USING LS-MESSAGE LS-ERROR.
           PERFORM INIT-ERROR
           MOVE EC-TIMEOUT TO LS-ERR-CODE
           IF LS-MESSAGE NOT = SPACES
               MOVE LS-MESSAGE TO LS-ERR-MESSAGE
           ELSE
               MOVE "Request timed out" TO LS-ERR-MESSAGE
           END-IF
           GOBACK.
      
      ******************************************************************
      * ERROR-SERVER: Create server error
      ******************************************************************
       ENTRY "ERROR-SERVER" USING LS-MESSAGE LS-ERROR.
           PERFORM INIT-ERROR
           MOVE EC-SERVER-ERROR TO LS-ERR-CODE
           IF LS-MESSAGE NOT = SPACES
               MOVE LS-MESSAGE TO LS-ERR-MESSAGE
           ELSE
               MOVE "Server error occurred" TO LS-ERR-MESSAGE
           END-IF
           GOBACK.
      
      ******************************************************************
      * ERROR-UNKNOWN: Create unknown error
      ******************************************************************
       ENTRY "ERROR-UNKNOWN" USING LS-MESSAGE LS-ERROR.
           PERFORM INIT-ERROR
           MOVE EC-UNKNOWN TO LS-ERR-CODE
           IF LS-MESSAGE NOT = SPACES
               MOVE LS-MESSAGE TO LS-ERR-MESSAGE
           ELSE
               MOVE "An unknown error occurred" TO LS-ERR-MESSAGE
           END-IF
           GOBACK.
      
      ******************************************************************
      * Internal: Initialize error structure
      ******************************************************************
       INIT-ERROR.
           MOVE SPACES TO LS-ERR-CODE
           MOVE SPACES TO LS-ERR-MESSAGE
           MOVE SPACES TO LS-ERR-DETAILS.
       
       END PROGRAM GREY-ERROR.
