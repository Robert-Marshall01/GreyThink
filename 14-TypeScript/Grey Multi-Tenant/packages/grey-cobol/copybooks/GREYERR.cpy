      ******************************************************************
      * GREYERR.cpy - Grey SDK Error Copybook
      * Contains normalized error structure for the Grey SDK.
      ******************************************************************
       01  WS-GREY-ERROR.
           05  WS-ERROR-CODE           PIC X(32)  VALUE SPACES.
               88  WS-ERR-UNAUTHORIZED            VALUE "UNAUTHORIZED".
               88  WS-ERR-FORBIDDEN               VALUE "FORBIDDEN".
               88  WS-ERR-NOT-FOUND               VALUE "NOT-FOUND".
               88  WS-ERR-VALIDATION              VALUE "VALIDATION-ERROR".
               88  WS-ERR-NETWORK                 VALUE "NETWORK-ERROR".
               88  WS-ERR-TIMEOUT                 VALUE "TIMEOUT".
               88  WS-ERR-SERVER                  VALUE "SERVER-ERROR".
               88  WS-ERR-UNKNOWN                 VALUE "UNKNOWN".
           05  WS-ERROR-MESSAGE        PIC X(256) VALUE SPACES.
           05  WS-ERROR-DETAILS        PIC X(1024) VALUE SPACES.
      
      ******************************************************************
      * Error code constants
      ******************************************************************
       01  WS-ERROR-CODES.
           05  EC-UNAUTHORIZED         PIC X(32) VALUE "UNAUTHORIZED".
           05  EC-FORBIDDEN            PIC X(32) VALUE "FORBIDDEN".
           05  EC-NOT-FOUND            PIC X(32) VALUE "NOT-FOUND".
           05  EC-VALIDATION-ERROR     PIC X(32) VALUE "VALIDATION-ERROR".
           05  EC-NETWORK-ERROR        PIC X(32) VALUE "NETWORK-ERROR".
           05  EC-TIMEOUT              PIC X(32) VALUE "TIMEOUT".
           05  EC-SERVER-ERROR         PIC X(32) VALUE "SERVER-ERROR".
           05  EC-UNKNOWN              PIC X(32) VALUE "UNKNOWN".
