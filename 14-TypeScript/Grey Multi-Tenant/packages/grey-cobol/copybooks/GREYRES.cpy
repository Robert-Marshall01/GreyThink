      ******************************************************************
      * GREYRES.cpy - Grey SDK Result Copybook
      * Contains result structure for SDK operations.
      ******************************************************************
       01  WS-GREY-RESULT.
           05  WS-RESULT-OK            PIC X      VALUE "N".
               88  WS-RESULT-SUCCESS              VALUE "Y".
               88  WS-RESULT-FAILURE              VALUE "N".
           05  WS-RESULT-DATA          PIC X(32000) VALUE SPACES.
           05  WS-RESULT-DATA-LEN      PIC 9(5)   VALUE 0.
           05  WS-RESULT-ERROR.
               10  WS-RES-ERR-CODE     PIC X(32)  VALUE SPACES.
               10  WS-RES-ERR-MSG      PIC X(256) VALUE SPACES.
               10  WS-RES-ERR-DETAILS  PIC X(1024) VALUE SPACES.
      
      ******************************************************************
      * HTTP Response fields
      ******************************************************************
       01  WS-HTTP-RESPONSE.
           05  WS-HTTP-STATUS          PIC 9(3)   VALUE 0.
           05  WS-HTTP-BODY            PIC X(32000) VALUE SPACES.
           05  WS-HTTP-BODY-LEN        PIC 9(5)   VALUE 0.
           05  WS-HTTP-CONTENT-TYPE    PIC X(128) VALUE SPACES.
