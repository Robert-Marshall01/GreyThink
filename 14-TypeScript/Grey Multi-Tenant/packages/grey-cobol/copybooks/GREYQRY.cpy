      ******************************************************************
      * GREYQRY.cpy - Grey SDK Query/Mutation Copybook
      * Contains query and mutation data structures.
      ******************************************************************
       01  WS-QUERY-REQUEST.
           05  WS-QRY-STRING           PIC X(8000) VALUE SPACES.
           05  WS-QRY-VARIABLES        PIC X(4000) VALUE SPACES.
           05  WS-QRY-OPERATION-NAME   PIC X(256) VALUE SPACES.
      
       01  WS-QUERY-RESPONSE.
           05  WS-QRY-DATA             PIC X(32000) VALUE SPACES.
           05  WS-QRY-DATA-LEN         PIC 9(5)   VALUE 0.
           05  WS-QRY-ERRORS           PIC X(4000) VALUE SPACES.
           05  WS-QRY-HAS-ERRORS       PIC X      VALUE "N".
               88  WS-QRY-SUCCESS                 VALUE "N".
               88  WS-QRY-HAS-ERR                 VALUE "Y".
      
       01  WS-MUTATION-REQUEST.
           05  WS-MUT-STRING           PIC X(8000) VALUE SPACES.
           05  WS-MUT-VARIABLES        PIC X(4000) VALUE SPACES.
           05  WS-MUT-OPERATION-NAME   PIC X(256) VALUE SPACES.
      
       01  WS-MUTATION-RESPONSE.
           05  WS-MUT-DATA             PIC X(32000) VALUE SPACES.
           05  WS-MUT-DATA-LEN         PIC 9(5)   VALUE 0.
           05  WS-MUT-ERRORS           PIC X(4000) VALUE SPACES.
           05  WS-MUT-HAS-ERRORS       PIC X      VALUE "N".
               88  WS-MUT-SUCCESS                 VALUE "N".
               88  WS-MUT-HAS-ERR                 VALUE "Y".
      
       01  WS-BATCH-REQUEST.
           05  WS-BATCH-COUNT          PIC 9(3)   VALUE 0.
           05  WS-BATCH-ITEMS OCCURS 50 TIMES.
               10  WS-BATCH-STRING     PIC X(4000).
               10  WS-BATCH-VARS       PIC X(2000).
               10  WS-BATCH-OP-NAME    PIC X(128).
      
       01  WS-BATCH-RESPONSE.
           05  WS-BATCH-RESULT-COUNT   PIC 9(3)   VALUE 0.
           05  WS-BATCH-RESULTS OCCURS 50 TIMES.
               10  WS-BATCH-RES-DATA   PIC X(8000).
               10  WS-BATCH-RES-OK     PIC X      VALUE "N".
