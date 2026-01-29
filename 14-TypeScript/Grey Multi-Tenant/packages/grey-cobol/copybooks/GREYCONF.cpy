      ******************************************************************
      * GREYCONF.cpy - Grey SDK Configuration Copybook
      * Contains configuration options for the Grey SDK.
      ******************************************************************
       01  WS-GREY-CONFIG.
           05  WS-HOST                 PIC X(256) VALUE SPACES.
           05  WS-PORT                 PIC 9(5)   VALUE 8080.
           05  WS-USE-TLS              PIC X      VALUE "N".
               88  WS-TLS-ENABLED                 VALUE "Y".
               88  WS-TLS-DISABLED                VALUE "N".
           05  WS-TIMEOUT              PIC 9(3)   VALUE 30.
           05  WS-AUTH-TOKEN           PIC X(2048) VALUE SPACES.
           05  WS-BASE-URL             PIC X(512) VALUE SPACES.
           05  WS-CUSTOM-HEADERS.
               10  WS-HEADER-COUNT     PIC 9(2)   VALUE 0.
               10  WS-HEADERS OCCURS 10 TIMES.
                   15  WS-HEADER-NAME  PIC X(64).
                   15  WS-HEADER-VALUE PIC X(256).
      
      ******************************************************************
      * Configuration helper flags
      ******************************************************************
       01  WS-CONFIG-FLAGS.
           05  WS-CONFIG-INITIALIZED   PIC X      VALUE "N".
               88  WS-CONFIG-READY                VALUE "Y".
               88  WS-CONFIG-NOT-READY            VALUE "N".
