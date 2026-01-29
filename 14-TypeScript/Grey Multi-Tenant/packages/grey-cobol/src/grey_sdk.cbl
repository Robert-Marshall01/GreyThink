      ******************************************************************
      * grey_sdk.cbl - Grey SDK Main Entry Point
      * Provides high-level SDK initialization and configuration.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GREY-SDK.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY GREYCONF.
       COPY GREYERR.
       COPY GREYRES.
       
       01  WS-INITIALIZED              PIC X      VALUE "N".
           88  WS-SDK-READY                       VALUE "Y".
           88  WS-SDK-NOT-READY                   VALUE "N".
       
       LINKAGE SECTION.
       01  LS-CONFIG.
           05  LS-HOST                 PIC X(256).
           05  LS-PORT                 PIC 9(5).
           05  LS-USE-TLS              PIC X.
           05  LS-TIMEOUT              PIC 9(3).
           05  LS-AUTH-TOKEN           PIC X(2048).
           05  LS-BASE-URL             PIC X(512).
           05  LS-CUSTOM-HEADERS.
               10  LS-HEADER-COUNT     PIC 9(2).
               10  LS-HEADERS OCCURS 10 TIMES.
                   15  LS-HEADER-NAME  PIC X(64).
                   15  LS-HEADER-VALUE PIC X(256).
       
       01  LS-HOST-IN                  PIC X(256).
       01  LS-PORT-IN                  PIC 9(5).
       
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
      * SDK-INIT-LOCAL: Initialize SDK for local development
      * Input:  LS-PORT-IN (optional, defaults to 8080)
      * Output: LS-CONFIG
      ******************************************************************
       ENTRY "SDK-INIT-LOCAL" USING LS-PORT-IN LS-CONFIG.
           PERFORM INIT-CONFIG
           
           MOVE "localhost" TO LS-HOST
           
           IF LS-PORT-IN > 0
               MOVE LS-PORT-IN TO LS-PORT
           ELSE
               MOVE 8080 TO LS-PORT
           END-IF
           
           MOVE "N" TO LS-USE-TLS
           MOVE 30 TO LS-TIMEOUT
           
           PERFORM BUILD-BASE-URL
           MOVE "Y" TO WS-INITIALIZED
           
           GOBACK.
      
      ******************************************************************
      * SDK-INIT-PRODUCTION: Initialize SDK for production
      * Input:  LS-HOST-IN, LS-PORT-IN (optional, defaults to 443)
      * Output: LS-CONFIG
      ******************************************************************
       ENTRY "SDK-INIT-PRODUCTION" USING LS-HOST-IN LS-PORT-IN
                                         LS-CONFIG.
           PERFORM INIT-CONFIG
           
           MOVE LS-HOST-IN TO LS-HOST
           
           IF LS-PORT-IN > 0
               MOVE LS-PORT-IN TO LS-PORT
           ELSE
               MOVE 443 TO LS-PORT
           END-IF
           
           MOVE "Y" TO LS-USE-TLS
           MOVE 30 TO LS-TIMEOUT
           
           PERFORM BUILD-BASE-URL
           MOVE "Y" TO WS-INITIALIZED
           
           GOBACK.
      
      ******************************************************************
      * SDK-SET-TIMEOUT: Set request timeout
      * Input:  LS-CONFIG, timeout value in LS-TIMEOUT
      ******************************************************************
       ENTRY "SDK-SET-TIMEOUT" USING LS-CONFIG.
      *    Timeout is already in LS-CONFIG, just validate
           IF LS-TIMEOUT < 1
               MOVE 30 TO LS-TIMEOUT
           END-IF
           GOBACK.
      
      ******************************************************************
      * SDK-SET-AUTH-TOKEN: Set authentication token
      * Input:  LS-CONFIG with LS-AUTH-TOKEN set
      ******************************************************************
       ENTRY "SDK-SET-AUTH-TOKEN" USING LS-CONFIG.
      *    Token is already in LS-CONFIG
           GOBACK.
      
      ******************************************************************
      * SDK-CLEAR-AUTH-TOKEN: Clear authentication token
      * Output: LS-CONFIG with LS-AUTH-TOKEN cleared
      ******************************************************************
       ENTRY "SDK-CLEAR-AUTH-TOKEN" USING LS-CONFIG.
           MOVE SPACES TO LS-AUTH-TOKEN
           GOBACK.
      
      ******************************************************************
      * SDK-ADD-HEADER: Add custom header
      * Input:  LS-CONFIG, header name/value in next available slot
      ******************************************************************
       ENTRY "SDK-ADD-HEADER" USING LS-CONFIG.
      *    Header should be added by caller incrementing LS-HEADER-COUNT
           GOBACK.
      
      ******************************************************************
      * SDK-GET-BASE-URL: Get the base URL
      * Output: LS-CONFIG with LS-BASE-URL populated
      ******************************************************************
       ENTRY "SDK-GET-BASE-URL" USING LS-CONFIG.
           PERFORM BUILD-BASE-URL
           GOBACK.
      
      ******************************************************************
      * SDK-IS-INITIALIZED: Check if SDK is initialized
      * Output: LS-RESULT-OK = "Y" if initialized
      ******************************************************************
       ENTRY "SDK-IS-INITIALIZED" USING LS-RESULT.
           IF WS-SDK-READY
               MOVE "Y" TO LS-RESULT-OK
           ELSE
               MOVE "N" TO LS-RESULT-OK
           END-IF
           GOBACK.
      
      ******************************************************************
      * Internal: Initialize config structure
      ******************************************************************
       INIT-CONFIG.
           MOVE SPACES TO LS-HOST
           MOVE 0 TO LS-PORT
           MOVE "N" TO LS-USE-TLS
           MOVE 30 TO LS-TIMEOUT
           MOVE SPACES TO LS-AUTH-TOKEN
           MOVE SPACES TO LS-BASE-URL
           MOVE 0 TO LS-HEADER-COUNT.
      
      ******************************************************************
      * Internal: Build base URL from config
      ******************************************************************
       BUILD-BASE-URL.
           MOVE SPACES TO LS-BASE-URL
           
           IF LS-USE-TLS = "Y"
               STRING "https://" DELIMITED BY SIZE
                      LS-HOST DELIMITED BY SPACE
                      ":" DELIMITED BY SIZE
                      LS-PORT DELIMITED BY SIZE
                      INTO LS-BASE-URL
           ELSE
               STRING "http://" DELIMITED BY SIZE
                      LS-HOST DELIMITED BY SPACE
                      ":" DELIMITED BY SIZE
                      LS-PORT DELIMITED BY SIZE
                      INTO LS-BASE-URL
           END-IF.
       
       END PROGRAM GREY-SDK.
