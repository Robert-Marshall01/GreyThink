      ******************************************************************
      * GREYUSER.cpy - Grey SDK User Copybook
      * Contains user data structures.
      ******************************************************************
       01  WS-USER-REQUEST.
           05  WS-USER-ID-REQ          PIC X(64)  VALUE SPACES.
      
       01  WS-USER-RESPONSE.
           05  WS-USER-ID              PIC X(64)  VALUE SPACES.
           05  WS-USER-USERNAME        PIC X(256) VALUE SPACES.
           05  WS-USER-EMAIL           PIC X(256) VALUE SPACES.
           05  WS-USER-FIRST-NAME      PIC X(128) VALUE SPACES.
           05  WS-USER-LAST-NAME       PIC X(128) VALUE SPACES.
           05  WS-USER-CREATED-AT      PIC X(32)  VALUE SPACES.
           05  WS-USER-UPDATED-AT      PIC X(32)  VALUE SPACES.
           05  WS-USER-STATUS          PIC X(32)  VALUE SPACES.
               88  WS-USER-ACTIVE                 VALUE "active".
               88  WS-USER-INACTIVE               VALUE "inactive".
               88  WS-USER-PENDING                VALUE "pending".
