      ******************************************************************
      * GREYAUTH.cpy - Grey SDK Auth Copybook
      * Contains authentication request/response structures.
      ******************************************************************
       01  WS-AUTH-REQUEST.
           05  WS-AUTH-USERNAME        PIC X(256) VALUE SPACES.
           05  WS-AUTH-PASSWORD        PIC X(256) VALUE SPACES.
           05  WS-AUTH-REFRESH-TOKEN   PIC X(2048) VALUE SPACES.
      
       01  WS-AUTH-RESPONSE.
           05  WS-ACCESS-TOKEN         PIC X(2048) VALUE SPACES.
           05  WS-REFRESH-TOKEN        PIC X(2048) VALUE SPACES.
           05  WS-TOKEN-TYPE           PIC X(32)  VALUE SPACES.
           05  WS-EXPIRES-IN           PIC 9(8)   VALUE 0.
           05  WS-USER-ID              PIC X(64)  VALUE SPACES.
