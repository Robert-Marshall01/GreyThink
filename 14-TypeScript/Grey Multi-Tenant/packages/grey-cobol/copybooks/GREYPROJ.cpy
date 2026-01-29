      ******************************************************************
      * GREYPROJ.cpy - Grey SDK Projects Copybook
      * Contains project data structures.
      ******************************************************************
       01  WS-PROJECT-REQUEST.
           05  WS-PROJ-ID-REQ          PIC X(64)  VALUE SPACES.
           05  WS-PROJ-NAME-REQ        PIC X(256) VALUE SPACES.
           05  WS-PROJ-DESC-REQ        PIC X(1024) VALUE SPACES.
           05  WS-PROJ-METADATA-REQ    PIC X(2048) VALUE SPACES.
      
       01  WS-PROJECT-LIST-REQUEST.
           05  WS-PROJ-PAGE            PIC 9(5)   VALUE 1.
           05  WS-PROJ-PER-PAGE        PIC 9(3)   VALUE 20.
           05  WS-PROJ-SORT-BY         PIC X(32)  VALUE "created_at".
           05  WS-PROJ-SORT-ORDER      PIC X(4)   VALUE "desc".
               88  WS-PROJ-SORT-ASC               VALUE "asc".
               88  WS-PROJ-SORT-DESC              VALUE "desc".
      
       01  WS-PROJECT-RESPONSE.
           05  WS-PROJ-ID              PIC X(64)  VALUE SPACES.
           05  WS-PROJ-NAME            PIC X(256) VALUE SPACES.
           05  WS-PROJ-DESCRIPTION     PIC X(1024) VALUE SPACES.
           05  WS-PROJ-METADATA        PIC X(2048) VALUE SPACES.
           05  WS-PROJ-CREATED-AT      PIC X(32)  VALUE SPACES.
           05  WS-PROJ-UPDATED-AT      PIC X(32)  VALUE SPACES.
           05  WS-PROJ-OWNER-ID        PIC X(64)  VALUE SPACES.
      
       01  WS-PROJECT-LIST-RESPONSE.
           05  WS-PROJ-TOTAL-COUNT     PIC 9(8)   VALUE 0.
           05  WS-PROJ-PAGE-COUNT      PIC 9(5)   VALUE 0.
           05  WS-PROJ-CURRENT-PAGE    PIC 9(5)   VALUE 0.
           05  WS-PROJECT-COUNT        PIC 9(3)   VALUE 0.
           05  WS-PROJECTS OCCURS 100 TIMES.
               10  WS-PROJ-ITEM-ID     PIC X(64).
               10  WS-PROJ-ITEM-NAME   PIC X(256).
               10  WS-PROJ-ITEM-DESC   PIC X(256).
               10  WS-PROJ-ITEM-CREATED PIC X(32).
