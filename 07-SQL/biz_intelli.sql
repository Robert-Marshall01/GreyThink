/*
  CITATION for instructions: https://www.youtube.com/watch?v=9ADd-_mM5Dw
  VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
  To install:
  1. Go to Visual Code.
  2. Download the "SQLTools" extension.
  3. Download another extension that starts with "SQLTools" following with the desired 
  RDBMS (Relational DataBase Management System) such as MySQL.
  4. Go to "Add New Connection" under the new cylinder DB button added to your left-hand bar.
  5. Select your desired RDBMS.
  6. Input your connection name, username, and database.
  7. Add an additional '0' to the given port number to make it '33060.'
  8. Change password to plaintext in settings and type in your password.
  9. Change authentication protocal to "xprotocol."
  10. Specify a connection time-out setting (e.x. 30).
  IMPORTANT: steps 11 - 14 are MySQL specific.
  ------------------------------------------------------------------------------------------------------
  11. Go onto https://dev.mysql.com/downloads/file/?id=544662 to download the dependencies.
  12. Follow the wizard.
  13. Go onto your PowerShell, and type:
  & "C:\Program Files\MySQL\MySQL Server 8.0\bin\mysql.exe" -u YOUR_USERNAME -p
  14. Once you have "mysql>" on the left as your input, type the following command:
  CREATE DATABASE your_database;
  ------------------------------------------------------------------------------------------------------
  15. Save connection.
  16. Click on the "plug icon" to connect to the database, and be in your database.
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*/
