      ******************************************************************
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. myprog.
       AUTHOR.   YourName.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME   PIC  X(30) VALUE "dbname".
       01  USERNAME PIC  X(30) VALUE "username".
       01  PASSWD   PIC  X(10) VALUE "password".
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.


      ******************************************************************

       PROCEDURE DIVISION.
       0000-START-MAIN.
           EXEC SQL
              CONNECT :USERNAME IDENTIFIEDBY :PASSWD USING :DBNAME
           END-EXEC.
           DISPLAY "HELLO COBOL".
       END-0000-MAIN.
           EXEC SQL DISCONNECT ALL END-EXEC.
           GOBACK.
