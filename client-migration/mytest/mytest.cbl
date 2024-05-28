      ******************************************************************
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. mytest.
       AUTHOR.   YourName.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AGE-MAX PIC X(10).

       EXEC SQL INCLUDE SQLCA END-EXEC.

      ******************************************************************

       PROCEDURE DIVISION.
       0000-START-MAIN.
           EXEC SQL
               SELECT MIN(age) INTO :AGE-MAX FROM student
           END-EXEC.

           DISPLAY "HELLO COBOL" AGE-MAX.
       END-0000-MAIN.
           EXEC SQL DISCONNECT ALL END-EXEC. 
           GOBACK.
