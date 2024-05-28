      ******************************************************************
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. mytest.
       AUTHOR.   YourName.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AGE-MAX PIC X(10).

OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".

      ******************************************************************

OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(028) VALUE "SELECT MIN(age) FROM student".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
       PROCEDURE DIVISION.
       0000-START-MAIN.
OCESQL*    EXEC SQL
OCESQL*        SELECT MIN(age) INTO :AGE-MAX FROM student
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE AGE-MAX
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0001
OCESQL          BY VALUE 0
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.

           DISPLAY "HELLO COBOL" AGE-MAX.
       END-0000-MAIN.
OCESQL*    EXEC SQL DISCONNECT ALL END-EXEC. 
OCESQL     CALL "OCESQLDisconnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL     END-CALL.
           GOBACK.
           GOBACK.
           GOBACK.
