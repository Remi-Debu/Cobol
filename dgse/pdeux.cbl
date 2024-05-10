      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. pdeux.
       AUTHOR       Rémi.

      ****************************************************************** 
       DATA DIVISION.
      ****************************************************************** 
       WORKING-STORAGE SECTION.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                    PIC  X(30) VALUE 'dgse'.
       01  USERNAME                  PIC  X(30) VALUE 'cobol'.
       01  PASSWD                    PIC  X(10) VALUE 'cbl85'.
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

      ****************************************************************** 
       PROCEDURE DIVISION.
      ****************************************************************** 
       MAIN-START.
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.

           IF SQLCODE NOT = ZERO 
               PERFORM ERROR-RTN-START THRU ERROR-RTN-END
           END-IF.
               
           PERFORM START-SQL-REQUEST THRU END-SQL-REQUEST.

       MAIN-END.
           EXEC SQL COMMIT WORK END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.  
           STOP RUN. 

      ******************************************************************
       ERROR-RTN-START.
           DISPLAY "*** SQL ERROR ***".
           DISPLAY "SQLCODE: " SQLCODE SPACE.
           EVALUATE SQLCODE
              WHEN  +100
                 DISPLAY "Record not found"
              WHEN  -01
                 DISPLAY "Connection failed"
              WHEN  -20
                 DISPLAY "Internal error"
              WHEN  -30
                 DISPLAY "PostgreSQL error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
                 EXEC SQL
                     ROLLBACK
                 END-EXEC
              WHEN  OTHER
                 DISPLAY "Undefined error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
           END-EVALUATE.
       ERROR-RTN-END.
           EXIT. 

      ******************************************************************
       START-SQL-REQUEST.
           EXEC SQL
               UPDATE databank
               SET country_code = 'BE'
               WHERE age > 35 AND age < 40
           END-EXEC.
           EXEC SQL
               UPDATE databank
               SET country = 'belgium'
               WHERE country_code = 'BE'
           END-EXEC.
           EXEC SQL
               UPDATE databank
               SET spoken = UPPER(spoken),
               country = UPPER(country)
           END-EXEC.
       END-SQL-REQUEST.
           EXIT.


