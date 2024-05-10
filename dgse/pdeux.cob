      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. pdeux.
       AUTHOR       Rémi.

      ****************************************************************** 
       DATA DIVISION.
      ****************************************************************** 
       WORKING-STORAGE SECTION.

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                    PIC  X(30) VALUE 'dgse'.
       01  USERNAME                  PIC  X(30) VALUE 'cobol'.
       01  PASSWD                    PIC  X(10) VALUE 'cbl85'.
 
       01  SQL-PHRASE. 
           03 P-ID                   PIC X(40).
           03 P-FIRSTNAME            PIC X(50).
           03 P-PHRASE               PIC X(50).

       01  SQL-DATABANK.
           03 DBK-ID                 PIC X(40).
           03 DBK-FIRSTNAME          PIC X(50).
           03 DBK-LASTNAME           PIC X(50).
           03 DBK-EMAIL              PIC X(50).
           03 DBK-GENDER             PIC X(50).
           03 DBK-AGE                PIC 9(10).
           03 DBK-SPOKEN             PIC X(50).
           03 DBK-COUNTRY            PIC X(50).
           03 DBK-COUNTRY-CODE       PIC X(50).
           03 DBK-INFO-MOBILEPHONE   PIC X(50).
OCESQL*EXEC SQL END DECLARE SECTION END-EXEC.

OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".

      ****************************************************************** 
OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(067) VALUE "UPDATE databank SET country_co"
OCESQL  &  "de = 'BE' WHERE age > 35 AND age < 40".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(065) VALUE "UPDATE databank SET country = "
OCESQL  &  "'belgium' WHERE country_code = 'BE'".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(068) VALUE "UPDATE databank SET spoken = U"
OCESQL  &  "PPER(spoken), country = UPPER(country)".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
       PROCEDURE DIVISION.
      ****************************************************************** 
       MAIN-START.
OCESQL*    EXEC SQL
OCESQL*        CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLConnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE USERNAME
OCESQL          BY VALUE 30
OCESQL          BY REFERENCE PASSWD
OCESQL          BY VALUE 10
OCESQL          BY REFERENCE DBNAME
OCESQL          BY VALUE 30
OCESQL     END-CALL.

           IF SQLCODE NOT = ZERO 
               PERFORM ERROR-RTN-START THRU ERROR-RTN-END
           END-IF.
               
           PERFORM START-SQL-REQUEST THRU END-SQL-REQUEST.

       MAIN-END.
OCESQL*    EXEC SQL COMMIT WORK END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "COMMIT" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
OCESQL*    EXEC SQL DISCONNECT ALL END-EXEC.  
OCESQL     CALL "OCESQLDisconnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL     END-CALL.
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
OCESQL*          EXEC SQL
OCESQL*              ROLLBACK
OCESQL*          END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "ROLLBACK" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
              WHEN  OTHER
                 DISPLAY "Undefined error"
                 DISPLAY "ERRCODE:" SPACE SQLSTATE
                 DISPLAY SQLERRMC
           END-EVALUATE.
       ERROR-RTN-END.
           EXIT. 

      ******************************************************************
       START-SQL-REQUEST.
OCESQL*    EXEC SQL
OCESQL*        UPDATE databank
OCESQL*        SET country_code = 'BE'
OCESQL*        WHERE age > 35 AND age < 40
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0002
OCESQL     END-CALL.
OCESQL*    EXEC SQL
OCESQL*        UPDATE databank
OCESQL*        SET country = 'belgium'
OCESQL*        WHERE country_code = 'BE'
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL     END-CALL.
OCESQL*    EXEC SQL
OCESQL*        UPDATE databank
OCESQL*        SET spoken = UPPER(spoken),
OCESQL*        country = UPPER(country)
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0004
OCESQL     END-CALL.
       END-SQL-REQUEST.
           EXIT.




