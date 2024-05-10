      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. pun.
       AUTHOR     Rémi.

      ****************************************************************** 
       DATA DIVISION.
      ****************************************************************** 
       WORKING-STORAGE SECTION.
OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME   PIC  X(30) VALUE 'dgse'.
       01  USERNAME PIC  X(30) VALUE 'cobol'.
       01  PASSWD   PIC  X(10) VALUE 'cbl85'.

       01  GROUP-AGE.
           03 AGE-MAX   PIC 9(10).
           03 AGE-MIN   PIC 9(10).
           03 AGE-COUNT PIC 9(10).
 
       01  SQL-PHRASE. 
           03 P-ID        PIC X(40).
           03 P-FIRSTNAME PIC X(50).
           03 P-PHRASE    PIC X(50).

       01  SQL-DATABANK.
           03 DBK-ID               PIC X(40).
           03 DBK-FIRSTNAME        PIC X(50).
           03 DBK-LASTNAME         PIC X(50).
           03 DBK-EMAIL            PIC X(50).
           03 DBK-GENDER           PIC X(50).
           03 DBK-AGE              PIC 9(10).
           03 DBK-SPOKEN           PIC X(50).
           03 DBK-COUNTRY          PIC X(50).
           03 DBK-COUNTRY-CODE     PIC X(50).
           03 DBK-INFO-MOBILEPHONE PIC X(50).
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
OCESQL     02  FILLER PIC X(029) VALUE "SELECT MAX(age) FROM databank".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(029) VALUE "SELECT MIN(age) FROM databank".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(062) VALUE "SELECT age, COUNT( * ) FROM da"
OCESQL  &  "tabank GROUP BY age ORDER BY age".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0005.
OCESQL     02  FILLER PIC X(159) VALUE "SELECT db.first_name, db.last_"
OCESQL  &  "name, db.email, ph.phrase FROM databank AS db JOIN phrase "
OCESQL  &  "AS ph ON db.country_code = ph.country_code WHERE db.countr"
OCESQL  &  "y = 'BELGIUM'".
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
           PERFORM START-PRINT THRU END-PRINT.

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
OCESQL*        SELECT MAX(age) INTO :AGE-MAX FROM databank
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE AGE-MAX
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0002
OCESQL          BY VALUE 0
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
OCESQL*    EXEC SQL
OCESQL*        SELECT MIN(age) INTO :AGE-MIN FROM databank
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE AGE-MIN
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL          BY VALUE 0
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
OCESQL*    EXEC SQL
OCESQL*        DECLARE CRSAGE CURSOR FOR
OCESQL*        SELECT age, COUNT(*)
OCESQL*        FROM databank
OCESQL*        GROUP BY age
OCESQL*        ORDER BY age
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "pun_CRSAGE" & x"00"
OCESQL          BY REFERENCE SQ0004
OCESQL     END-CALL.
OCESQL*    EXEC SQL
OCESQL*        DECLARE CRSBE CURSOR FOR
OCESQL*        SELECT db.first_name, db.last_name, db.email, ph.phrase
OCESQL*        FROM databank AS db
OCESQL*        JOIN phrase AS ph ON db.country_code = ph.country_code
OCESQL*        WHERE db.country = 'BELGIUM'
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "pun_CRSBE" & x"00"
OCESQL          BY REFERENCE SQ0005
OCESQL     END-CALL.
       END-SQL-REQUEST.
           EXIT. 

      ******************************************************************
       START-PRINT.
           DISPLAY "Age maximum :" SPACE AGE-MAX.
           DISPLAY "Age minimum :" SPACE AGE-MIN.
           DISPLAY SPACE.
           PERFORM START-N-INDIVIDU THRU END-N-INDIVIDU.
           PERFORM START-BE THRU END-BE.
       END-PRINT.
           EXIT.


      ******************************************************************
      *    Affiche le nombre d’individus par âge.                      *
      ******************************************************************
       START-N-INDIVIDU.
           DISPLAY "Age" SPACE "|" SPACE "Nombre d'individus".

OCESQL*    EXEC SQL  
OCESQL*        OPEN CRSAGE    
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "pun_CRSAGE" & x"00"
OCESQL     END-CALL.

           EVALUATE SQLCODE
           WHEN ZERO
              CONTINUE
           WHEN OTHER
              DISPLAY "ERROR OPENING CURSOR CRSAGE :" SPACE SQLCODE
           END-EVALUATE.

           PERFORM UNTIL SQLCODE = 100
OCESQL*        EXEC SQL
OCESQL*            FETCH CRSAGE
OCESQL*            INTO :DBK-AGE, :AGE-COUNT
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DBK-AGE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE AGE-COUNT
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "pun_CRSAGE" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

               EVALUATE SQLCODE
                   WHEN ZERO
                       DISPLAY DBK-AGE SPACE "|" SPACE AGE-COUNT
                   WHEN 100
                       DISPLAY "NO MORE ROWS IN CURSOR RESULT SET"
                   WHEN OTHER
                       DISPLAY "ERROR FETCHING CURSOR CRSAGE :"
                       SPACE SQLCODE
               END-EVALUATE
           END-PERFORM.


OCESQL*    EXEC SQL  
OCESQL*        CLOSE CRSAGE    
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "pun_CRSAGE" & x"00"
OCESQL     END-CALL
OCESQL    .

           EVALUATE SQLCODE
               WHEN ZERO
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERROR CLOSING CURSOR CRSAGE :"
                   SPACE SQLCODE
           END-EVALUATE.
       END-N-INDIVIDU.
           EXIT.

      ******************************************************************
      *    Affiche le le nom, prénom, email et citation pour           *
      *    les individus de Belgique.                                  *
      ******************************************************************
       START-BE.
           DISPLAY SPACE.
           DISPLAY "Prenom | Nom | Email | Phrase"
OCESQL*    EXEC SQL  
OCESQL*        OPEN CRSBE    
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "pun_CRSBE" & x"00"
OCESQL     END-CALL.

           EVALUATE SQLCODE
           WHEN ZERO
              CONTINUE
           WHEN OTHER
              DISPLAY "ERROR OPENING CURSOR CRSBE :" SPACE SQLCODE
           END-EVALUATE.

           PERFORM UNTIL SQLCODE = 100
OCESQL*        EXEC SQL
OCESQL*            FETCH CRSBE
OCESQL*            INTO :DBK-FIRSTNAME, :DBK-LASTNAME, :DBK-EMAIL, 
OCESQL*            :P-PHRASE
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DBK-FIRSTNAME
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DBK-LASTNAME
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DBK-EMAIL
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE P-PHRASE
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "pun_CRSBE" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

               EVALUATE SQLCODE
                   WHEN ZERO
                       DISPLAY FUNCTION TRIM(DBK-FIRSTNAME) 
                       SPACE "|" SPACE FUNCTION TRIM(DBK-LASTNAME)
                       SPACE "|" SPACE FUNCTION TRIM(DBK-EMAIL)
                       SPACE "|" SPACE FUNCTION TRIM(P-PHRASE)
                   WHEN 100
                       DISPLAY "NO MORE ROWS IN CURSOR RESULT SET"
                   WHEN OTHER
                       DISPLAY "ERROR FETCHING CURSOR CRSBE :" 
                       SPACE SQLCODE
               END-EVALUATE
           END-PERFORM.


OCESQL*    EXEC SQL  
OCESQL*        CLOSE CRSBE    
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "pun_CRSBE" & x"00"
OCESQL     END-CALL
OCESQL    .

           EVALUATE SQLCODE
               WHEN ZERO
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERROR CLOSING CURSOR CRSBE :"
                   SPACE SQLCODE
           END-EVALUATE.
       END-BE.
           EXIT.



