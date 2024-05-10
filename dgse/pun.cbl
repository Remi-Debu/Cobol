      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. pun.
       AUTHOR     Rémi.

      ****************************************************************** 
       DATA DIVISION.
      ****************************************************************** 
       WORKING-STORAGE SECTION.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                    PIC  X(30) VALUE 'dgse'.
       01  USERNAME                  PIC  X(30) VALUE 'cobol'.
       01  PASSWD                    PIC  X(10) VALUE 'cbl85'.

       01  GROUP-AGE.
           03  GAGE-MAX     PIC 9(10).
           03  GAGE-MIN     PIC 9(10).
           03  GAGE-GRP-AGE PIC 9(10).
           03  GAGE-NB-IND  PIC 9(10).

       01  GROUP-BE.
           03 GBE-FN   PIC X(50). 
           03 GBE-LN   PIC X(50). 
           03 GBE-MAIL PIC X(50). 
           03 GBE-PH   PIC X(50).
 
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
           PERFORM START-PRINT THRU END-PRINT.

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
               SELECT MAX(age) INTO :GAGE-MAX FROM databank
           END-EXEC.
           EXEC SQL
               SELECT MIN(age) INTO :GAGE-MIN FROM databank
           END-EXEC.
           EXEC SQL
               DECLARE CRSAGE CURSOR FOR
               SELECT age, COUNT(*)
               FROM databank
               GROUP BY age
               ORDER BY age
           END-EXEC.
           EXEC SQL
               DECLARE CRSBE CURSOR FOR
               SELECT db.first_name, db.last_name, db.email, ph.phrase
               FROM databank AS db
               JOIN phrase AS ph ON db.country_code = ph.country_code
               WHERE db.country = 'Belgium'
           END-EXEC.
       END-SQL-REQUEST.
           EXIT. 

      ******************************************************************
       START-PRINT.
           DISPLAY "Age maximum :" SPACE GAGE-MAX.
           DISPLAY "Age minimum :" SPACE GAGE-MIN.
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

           EXEC SQL  
               OPEN CRSAGE    
           END-EXEC.

           EVALUATE SQLCODE
           WHEN ZERO
              CONTINUE
           WHEN OTHER
              DISPLAY "ERROR OPENING CURSOR CRSAGE :" SPACE SQLCODE
           END-EVALUATE.

           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH CRSAGE
                   INTO :GAGE-GRP-AGE, :GAGE-NB-IND
               END-EXEC

               EVALUATE SQLCODE
                   WHEN ZERO
                       DISPLAY GAGE-GRP-AGE SPACE "|" SPACE GAGE-NB-IND
                   WHEN 100
                       DISPLAY "NO MORE ROWS IN CURSOR RESULT SET"
                   WHEN OTHER
                       DISPLAY "ERROR FETCHING CURSOR CRSAGE :"
                       SPACE SQLCODE
               END-EVALUATE
           END-PERFORM.


           EXEC SQL  
               CLOSE CRSAGE    
           END-EXEC.

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
           EXEC SQL  
               OPEN CRSBE    
           END-EXEC.

           EVALUATE SQLCODE
           WHEN ZERO
              CONTINUE
           WHEN OTHER
              DISPLAY "ERROR OPENING CURSOR CRSBE :" SPACE SQLCODE
           END-EVALUATE.

           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH CRSBE
                   INTO :GBE-FN, :GBE-LN, :GBE-MAIL, :GBE-PH
               END-EXEC

               EVALUATE SQLCODE
                   WHEN ZERO
                       DISPLAY FUNCTION TRIM(GBE-FN) 
                       SPACE "|" SPACE FUNCTION TRIM(GBE-FN)
                       SPACE "|" SPACE FUNCTION TRIM(GBE-MAIL)
                       SPACE "|" SPACE FUNCTION TRIM(GBE-PH)
                   WHEN 100
                       DISPLAY "NO MORE ROWS IN CURSOR RESULT SET"
                   WHEN OTHER
                       DISPLAY "ERROR FETCHING CURSOR CRSBE :" 
                       SPACE SQLCODE
               END-EVALUATE
           END-PERFORM.


           EXEC SQL  
               CLOSE CRSBE    
           END-EXEC.

           EVALUATE SQLCODE
               WHEN ZERO
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERROR CLOSING CURSOR CRSBE :"
                   SPACE SQLCODE
           END-EVALUATE.
       END-BE.
           EXIT.

