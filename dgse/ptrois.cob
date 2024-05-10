      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ptrois.
       AUTHOR        Rémi.

      ****************************************************************** 
       DATA DIVISION.
      ****************************************************************** 
       WORKING-STORAGE SECTION.

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                    PIC  X(30) VALUE 'dgse'.
       01  USERNAME                  PIC  X(30) VALUE 'cobol'.
       01  PASSWD                    PIC  X(10) VALUE 'cbl85'.

       01  GROUP-AGE.
           03  GAGE-MAX PIC 9(10).
           03  GAGE-MIN PIC 9(10).
           03  GAGE-MED PIC 9(10).

       01  GROUP-AVG-GENDER.
           03 GD-COUNTRY         PIC X(50).
           03 GD-TOTAL           PIC X(05).
           03 GD-AVG-AGENDER     PIC X(05).
           03 GD-AVG-BIGENDER    PIC X(05).
           03 GD-AVG-FEMALE      PIC X(05).
           03 GD-AVG-GENDERFLUID PIC X(05).
           03 GD-AVG-GENDERQUEER PIC X(05).
           03 GD-AVG-MALE        PIC X(05).
           03 GD-AVG-NONBINARY   PIC X(05).
           03 GD-AVG-POLYGENDER  PIC X(05).
 
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
OCESQL     02  FILLER PIC X(089) VALUE "SELECT MAX(age), MIN(age), PER"
OCESQL  &  "CENTILE_CONT(0.5) WITHIN GROUP (ORDER BY age) FROM databan"
OCESQL  &  "k".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(256) VALUE "SELECT country, count(gender) "
OCESQL  &  "AS total, ROUND((AVG(CASE WHEN gender = 'Agender' THEN 1 E"
OCESQL  &  "LSE 0 END) * 100), 2) AS Agender, ROUND((AVG(CASE WHEN gen"
OCESQL  &  "der = 'Bigender' THEN 1 ELSE 0 END) * 100), 2) AS Bigender"
OCESQL  &  ", ROUND((AVG(CASE WHEN gender = 'Female' THEN 1 ELSE".
OCESQL     02  FILLER PIC X(256) VALUE " 0 END) * 100), 2) AS Female, "
OCESQL  &  "ROUND((AVG(CASE WHEN gender = 'Genderfluid' THEN 1 ELSE 0 "
OCESQL  &  "END) * 100), 2) AS Genderfluid, ROUND((AVG(CASE WHEN gende"
OCESQL  &  "r = 'Genderqueer' THEN 1 ELSE 0 END) * 100), 2) AS Genderq"
OCESQL  &  "ueer, ROUND((AVG(CASE WHEN gender = 'Male' THEN 1 EL".
OCESQL     02  FILLER PIC X(255) VALUE "SE 0 END) * 100), 2) AS Male, "
OCESQL  &  "ROUND((AVG(CASE WHEN gender = 'Non-binary' THEN 1 ELSE 0 E"
OCESQL  &  "ND) * 100), 2) AS NonBinary, ROUND((AVG(CASE WHEN gender ="
OCESQL  &  " 'Polygender' THEN 1 ELSE 0 END) * 100), 2) AS Polygender "
OCESQL  &  "FROM databank GROUP BY country ORDER BY country ASC".
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
OCESQL*        SELECT MAX(age), MIN(age), 
OCESQL*        PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY age) 
OCESQL*        INTO :GAGE-MAX, :GAGE-MIN, :GAGE-MED 
OCESQL*        FROM databank
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GAGE-MAX
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GAGE-MIN
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GAGE-MED
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0002
OCESQL          BY VALUE 0
OCESQL          BY VALUE 3
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
OCESQL*    EXEC SQL
OCESQL*        DECLARE CRGENDER CURSOR FOR
OCESQL*        SELECT country, count(gender) AS total,
OCESQL*        ROUND((AVG(CASE WHEN gender = 'Agender' 
OCESQL*        THEN 1 ELSE 0 END) * 100),2) AS Agender,
OCESQL*        ROUND((AVG(CASE WHEN gender = 'Bigender' 
OCESQL*        THEN 1 ELSE 0 END) * 100),2) AS Bigender,
OCESQL*        ROUND((AVG(CASE WHEN gender = 'Female' 
OCESQL*        THEN 1 ELSE 0 END) * 100),2) AS Female,
OCESQL*        ROUND((AVG(CASE WHEN gender = 'Genderfluid' 
OCESQL*        THEN 1 ELSE 0 END) * 100),2) AS Genderfluid,
OCESQL*        ROUND((AVG(CASE WHEN gender = 'Genderqueer' 
OCESQL*        THEN 1 ELSE 0 END) * 100),2) AS Genderqueer,
OCESQL*        ROUND((AVG(CASE WHEN gender = 'Male' 
OCESQL*        THEN 1 ELSE 0 END) * 100),2) AS Male,
OCESQL*        ROUND((AVG(CASE WHEN gender = 'Non-binary' 
OCESQL*        THEN 1 ELSE 0 END) * 100),2) AS NonBinary,
OCESQL*        ROUND((AVG(CASE WHEN gender = 'Polygender' 
OCESQL*        THEN 1 ELSE 0 END) * 100),2) AS Polygender
OCESQL*        FROM databank
OCESQL*        GROUP BY country ORDER BY country ASC
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "ptrois_CRGENDER" & x"00"
OCESQL          BY REFERENCE SQ0003
OCESQL     END-CALL.
       END-SQL-REQUEST.
           EXIT. 

      ******************************************************************
       START-PRINT.
           DISPLAY "Age maximum :" SPACE GAGE-MAX.
           DISPLAY "Age minimum :" SPACE GAGE-MIN.
           DISPLAY "Age median :"  SPACE GAGE-MED.
           DISPLAY SPACE.
           PERFORM START-AVG-GENDER THRU END-AVG-GENDER.
       END-PRINT.
           EXIT.

      ******************************************************************
      *    Affiche le pourcentage de genre par pays.                   *
      ******************************************************************
       START-AVG-GENDER.
      *     DISPLAY "Age" SPACE "|" SPACE "Nombre d'individus".

OCESQL*    EXEC SQL  
OCESQL*        OPEN CRGENDER    
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "ptrois_CRGENDER" & x"00"
OCESQL     END-CALL.

           EVALUATE SQLCODE
           WHEN ZERO
              CONTINUE
           WHEN OTHER
              DISPLAY "ERROR OPENING CURSOR CRGENDER :" SPACE SQLCODE
           END-EVALUATE.

           PERFORM UNTIL SQLCODE = 100
OCESQL*        EXEC SQL
OCESQL*            FETCH CRGENDER
OCESQL*            INTO :GD-COUNTRY, :GD-TOTAL, 
OCESQL*            :GD-AVG-AGENDER, :GD-AVG-BIGENDER, 
OCESQL*            :GD-AVG-FEMALE, :GD-AVG-GENDERFLUID, 
OCESQL*            :GD-AVG-GENDERQUEER, :GD-AVG-MALE, 
OCESQL*            :GD-AVG-NONBINARY, :GD-AVG-POLYGENDER
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-COUNTRY
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 5
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-TOTAL
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 5
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-AGENDER
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 5
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-BIGENDER
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 5
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-FEMALE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 5
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-GENDERFLUID
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 5
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-GENDERQUEER
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 5
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-MALE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 5
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-NONBINARY
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 5
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-POLYGENDER
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "ptrois_CRGENDER" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

               EVALUATE SQLCODE
                   WHEN ZERO
                       DISPLAY GD-COUNTRY
                       SPACE "|" SPACE GD-TOTAL
                       SPACE "|" SPACE GD-AVG-AGENDER 
                       SPACE "|" SPACE GD-AVG-BIGENDER
                       SPACE "|" SPACE GD-AVG-FEMALE
                       SPACE "|" SPACE GD-AVG-GENDERFLUID
                       SPACE "|" SPACE GD-AVG-GENDERQUEER
                       SPACE "|" SPACE GD-AVG-MALE
                       SPACE "|" SPACE GD-AVG-NONBINARY
                       SPACE "|" SPACE GD-AVG-POLYGENDER
                   WHEN 100
                       DISPLAY "NO MORE ROWS IN CURSOR RESULT SET"
                   WHEN OTHER
                       DISPLAY "ERROR FETCHING CURSOR CRGENDER :"
                       SPACE SQLCODE
               END-EVALUATE
           END-PERFORM.


OCESQL*    EXEC SQL  
OCESQL*        CLOSE CRGENDER    
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "ptrois_CRGENDER" & x"00"
OCESQL     END-CALL
OCESQL    .

           EVALUATE SQLCODE
               WHEN ZERO
                   CONTINUE
               WHEN OTHER
                   DISPLAY "ERROR CLOSING CURSOR CRGENDER :"
                   SPACE SQLCODE
           END-EVALUATE.
       END-AVG-GENDER.
           EXIT.
           EXIT.
           EXIT.
