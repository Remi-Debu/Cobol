      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ptrois.
       AUTHOR        Rémi.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-RAPPORT ASSIGN TO "rapport.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-RAPPORT.

      ****************************************************************** 
       DATA DIVISION.
      ****************************************************************** 
       FILE SECTION.
       FD  F-RAPPORT
           RECORD CONTAINS 100 CHARACTERS
           RECORDING MODE IS F.
       01  R-RAPPORT PIC X(200).

       WORKING-STORAGE SECTION.
       01  FS-RAPPORT PIC X(02).
           88 FS-RAPPOT-OK VALUE "00".

       01  PRINT.
           03 PNT-AST   PIC X(134) VALUE ALL "*".
           03 PNT-BLANK PIC X(51).
           03 PNT-TITLE PIC X(31) 
           VALUE "PROPORTIONS DES GENRES PAR PAYS".
           03 PNT-NUM   PIC Z(09)9.

       01  GROUP-AGE.
           03 AGE-MAX PIC 9(10).
           03 AGE-MIN PIC 9(10).
           03 AGE-MED PIC 9(10).

       01  GROUP-AVG-GENDER.
           03 GD-COUNTRY         PIC X(20) VALUE "------COUNTRY-------".
           03 FILLER             PIC X(02) VALUE "||".
           03 GD-TOTAL           PIC X(07) VALUE "-TOTAL-".
           03 FILLER             PIC X(02) VALUE "||".
           03 GD-AVG-AGENDER     PIC X(09) VALUE "-AGENDER-".
           03 FILLER             PIC X(02) VALUE "||".
           03 GD-AVG-BIGENDER    PIC X(10) VALUE "-BIGENDER-".
           03 FILLER             PIC X(02) VALUE "||".
           03 GD-AVG-FEMALE      PIC X(08) VALUE "-FEMALE-".
           03 FILLER             PIC X(02) VALUE "||".
           03 GD-AVG-GENDERFLUID PIC X(14) VALUE "-GENDER-FLUID-".
           03 FILLER             PIC X(02) VALUE "||".
           03 GD-AVG-GENDERQUEER PIC X(14) VALUE "-GENDER-QUEER-".
           03 FILLER             PIC X(02) VALUE "||".
           03 GD-AVG-MALE        PIC X(07) VALUE "-MALE--".
           03 FILLER             PIC X(02) VALUE "||".
           03 GD-AVG-NONBINARY   PIC X(12) VALUE "-NON-BINARY-".
           03 FILLER             PIC X(02) VALUE "||".
           03 GD-AVG-POLYGENDER  PIC X(13) VALUE "-POLY-GENDER-".
           03 FILLER             PIC X(02) VALUE "||".

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME   PIC  X(30) VALUE 'dgse'.
       01  USERNAME PIC  X(30) VALUE 'cobol'.
       01  PASSWD   PIC  X(10) VALUE 'cbl85'.
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
           PERFORM START-RAPPORT-HEADER THRU END-RAPPORT-HEADER.
           PERFORM START-AVG-GENDER THRU END-AVG-GENDER.
           PERFORM START-RAPPORT-FOOTER THRU END-RAPPORT-FOOTER.

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
OCESQL*        INTO :AGE-MAX, :AGE-MIN, :AGE-MED 
OCESQL*        FROM databank
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE AGE-MAX
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE AGE-MIN
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE AGE-MED
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
      *    Affiche le pourcentage de genre par pays.                   *
      ******************************************************************
       START-AVG-GENDER.
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
OCESQL          BY VALUE 20
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-COUNTRY
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 7
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-TOTAL
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 9
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-AGENDER
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-BIGENDER
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 8
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-FEMALE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 14
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-GENDERFLUID
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 14
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-GENDERQUEER
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 7
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-MALE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 12
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE GD-AVG-NONBINARY
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 13
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
                       PERFORM START-RAPPORT-BODY THRU END-RAPPORT-BODY
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

      ******************************************************************
       START-RAPPORT-HEADER.
           OPEN OUTPUT F-RAPPORT.
           WRITE R-RAPPORT FROM PNT-AST.

           INITIALIZE R-RAPPORT.
           STRING PNT-BLANK PNT-TITLE DELIMITED BY SIZE INTO R-RAPPORT.
           WRITE R-RAPPORT.

           WRITE R-RAPPORT FROM PNT-AST.
           WRITE R-RAPPORT FROM PNT-BLANK.

           WRITE R-RAPPORT FROM GROUP-AVG-GENDER.

           CLOSE F-RAPPORT.
       END-RAPPORT-HEADER.
           EXIT.

      ****************************************************************** 
       START-RAPPORT-BODY.
           INITIALIZE R-RAPPORT.
           OPEN EXTEND F-RAPPORT.
           MOVE GROUP-AVG-GENDER TO R-RAPPORT.
           WRITE R-RAPPORT.
           CLOSE F-RAPPORT.
       END-RAPPORT-BODY.
           EXIT.

      ******************************************************************
       START-RAPPORT-FOOTER.
           OPEN EXTEND F-RAPPORT.
           WRITE R-RAPPORT FROM PNT-BLANK.
           WRITE R-RAPPORT FROM PNT-AST.

           INITIALIZE R-RAPPORT.
           INITIALIZE PNT-NUM.
           MOVE AGE-MAX TO PNT-NUM.
           STRING "Age maximum :" SPACE FUNCTION TRIM(PNT-NUM) 
           DELIMITED BY SIZE
           INTO R-RAPPORT.
           WRITE R-RAPPORT.

           INITIALIZE R-RAPPORT.
           INITIALIZE PNT-NUM.
           MOVE AGE-MIN TO PNT-NUM.
           STRING "Age minimum :" SPACE FUNCTION TRIM(PNT-NUM) 
           DELIMITED BY SIZE
           INTO R-RAPPORT.
           WRITE R-RAPPORT.

           INITIALIZE R-RAPPORT.
           INITIALIZE PNT-NUM.
           MOVE AGE-MED TO PNT-NUM.
           STRING "Age median  :"  SPACE FUNCTION TRIM(PNT-NUM) 
           DELIMITED BY SIZE
           INTO R-RAPPORT.
           WRITE R-RAPPORT.

           WRITE R-RAPPORT FROM PNT-AST.
           CLOSE F-RAPPORT.
       END-RAPPORT-FOOTER.
           EXIT.



