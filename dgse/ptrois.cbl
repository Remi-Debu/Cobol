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

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME   PIC  X(30) VALUE 'dgse'.
       01  USERNAME PIC  X(30) VALUE 'cobol'.
       01  PASSWD   PIC  X(10) VALUE 'cbl85'.
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
           PERFORM START-RAPPORT-HEADER THRU END-RAPPORT-HEADER.
           PERFORM START-AVG-GENDER THRU END-AVG-GENDER.
           PERFORM START-RAPPORT-FOOTER THRU END-RAPPORT-FOOTER.

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
               SELECT MAX(age), MIN(age), 
               PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY age) 
               INTO :AGE-MAX, :AGE-MIN, :AGE-MED 
               FROM databank
           END-EXEC.
           EXEC SQL
               DECLARE CRGENDER CURSOR FOR
               SELECT country, count(gender) AS total,
               ROUND((AVG(CASE WHEN gender = 'Agender' 
               THEN 1 ELSE 0 END) * 100),2) AS Agender,
               ROUND((AVG(CASE WHEN gender = 'Bigender' 
               THEN 1 ELSE 0 END) * 100),2) AS Bigender,
               ROUND((AVG(CASE WHEN gender = 'Female' 
               THEN 1 ELSE 0 END) * 100),2) AS Female,
               ROUND((AVG(CASE WHEN gender = 'Genderfluid' 
               THEN 1 ELSE 0 END) * 100),2) AS Genderfluid,
               ROUND((AVG(CASE WHEN gender = 'Genderqueer' 
               THEN 1 ELSE 0 END) * 100),2) AS Genderqueer,
               ROUND((AVG(CASE WHEN gender = 'Male' 
               THEN 1 ELSE 0 END) * 100),2) AS Male,
               ROUND((AVG(CASE WHEN gender = 'Non-binary' 
               THEN 1 ELSE 0 END) * 100),2) AS NonBinary,
               ROUND((AVG(CASE WHEN gender = 'Polygender' 
               THEN 1 ELSE 0 END) * 100),2) AS Polygender
               FROM databank
               GROUP BY country ORDER BY country ASC
           END-EXEC.
       END-SQL-REQUEST.
           EXIT. 

      ******************************************************************
      *    Affiche le pourcentage de genre par pays.                   *
      ******************************************************************
       START-AVG-GENDER.
           EXEC SQL  
               OPEN CRGENDER    
           END-EXEC.

           EVALUATE SQLCODE
           WHEN ZERO
              CONTINUE
           WHEN OTHER
              DISPLAY "ERROR OPENING CURSOR CRGENDER :" SPACE SQLCODE
           END-EVALUATE.

           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH CRGENDER
                   INTO :GD-COUNTRY, :GD-TOTAL, 
                   :GD-AVG-AGENDER, :GD-AVG-BIGENDER, 
                   :GD-AVG-FEMALE, :GD-AVG-GENDERFLUID, 
                   :GD-AVG-GENDERQUEER, :GD-AVG-MALE, 
                   :GD-AVG-NONBINARY, :GD-AVG-POLYGENDER
               END-EXEC

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


           EXEC SQL  
               CLOSE CRGENDER    
           END-EXEC.

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

