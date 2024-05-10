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
       01  R-RAPPORT PIC X(100).

       WORKING-STORAGE SECTION.
       01  FS-RAPPORT PIC X(02).
           88 FS-RAPPOT-OK VALUE "00".

       01  PRINT.
           03 PNT-AST PIC X(50).
           03 PNT-BLANK PIC X().

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME   PIC  X(30) VALUE 'dgse'.
       01  USERNAME PIC  X(30) VALUE 'cobol'.
       01  PASSWD   PIC  X(10) VALUE 'cbl85'.

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
           03 P-ID                 PIC X(40).
           03 P-FIRSTNAME          PIC X(50).
           03 P-PHRASE             PIC X(50).

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
               SELECT MAX(age), MIN(age), 
               PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY age) 
               INTO :GAGE-MAX, :GAGE-MIN, :GAGE-MED 
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
                       PERFORM START-RAPPORT THRU END-RAPPORT
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

       START-RAPPORT.
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

           OPEN OUTPUT F-RAPPORT.

           

           CLOSE F-RAPPORT.
       END-RAPPORT.
