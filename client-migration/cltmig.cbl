      ******************************************************************
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cltmig.
       AUTHOR.       Rémi.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT ASSIGN TO 'input.dat'
           ACCESS MODE IS SEQUENTIAL
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-INPUT.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.
       01  REC-F-INPUT-2         PIC X(02).

       01  REC-STUDENT.
           03 R-S-KEY            PIC 9(02).       
           03 R-S-LASTNAME       PIC X(07).       
           03 R-S-FIRSTNAME      PIC X(06).       
           03 R-S-AGE            PIC 9(03).

       01  REC-COURSE.
           03 R-C-KEY            PIC 9(02).       
           03 R-C-LABEL          PIC X(21).       
           03 R-C-COEF           PIC X(03).       
           03 R-C-GRADE          PIC X(05).

       WORKING-STORAGE SECTION.
       01  FS-INPUT      PIC X(02) VALUE SPACE.
           88 FS-INPUT-OK  VALUE '00'.        
           88 FS-INPUT-EOF VALUE '10'.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME   PIC  X(30) VALUE 'school'.
       01  USERNAME PIC  X(30) VALUE 'cobol'.
       01  PASSWD   PIC  X(10) VALUE 'cbl85'.

       01  SQL-STUDENT.
           05  SQL-S-LASTNAME           PIC X(07).
           05  SQL-S-FIRSTNAME          PIC X(06).
           05  SQL-S-AGE                PIC 9(03).
       
       01  SQL-COURSE.
           05  SQL-C-LABEL              PIC X(35).
           05  SQL-C-COEF               PIC 9V9.
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       1000-MAIN-START.
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.

           IF SQLCODE NOT = ZERO 
               PERFORM 1001-ERROR-RTN-START
                  THRU 1001-ERROR-RTN-END
           END-IF.
           
           PERFORM 3001-SQL-TBL-CREATION-START
              THRU 3001-SQL-TBL-CREATION-END.
           
           PERFORM 7001-FILE-READ-START
              THRU 7001-FILE-READ-END.

       1000-MAIN-END.
           EXEC SQL COMMIT WORK END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.  
           STOP RUN. 

      ******************************************************************
       1001-ERROR-RTN-START.
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
       1001-ERROR-RTN-END.
           STOP RUN. 

      ******************************************************************
       3001-SQL-TBL-CREATION-START.
           EXEC SQL 
               DROP TABLE IF EXISTS STUDENT
           END-EXEC.
           EXEC SQL 
               CREATE TABLE STUDENT
               (
                   ID        SERIAL,
                   LASTNAME  CHAR(35) NOT NULL DEFAULT 'DUPOND',
                   FIRSTNAME CHAR(35) NOT NULL DEFAULT 'MonsieurMadame',
                   AGE       NUMERIC(3) NOT NULL DEFAULT 99,
                   CONSTRAINT STUDENT_ID_0 PRIMARY KEY (ID)
               )               
           END-EXEC.
           EXEC SQL 
               DROP TABLE IF EXISTS COURSE
           END-EXEC.
           EXEC SQL 
               CREATE TABLE COURSE
               (
                   ID        SERIAL,
                   LABEL     CHAR(35) NOT NULL DEFAULT 'Manquant',
                   COEF      NUMERIC(3,1) NOT NULL DEFAULT 1,
                   CONSTRAINT COURSE_ID_0 PRIMARY KEY (ID)
               )               
           END-EXEC.
       3001-SQL-TBL-CREATION-END.

      ******************************************************************
       7001-FILE-READ-START.
           OPEN INPUT F-INPUT.
           IF NOT FS-INPUT-OK
               DISPLAY 'ABORT POPULATING TABLE'
               GO TO 7001-FILE-READ-END
           END-IF.
           
           PERFORM UNTIL FS-INPUT-EOF
               READ F-INPUT
               EVALUATE REC-F-INPUT-2
                   WHEN '01'
                       PERFORM 7101-FILE-HANDLE-STUDENT-START
                           THRU 7101-FILE-HANDLE-STUDENT-END
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM.
       7001-FILE-READ-END.
           CLOSE F-INPUT.

      ******************************************************************
       7101-FILE-HANDLE-STUDENT-START.
           MOVE R-S-LASTNAME TO SQL-S-LASTNAME.
           MOVE R-S-FIRSTNAME TO SQL-S-FIRSTNAME.
           MOVE R-S-AGE TO SQL-S-AGE.
           DISPLAY SQL-S-AGE.

           EXEC SQL
               INSERT INTO STUDENT (LASTNAME,FIRSTNAME,AGE) 
               VALUES (
                   :SQL-S-LASTNAME, 
                   :SQL-S-FIRSTNAME,
                   :SQL-S-AGE
                   )
           END-EXEC.
       7101-FILE-HANDLE-STUDENT-END.
           EXIT.

       