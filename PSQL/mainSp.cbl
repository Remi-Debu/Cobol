       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       AUTHOR      Remi.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'input.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT-2         PIC X(02).

       01  REC-STUDENT.
           03 R-S-KEY            PIC 9(02).       
           03 R-S-LASTNAME       PIC X(07).       
           03 R-S-FIRSTNAME      PIC X(06).       
           03 R-S-AGE            PIC 9(02).

       01  REC-COURSE.
           03 R-C-KEY            PIC 9(02).       
           03 R-C-LABEL          PIC X(21).       
           03 R-C-COEF           PIC X(03).       
           03 R-C-GRADE          PIC X(05).

       WORKING-STORAGE SECTION.
       01  F-INPUT-STATUS      PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       01  TABLE-STUDENT.
           03  S-CNT  PIC 9(03) VALUE 1.
           03  STUDENT OCCURS 200 TIMES INDEXED BY S-IDX.
               05 S-LASTNAME       PIC X(10).
               05 S-FIRSTNAME      PIC X(10).
               05 S-AGE            PIC 9(02).
               05 S-SUM-GRADE-COEF PIC 9(05)V9(02).
               05 S-AV-GRADE       PIC 9(02)V9(02).
       
       01  TABLE-COURSE.
           03 C-CNT  PIC 9(03) VALUE 1.
           03 COURSE OCCURS 200 TIMES INDEXED BY C-IDX.
               05 C-ID        PIC X(10).
               05 C-ID-NAME   PIC X(04).
               05 C-LABEL     PIC X(21).
               05 C-COEF      PIC 9V9.
               05 C-SUM-GRADE PIC 9(05)V9(02).
               05 C-AV-GRADE  PIC 9(02)V9(02).

       01  TABLE-GRADE.
           03 G-CNT PIC 9(03) VALUE 1.
           03 GRADE OCCURS 200 TIMES INDEXED BY G-IDX.
               05 G-S-ID       PIC 9(05).
               05 G-C-ID       PIC 9(05).
               05 G-C-LABEL    PIC X(25).
               05 G-COEF       PIC 9V9.
               05 G-GRADE      PIC 9(02)V9(02).

       01  WS-IS-EXIST      PIC X.
           88 WS-IS-EXIST-Y VALUE "Y".
           88 WS-IS-EXIST-N VALUE "N".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                  PIC  X(30) VALUE 'school'.
       01  USERNAME                PIC  X(30) VALUE 'cobol'.
       01  PASSWD                  PIC  X(10) VALUE SPACE.

       01  SQL-STUDENT.
           05  SQL-S-LASTNAME           PIC X(35).
           05  SQL-S-FIRSTNAME          PIC X(35).
           05  SQL-S-AGE                PIC 9(03).
       
       01  SQL-COURSE.
           05  SQL-C-LABEL              PIC X(50).
           05  SQL-C-COEF               PIC 9V9.

       01  SQL-GRADE.
           05 SQL-G-S-ID PIC 9(05).
           05 SQL-G-C-ID PIC 9(05).
           05 SQL-G-LABEL PIC X(50).
           05 SQL-G-COEF PIC 9V9.
           05 SQL-G-GRADE PIC 99V99.

       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.
       1000-MAIN-START.
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.

           IF  SQLCODE NOT = ZERO 
               PERFORM 1001-ERROR-RTN-START
                   THRU 1001-ERROR-RTN-END
           END-IF.
           
           PERFORM START-R-IP THRU END-R-IP.

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
              *> TO RESTART TRANSACTION, DO ROLLBACK.
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
       START-R-IP.
           OPEN INPUT F-INPUT.
           IF F-INPUT-STATUS EQUAL "00"
              SET F-INPUT-STATUS-OK TO TRUE

              PERFORM UNTIL F-INPUT-STATUS-EOF
                 READ F-INPUT 
                 AT END 
                    SUBTRACT 1 FROM S-CNT
                    SUBTRACT 1 FROM C-CNT
                    SUBTRACT 1 FROM G-CNT
                    SET F-INPUT-STATUS-EOF TO TRUE
                 NOT AT END 
                    EVALUATE REC-F-INPUT-2
                    WHEN "01"
                       PERFORM START-HANDLE-STUDENT 
                          THRU END-HANDLE-STUDENT
                    WHEN "02"
                       PERFORM START-HANDLE-COURSE 
                          THRU END-HANDLE-COURSE
                       PERFORM START-HANDLE-GRADE 
                          THRU END-HANDLE-GRADE
                    WHEN OTHER
                       CONTINUE
                    END-EVALUATE
                  END-READ
              END-PERFORM
           ELSE
              DISPLAY "ERREUR :" SPACE F-INPUT-STATUS
           END-IF.
           CLOSE F-INPUT.
       END-R-IP.
           EXIT.

      ******************************************************************
       START-HANDLE-STUDENT.
           MOVE R-S-LASTNAME  TO S-LASTNAME(S-CNT).
           MOVE R-S-FIRSTNAME TO S-FIRSTNAME(S-CNT).
           MOVE R-S-AGE       TO S-AGE(S-CNT).

           ADD 1 TO S-CNT.


           MOVE R-S-LASTNAME  TO SQL-S-LASTNAME.
           MOVE R-S-FIRSTNAME TO SQL-S-FIRSTNAME.
           MOVE R-S-AGE       TO SQL-S-AGE.

           EXEC SQL
               INSERT INTO student (LASTNAME,FIRSTNAME,AGE) 
               VALUES (
                   :SQL-S-LASTNAME, 
                   :SQL-S-FIRSTNAME,
                   :SQL-S-AGE
                   )
           END-EXEC.
       END-HANDLE-STUDENT.
           EXIT.

      ******************************************************************
       START-HANDLE-COURSE.
           INITIALIZE WS-IS-EXIST.
           
           SET C-IDX TO 1.
           SEARCH COURSE VARYING C-IDX
               AT END
                   SET WS-IS-EXIST-N TO TRUE
               WHEN C-LABEL(C-IDX) EQUAL R-C-LABEL
                   GO TO END-HANDLE-COURSE
           END-SEARCH.
           
           IF WS-IS-EXIST-N
               MOVE R-C-LABEL TO C-LABEL(C-CNT)
               MOVE R-C-COEF  TO C-COEF(C-CNT)

               MOVE R-C-LABEL TO SQL-C-LABEL
               MOVE R-C-COEF  TO SQL-C-COEF

               EXEC SQL
               INSERT INTO COURSE (LABEL,COEF) 
               VALUES (
                   :SQL-C-LABEL, 
                   :SQL-C-COEF
                   )
               END-EXEC

               ADD 1 TO C-CNT
           END-IF.
       END-HANDLE-COURSE.
           EXIT.

      ******************************************************************
       START-HANDLE-GRADE.
           SUBTRACT 1 FROM S-CNT.
           SUBTRACT 1 FROM C-CNT.

           MOVE S-CNT TO G-S-ID(G-CNT).
           MOVE C-CNT TO G-C-ID(G-CNT).

           MOVE S-CNT TO SQL-G-S-ID.
           MOVE C-CNT TO SQL-G-C-ID.
           
           DISPLAY S-CNT.
           DISPLAY C-CNT.
           ADD 1 TO S-CNT.
           ADD 1 TO C-CNT.

           MOVE R-C-LABEL TO G-C-LABEL(G-CNT).
           MOVE R-C-GRADE TO G-GRADE(G-CNT).
           MOVE R-C-COEF  TO G-COEF(G-CNT).

           MOVE R-C-LABEL TO SQL-G-LABEL.
           MOVE R-C-COEF  TO SQL-G-COEF.
           MOVE R-C-GRADE TO SQL-G-GRADE.

           DISPLAY R-C-GRADE.

           EXEC SQL
               INSERT INTO GRADE 
               (GRADE_STUDENT_ID,GRADE_COURSE_IDLABEL,COEF,GRADE) 
               VALUES (
                   :SQL-G-S-ID, 
                   :SQL-G-C-ID,
                   :SQL-G-LABEL,
                   :SQL-G-COEF,
                   :SQL-G-GRADE
                   )
           END-EXEC

           ADD 1 TO G-CNT.
       END-HANDLE-GRADE.
           EXIT.
       