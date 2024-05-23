      ******************************************************************
      *    Le programme génére un rapport à partir des informations    *
      *    récupérées dans la DB "school".                             *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. sclrep.
       AUTHOR.       Rémi.

      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-OUTPUT ASSIGN TO 'output.dat'
           ACCESS MODE IS SEQUENTIAL
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-OUTPUT.

      ******************************************************************

       DATA DIVISION.
       FILE SECTION.
       FD  F-OUTPUT
           RECORD CONTAINS 1000 CHARACTERS 
           RECORDING MODE IS F.
       01  R-OUTPUT PIC X(1000).

       WORKING-STORAGE SECTION.
       01  FS-OUTPUT PIC X(02).
           88 FS-OUTPUT-OK  VALUE '00'.   

       01  TABLE-STUDENT.
           03  S-CNT  PIC 9(03).
           03  STUDENT OCCURS 200 TIMES
                          INDEXED BY S-IDX.
               05 S-LASTNAME       PIC X(10).
               05 S-FIRSTNAME      PIC X(10).
               05 S-AGE            PIC 9(02).
               05 S-AV-GRADE       PIC 9(02)V9(02).
       
       01  TABLE-COURSE.
           03 C-CNT  PIC 9(03).
           03 COURSE OCCURS 200 TIMES
                        INDEXED BY C-IDX.
               05 C-ID        PIC 9.
               05 C-ID-NAME   PIC X(02).
               05 C-LABEL     PIC X(35).
               05 C-COEF      PIC 9V9.
               05 C-AV-GRADE  PIC 99V99.

       01  TABLE-GRADE.
           03 G-CNT PIC 9(03).
           03 GRADE OCCURS 200 TIMES
                       INDEXED BY G-IDX.
               05 G-S-ID  PIC 9.
               05 G-C-ID  PIC 9.
               05 G-GRADE PIC 9(02)V9(02).  

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME              PIC  X(30) VALUE 'school'.
       01  USERNAME            PIC  X(30) VALUE 'cobol'.
       01  PASSWD              PIC  X(10) VALUE 'cbl85'.

       01  SQL-STUDENT.
           03 SQL-S-LASTNAME   PIC X(07).
           03 SQL-S-FIRSTNAME  PIC X(06).
           03 SQL-S-AGE        PIC 9(03).
           03 SQL-S-AV-GRADE   PIC X(05).
       
       01  SQL-COURSE.
           05 SQL-C-ID         PIC 9.
           05 SQL-C-LABEL      PIC X(35).
           05 SQL-C-COEF       PIC 99V9.
           05 SQL-C-AV-COURSE  PIC 99V99.

       01  SQL-GRADE.
           03 SQL-G-STUDENT-ID PIC 9.
           03 SQL-G-COURSE-ID  PIC 9.
           03 SQL-G-GRADE      PIC 99V99.
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
OCESQL     02  FILLER PIC X(250) VALUE "SELECT s.lastname, s.firstname"
OCESQL  &  ", s.age, ROUND(SUM(g.grade * c.coef) / SUM(c.coef), 2) AS "
OCESQL  &  "weighted_average FROM student s JOIN grade g ON s.id = g.s"
OCESQL  &  "tudent_id JOIN course c ON g.course_id = c.id GROUP BY s.i"
OCESQL  &  "d, s.lastname, s.firstname ORDER BY s.lastname".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(145) VALUE "SELECT c.id, c.label, c.coef, "
OCESQL  &  "ROUND(AVG(g.grade), 2) FROM grade g JOIN course c ON g.cou"
OCESQL  &  "rse_id = c.id GROUP BY c.id, c.label ORDER BY c.label ASC".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(046) VALUE "SELECT student_id, course_id, "
OCESQL  &  "grade FROM grade".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
       PROCEDURE DIVISION.
       0000-MAIN-START.
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
               PERFORM 1000-START-ERROR-RTN THRU END-1000-ERROR-RTN
           ELSE
               PERFORM 2000-START-SQL-REQUEST THRU END-2000-SQL-REQUEST
               PERFORM 3000-START-HANDLE THRU END-3000-HANDLE
           END-IF.

       END-0000-MAIN.
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
      *    Gestion des erreurs.                                        *
      ******************************************************************
       1000-START-ERROR-RTN.
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
       END-1000-ERROR-RTN.
           STOP RUN. 

      ******************************************************************
      ******************************************************************
       2000-START-SQL-REQUEST.
OCESQL*    EXEC SQL
OCESQL*        DECLARE CRSSTUDENT CURSOR FOR
OCESQL*        SELECT s.lastname, s.firstname, s.age,
OCESQL*        ROUND(SUM(g.grade * c.coef) / SUM(c.coef), 2) 
OCESQL*            AS weighted_average
OCESQL*        FROM student s
OCESQL*        JOIN grade g ON s.id = g.student_id
OCESQL*        JOIN course c ON g.course_id = c.id
OCESQL*        GROUP BY s.id, s.lastname, s.firstname
OCESQL*        ORDER BY s.lastname
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "sclrep_CRSSTUDENT" & x"00"
OCESQL          BY REFERENCE SQ0002
OCESQL     END-CALL.
OCESQL*    EXEC SQL 
OCESQL*        DECLARE CRSCOURSE CURSOR FOR
OCESQL*        SELECT c.id, c.label, c.coef, ROUND(AVG(g.grade), 2)
OCESQL*        FROM grade g
OCESQL*        JOIN course c ON g.course_id = c.id
OCESQL*        GROUP BY c.id, c.label
OCESQL*        ORDER BY c.label ASC
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "sclrep_CRSCOURSE" & x"00"
OCESQL          BY REFERENCE SQ0003
OCESQL     END-CALL.
OCESQL*    EXEC SQL
OCESQL*        DECLARE CRSGRADE CURSOR FOR
OCESQL*        SELECT student_id, course_id, grade FROM grade
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "sclrep_CRSGRADE" & x"00"
OCESQL          BY REFERENCE SQ0004
OCESQL     END-CALL.
       END-2000-SQL-REQUEST.
           EXIT.

      ******************************************************************
      ******************************************************************
       3000-START-HANDLE.
           PERFORM 3100-START-HANDLE-STUDENT 
              THRU END-3100-HANDLE-STUDENT.
           PERFORM 3200-START-HANDLE-COURSE
              THRU END-3200-HANDLE-COURSE.
           PERFORM 3300-START-HANDLE-GRADE
              THRU END-3300-HANDLE-GRADE.
       END-3000-HANDLE.
           EXIT.

      ******************************************************************
      ******************************************************************
       3100-START-HANDLE-STUDENT.
OCESQL*    EXEC SQL 
OCESQL*        OPEN CRSSTUDENT
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "sclrep_CRSSTUDENT" & x"00"
OCESQL     END-CALL.

           PERFORM UNTIL SQLCODE = 100
OCESQL*        EXEC SQL 
OCESQL*            FETCH CRSSTUDENT
OCESQL*            INTO :SQL-S-LASTNAME, :SQL-S-FIRSTNAME, 
OCESQL*            :SQL-S-AGE, :SQL-S-AV-GRADE
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 7
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-S-LASTNAME
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 6
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-S-FIRSTNAME
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 3
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-S-AGE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 5
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-S-AV-GRADE
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "sclrep_CRSSTUDENT" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

               EVALUATE SQLCODE
                   WHEN ZERO
                       ADD 1 TO S-CNT
                       MOVE SQL-S-LASTNAME  TO S-LASTNAME(S-CNT)
                       MOVE SQL-S-FIRSTNAME TO S-FIRSTNAME(S-CNT)
                       MOVE SQL-S-AGE       TO S-AGE(S-CNT)
                       MOVE SQL-S-AV-GRADE  TO S-AV-GRADE(S-CNT)
                   WHEN 100
                       DISPLAY "NO MORE ROWS IN CURSOR RESULT SET"
                   WHEN OTHER
                       DISPLAY "ERROR FETCHING CURSOR CRSSTUDENT :" 
                       SPACE SQLCODE
               END-EVALUATE
           END-PERFORM.

OCESQL*    EXEC SQL 
OCESQL*        CLOSE CRSSTUDENT
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "sclrep_CRSSTUDENT" & x"00"
OCESQL     END-CALL
OCESQL    .
       END-3100-HANDLE-STUDENT.
           EXIT.

      ******************************************************************
      ******************************************************************
       3200-START-HANDLE-COURSE.
OCESQL*    EXEC SQL 
OCESQL*        OPEN CRSCOURSE
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "sclrep_CRSCOURSE" & x"00"
OCESQL     END-CALL.

           PERFORM UNTIL SQLCODE = 100
OCESQL*        EXEC SQL 
OCESQL*            FETCH CRSCOURSE
OCESQL*            INTO :SQL-C-ID, :SQL-C-LABEL, 
OCESQL*            :SQL-C-COEF, :SQL-C-AV-COURSE
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 1
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-C-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 35
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-C-LABEL
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 3
OCESQL          BY VALUE -1
OCESQL          BY REFERENCE SQL-C-COEF
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE -2
OCESQL          BY REFERENCE SQL-C-AV-COURSE
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "sclrep_CRSCOURSE" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

               EVALUATE SQLCODE
                   WHEN ZERO
                       ADD 1 TO C-CNT
                       MOVE SQL-C-ID        TO C-ID(C-CNT)
                       MOVE SQL-C-LABEL     TO C-LABEL(C-CNT)
                       MOVE SQL-C-COEF      TO C-COEF(C-CNT)
                       MOVE SQL-C-AV-COURSE TO C-AV-GRADE(C-CNT)

                       STRING "C" SQL-C-ID
                       DELIMITED BY SIZE 
                       INTO C-ID-NAME(C-CNT)

                       DISPLAY C-ID-NAME(C-CNT)
                   WHEN 100
                       DISPLAY "NO MORE ROWS IN CURSOR RESULT SET"
                   WHEN OTHER
                       DISPLAY "ERROR FETCHING CURSOR CRSCOURSE :" 
                       SPACE SQLCODE
               END-EVALUATE
           END-PERFORM.

OCESQL*    EXEC SQL 
OCESQL*        CLOSE CRSCOURSE
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "sclrep_CRSCOURSE" & x"00"
OCESQL     END-CALL
OCESQL    .
       END-3200-HANDLE-COURSE.
           EXIT.

      ******************************************************************
      ******************************************************************
       3300-START-HANDLE-GRADE.
OCESQL*    EXEC SQL 
OCESQL*        OPEN CRSGRADE
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "sclrep_CRSGRADE" & x"00"
OCESQL     END-CALL.

           PERFORM UNTIL SQLCODE = 100
OCESQL*        EXEC SQL 
OCESQL*            FETCH CRSGRADE
OCESQL*            INTO :SQL-G-STUDENT-ID :SQL-G-COURSE-ID, 
OCESQL*            :SQL-G-GRADE
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 1
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-G-STUDENT-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 1
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-G-COURSE-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE -2
OCESQL          BY REFERENCE SQL-G-GRADE
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "sclrep_CRSGRADE" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

               EVALUATE SQLCODE
                   WHEN ZERO
                       ADD 1 TO G-CNT
                       MOVE SQL-G-STUDENT-ID TO G-S-ID(G-CNT)
                       MOVE SQL-G-COURSE-ID  TO G-C-ID(G-CNT)
                       MOVE SQL-G-GRADE      TO G-GRADE(G-CNT)
                   WHEN 100
                       DISPLAY "NO MORE ROWS IN CURSOR RESULT SET"
                   WHEN OTHER
                       DISPLAY "ERROR FETCHING CURSOR CRSGRADE :" 
                       SPACE SQLCODE
               END-EVALUATE
           END-PERFORM.

OCESQL*    EXEC SQL 
OCESQL*        CLOSE CRSGRADE
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "sclrep_CRSGRADE" & x"00"
OCESQL     END-CALL
OCESQL    .
       END-3300-HANDLE-GRADE.
           EXIT.
           EXIT.
           EXIT.
