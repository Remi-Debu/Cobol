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

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
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
OCESQL     02  FILLER PIC X(028) VALUE "DROP TABLE IF EXISTS STUDENT".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(212) VALUE "CREATE TABLE STUDENT ( ID SERI"
OCESQL  &  "AL, LASTNAME CHAR(35) NOT NULL DEFAULT 'DUPOND', FIRSTNAME"
OCESQL  &  " CHAR(35) NOT NULL DEFAULT 'MonsieurMadame', AGE SMALLINT("
OCESQL  &  "3) NOT NULL DEFAULT 99, CONSTRAINT STUDENT_ID_0 PRIMARY KE"
OCESQL  &  "Y (ID) )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(027) VALUE "DROP TABLE IF EXISTS COURSE".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0005.
OCESQL     02  FILLER PIC X(157) VALUE "CREATE TABLE COURSE ( ID SERIA"
OCESQL  &  "L, LABEL CHAR(35) NOT NULL DEFAULT 'Manquant', COEF NUMERI"
OCESQL  &  "C(3, 1) NOT NULL DEFAULT 1, CONSTRAINT COURSE_ID_0 PRIMARY"
OCESQL  &  " KEY (ID) )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0006.
OCESQL     02  FILLER PIC X(068) VALUE "INSERT INTO STUDENT (LASTNAME,"
OCESQL  &  " FIRSTNAME, AGE) VALUES ( $1, $2, $3 )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
       PROCEDURE DIVISION.
      ******************************************************************
       1000-MAIN-START.
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

           IF  SQLCODE NOT = ZERO 
               PERFORM 1001-ERROR-RTN-START
                   THRU 1001-ERROR-RTN-END
           END-IF.
           
           PERFORM 3001-SQL-TBL-CREATION-START
               THRU 3001-SQL-TBL-CREATION-END.
           
           PERFORM 7001-FILE-READ-START
               THRU 7001-FILE-READ-END.

       1000-MAIN-END.
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
       1001-ERROR-RTN-END.
           STOP RUN. 

      ******************************************************************
       3001-SQL-TBL-CREATION-START.
OCESQL*    EXEC SQL 
OCESQL*        DROP TABLE IF EXISTS STUDENT
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0002
OCESQL     END-CALL.
OCESQL*    EXEC SQL 
OCESQL*        CREATE TABLE STUDENT
OCESQL*        (
OCESQL*            ID        SERIAL,
OCESQL*            LASTNAME  CHAR(35) NOT NULL DEFAULT 'DUPOND',
OCESQL*            FIRSTNAME CHAR(35) NOT NULL DEFAULT 'MonsieurMadame',
OCESQL*            AGE       SMALLINT(3) NOT NULL DEFAULT 99,
OCESQL*            CONSTRAINT STUDENT_ID_0 PRIMARY KEY (ID)
OCESQL*        )               
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL     END-CALL.
OCESQL*    EXEC SQL 
OCESQL*        DROP TABLE IF EXISTS COURSE
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0004
OCESQL     END-CALL.
OCESQL*    EXEC SQL 
OCESQL*        CREATE TABLE COURSE
OCESQL*        (
OCESQL*            ID        SERIAL,
OCESQL*            LABEL     CHAR(35) NOT NULL DEFAULT 'Manquant',
OCESQL*            COEF      NUMERIC(3,1) NOT NULL DEFAULT 1,
OCESQL*            CONSTRAINT COURSE_ID_0 PRIMARY KEY (ID)
OCESQL*        )               
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0005
OCESQL     END-CALL.

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

OCESQL*    EXEC SQL
OCESQL*        INSERT INTO STUDENT (LASTNAME,FIRSTNAME,AGE) 
OCESQL*        VALUES (
OCESQL*            :SQL-S-LASTNAME, 
OCESQL*            :SQL-S-FIRSTNAME,
OCESQL*            :SQL-S-AGE
OCESQL*            )
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 7
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-S-LASTNAME
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 6
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-S-FIRSTNAME
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 3
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-S-AGE
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0006
OCESQL          BY VALUE 3
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
       7101-FILE-HANDLE-STUDENT-END.
           EXIT.

                            