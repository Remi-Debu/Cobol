      ******************************************************************
      *    Le programme insère dans la DB "school" les données        *
      *    provenant du fichier "input.dat".                           *
      *    Les données sont des informations sur des étudiants, leurs  *
      *    cours et leurs notes.                                       *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. scldb.
       AUTHOR.      Rémi.

      ******************************************************************

       ENVIRONMENT DIVISION.
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
       01  DBNAME              PIC  X(30) VALUE 'school'.
       01  USERNAME            PIC  X(30) VALUE 'cobol'.
       01  PASSWD              PIC  X(10) VALUE 'cbl85'.

       01  SQL-STUDENT.
           03 SQL-S-LASTNAME   PIC X(07).
           03 SQL-S-FIRSTNAME  PIC X(06).
           03 SQL-S-AGE        PIC 9(03).
       
       01  SQL-COURSE.
           05 SQL-C-LABEL      PIC X(21).
           05 SQL-C-COEF       PIC 9V9.

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
OCESQL     02  FILLER PIC X(068) VALUE "INSERT INTO STUDENT (LASTNAME,"
OCESQL  &  " FIRSTNAME, AGE) VALUES ( $1, $2, $3 )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(105) VALUE "INSERT INTO COURSE (LABEL, COE"
OCESQL  &  "F) SELECT $1, $2 WHERE NOT EXISTS ( SELECT 1 FROM COURSE W"
OCESQL  &  "HERE LABEL = $3 )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(069) VALUE "SELECT STUDENT.ID FROM STUDENT"
OCESQL  &  " WHERE LASTNAME = $1 AND FIRSTNAME = $2".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0005.
OCESQL     02  FILLER PIC X(045) VALUE "SELECT COURSE.ID FROM COURSE W"
OCESQL  &  "HERE LABEL = $1".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0006.
OCESQL     02  FILLER PIC X(070) VALUE "INSERT INTO GRADE (STUDENT_ID,"
OCESQL  &  " COURSE_ID, GRADE) VALUES ( $1, $2, $3 )".
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
               PERFORM 2000-START-FILE-READ THRU END-2000-FILE-READ
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
      *    Lecture du fichier "input.dat", appel différents paragraphes*
      *    selon les 2 premiers caractères du fichier.                 *
      ******************************************************************
       2000-START-FILE-READ.
           OPEN INPUT F-INPUT.

           IF NOT FS-INPUT-OK
               DISPLAY 'ABORT POPULATING TABLE'
               GO TO END-2000-FILE-READ
           END-IF.
           
           PERFORM UNTIL FS-INPUT-EOF
               READ F-INPUT
               EVALUATE REC-F-INPUT-2
                   WHEN '01'
                       PERFORM 2100-START-HANDLE-STUDENT
                           THRU END-2100-HANDLE-STUDENT
                   WHEN "02"
                       PERFORM 2100-START-HANDLE-COURSE 
                          THRU END-2100-HANDLE-COURSE
                   PERFORM 2100-START-HANDLE-GRADE 
                          THRU END-2100-HANDLE-GRADE
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM.

           CLOSE F-INPUT.
       END-2000-FILE-READ.
           EXIT.

      ******************************************************************
      *    Ajoute un étudiant dans la DB par rapport au RECORD du      *
      *    fichier lu.                                                 *
      ******************************************************************
       2100-START-HANDLE-STUDENT.
           MOVE R-S-LASTNAME  TO SQL-S-LASTNAME.
           MOVE R-S-FIRSTNAME TO SQL-S-FIRSTNAME.
           MOVE R-S-AGE       TO SQL-S-AGE.
           
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
OCESQL          BY REFERENCE SQ0002
OCESQL          BY VALUE 3
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
       END-2100-HANDLE-STUDENT.
           EXIT.

      ******************************************************************
      *    Ajoute un cours dans la DB par rapport au RECORD du         *
      *    fichier lu.                                                 *
      ******************************************************************
       2100-START-HANDLE-COURSE.
           MOVE R-C-LABEL TO SQL-C-LABEL.
           MOVE R-C-COEF  TO SQL-C-COEF.
           
      *    Ajoute un nouveau cours si le label n'existe pas
OCESQL*    EXEC SQL
OCESQL*        INSERT INTO COURSE (LABEL, COEF)
OCESQL*        SELECT :SQL-C-LABEL, :SQL-C-COEF
OCESQL*        WHERE NOT EXISTS (
OCESQL*            SELECT 1
OCESQL*            FROM COURSE
OCESQL*            WHERE LABEL = :SQL-C-LABEL
OCESQL*            )
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 21
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-C-LABEL
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 2
OCESQL          BY VALUE -1
OCESQL          BY REFERENCE SQL-C-COEF
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 21
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-C-LABEL
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL          BY VALUE 3
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
       END-2100-HANDLE-COURSE.
           EXIT.

      ******************************************************************
      *    Ajoute une note dans la DB avec l'ID de l'étudiant et       *
      *    l'ID du cours qui sont associés à la note.                  *
      ******************************************************************
       2100-START-HANDLE-GRADE.
      *    Récupère l'ID d'un étudiant spécifique basé sur 
      *    son nom et prénom, 
      *    et stock cette valeur dans SQL-G-STUDENT-ID
OCESQL*    EXEC SQL
OCESQL*           SELECT STUDENT.ID INTO :SQL-G-STUDENT-ID FROM STUDENT
OCESQL*           WHERE LASTNAME = :SQL-S-LASTNAME 
OCESQL*           AND FIRSTNAME = :SQL-S-FIRSTNAME
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 1
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-G-STUDENT-ID
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
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0004
OCESQL          BY VALUE 2
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.

      *    Récupère l'ID d'un cours spécifique basé sur son label, 
      *    et stock cette valeur dans SQL-G-COURSE-ID
           MOVE R-C-LABEL TO SQL-C-LABEL.
OCESQL*    EXEC SQL
OCESQL*           SELECT COURSE.ID INTO :SQL-G-COURSE-ID FROM COURSE
OCESQL*           WHERE LABEL = :SQL-C-LABEL
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 1
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-G-COURSE-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 21
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-C-LABEL
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0005
OCESQL          BY VALUE 1
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.

      *    Ajoute une GRADE en utilisant 
      *    ID de l'étudiant et du cours récupérés précédemment, 
      *    ainsi que la note spécifiée.
           MOVE R-C-GRADE TO SQL-G-GRADE.
OCESQL*    EXEC SQL
OCESQL*           INSERT INTO GRADE (STUDENT_ID,COURSE_ID,GRADE) 
OCESQL*           VALUES (
OCESQL*               :SQL-G-STUDENT-ID, 
OCESQL*               :SQL-G-COURSE-ID,
OCESQL*               :SQL-G-GRADE
OCESQL*               )
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 1
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-G-STUDENT-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 1
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-G-COURSE-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE -2
OCESQL          BY REFERENCE SQL-G-GRADE
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0006
OCESQL          BY VALUE 3
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.
       END-2100-HANDLE-GRADE.
               EXIT.
               EXIT.
               EXIT.
