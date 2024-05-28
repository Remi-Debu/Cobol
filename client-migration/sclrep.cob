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

       01  WS-PRINT.
           03 WS-PNT-NBR    PIC Z9.
           03 WS-PNT-GRADE  PIC Z9.99.
           03 WS-PNT-COEF   PIC 9.9.
           03 WS-PNT-AST    PIC X(97) VALUE ALL "*".
           03 WS-PNT-BLANK  PIC X(40) VALUE SPACES.
           03 WS-PNT-EMPTY  PIC X     VALUE SPACE.
           03 WS-PNT-STRING PIC X(97).

       01  WS-STRING-POS    PIC 9(03) VALUE 1.
       01  WS-NUM-TEMP      PIC 9(03)V9(02).
       01  WS-SUM-AV-GRADE  PIC 9(10)V9(02).  

       01  TABLE-STUDENT.
           03  S-CNT  PIC 9(03).
           03  STUDENT OCCURS 200 TIMES
                          INDEXED BY S-IDX.
               05 S-ID             PIC 9.
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
           03 SQL-S-ID         PIC 9.
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
OCESQL     02  FILLER PIC X(256) VALUE "SELECT s.id, s.lastname, s.fir"
OCESQL  &  "stname, s.age, ROUND(SUM(g.grade * c.coef) / SUM(c.coef), "
OCESQL  &  "2) AS weighted_average FROM student s JOIN grade g ON s.id"
OCESQL  &  " = g.student_id JOIN course c ON g.course_id = c.id GROUP "
OCESQL  &  "BY s.id, s.lastname, s.firstname ORDER BY s.lastname".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(138) VALUE "SELECT c.id, c.label, c.coef, "
OCESQL  &  "ROUND(AVG(g.grade), 2) FROM grade g JOIN course c ON g.cou"
OCESQL  &  "rse_id = c.id GROUP BY c.id, c.label ORDER BY c.id".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(081) VALUE "SELECT student_id, course_id, "
OCESQL  &  "grade FROM grade ORDER BY student_id, course_id ASC".
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
               PERFORM 4000-START-WRITE THRU END-4000-WRITE
           END-IF.
           
           CALL "mytest".
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
      *    Requêtes SQL.                                               *
      ******************************************************************
       2000-START-SQL-REQUEST.
      *    Récupère les informations des étudiants ID, nom, prénom, âge 
      *    et la moyenne pondérée de ses notes.
OCESQL*    EXEC SQL
OCESQL*        DECLARE CRSSTUDENT CURSOR FOR
OCESQL*        SELECT s.id, s.lastname, s.firstname, s.age,
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
      *    Récupère les informations des cours ID, label, coefficient
      *    et la moyenne des notes pour chaque cours.
OCESQL*    EXEC SQL 
OCESQL*        DECLARE CRSCOURSE CURSOR FOR
OCESQL*        SELECT c.id, c.label, c.coef, ROUND(AVG(g.grade), 2)
OCESQL*        FROM grade g
OCESQL*        JOIN course c ON g.course_id = c.id
OCESQL*        GROUP BY c.id, c.label
OCESQL*        ORDER BY c.id
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "sclrep_CRSCOURSE" & x"00"
OCESQL          BY REFERENCE SQ0003
OCESQL     END-CALL.
      *    Récupère toutes les informations des notes, en les triant 
      *    par ID d'étudiant et ID de cours (Tri ascendant).
OCESQL*    EXEC SQL
OCESQL*        DECLARE CRSGRADE CURSOR FOR
OCESQL*        SELECT student_id, course_id, grade FROM grade
OCESQL*        ORDER BY student_id, course_id ASC
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLCursorDeclare" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "sclrep_CRSGRADE" & x"00"
OCESQL          BY REFERENCE SQ0004
OCESQL     END-CALL.
       END-2000-SQL-REQUEST.
           EXIT.

      ******************************************************************
      *    Appel les différents paragraphe "HANDLE".                   *
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
      *    Récupère les données d'un étudiant grâce au curseur         *
      *    "CRSSTUDENT" et l'insère dans la table STUDENT.             *
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
OCESQL*            INTO :SQL-S-ID, :SQL-S-LASTNAME, :SQL-S-FIRSTNAME, 
OCESQL*            :SQL-S-AGE, :SQL-S-AV-GRADE
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 1
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE SQL-S-ID
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
                       MOVE SQL-S-ID        TO S-ID(S-CNT)
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
      *    Récupère les données d'un cours grâce au curseur            *
      *    "CRSCOURSE" et l'insère dans la table COURSE.               *
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
                       
                       STRING "C" SQL-C-ID DELIMITED BY SIZE 
                       INTO C-ID-NAME(C-CNT)
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
      *    Récupère les données d'une note grâce au curseur            *
      *    "CRSGRADE" et l'insère dans la table GRADE.                 *
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

      ******************************************************************
      *    Appel différents paragraphes qui vont servir à écrire le    *
      *    rapport "output.dat".                                       *
      ******************************************************************
       4000-START-WRITE.
           OPEN OUTPUT F-OUTPUT.
           PERFORM START-HEADER THRU END-HEADER.
           PERFORM START-TABLE-HEADER THRU END-TABLE-HEADER.
           PERFORM START-TABLE-DETAILS THRU END-TABLE-DETAILS.
           PERFORM START-TABLE-FOOTER THRU END-TABLE-FOOTER.
           PERFORM START-LEXIQUE THRU END-LEXIQUE.
           PERFORM START-FOOTER THRU END-FOOTER.
           CLOSE F-OUTPUT.
       END-4000-WRITE.
           EXIT.

      ******************************************************************
      *    Ecris l'en-tête du rapport.                                 *
      ******************************************************************
       START-HEADER.
           INITIALIZE WS-PNT-STRING.

           WRITE R-OUTPUT FROM WS-PNT-AST.

           STRING WS-PNT-BLANK "BULLETIN DE NOTES"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING.

           WRITE R-OUTPUT FROM WS-PNT-STRING.
           WRITE R-OUTPUT FROM WS-PNT-AST.
       END-HEADER.
           EXIT.

      ******************************************************************
      *    Ecris l'en-tête du tableau qui contient le nom des colonnes *
      *    (NOM, PRENOM, MOYENNE, C1, C2, ...)                         *
      ******************************************************************
       START-TABLE-HEADER.
           INITIALIZE WS-PNT-STRING.

           STRING "PRENOM"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING.

           STRING "NOM"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(12:10).

           STRING "MOYENNE"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(23:20).

           SET WS-STRING-POS TO 35.
           PERFORM VARYING C-IDX FROM 1 BY 1 UNTIL C-IDX > C-CNT
              INITIALIZE WS-PNT-NBR

              STRING C-ID-NAME(C-IDX)
              DELIMITED BY SIZE
              INTO WS-PNT-STRING(WS-STRING-POS:29)

              ADD 10 TO WS-STRING-POS
           END-PERFORM.
           
           WRITE R-OUTPUT FROM WS-PNT-STRING.
           WRITE R-OUTPUT FROM WS-PNT-EMPTY.
       END-TABLE-HEADER.
           EXIT.

      ******************************************************************
      *    Ecris chaque ligne du tableau qui contient les valeurs qui  *
      *    correspondent au nom des colonnes.                          *
      ******************************************************************
       START-TABLE-DETAILS.
           PERFORM VARYING S-IDX FROM 1 BY 1 UNTIL S-IDX > S-CNT
              INITIALIZE WS-PNT-STRING

              STRING S-FIRSTNAME(S-IDX) SPACE S-LASTNAME(S-IDX)
              DELIMITED BY SIZE
              INTO WS-PNT-STRING(1:20) 
              
              PERFORM START-TABLE-DETAILS-C THRU END-TABLE-DETAILS-C
              
      *       Effectue la somme des moyennes générales de chaque élève.
              ADD S-AV-GRADE(S-IDX) TO WS-SUM-AV-GRADE
              INITIALIZE WS-PNT-GRADE
              MOVE S-AV-GRADE(S-IDX) TO WS-PNT-GRADE

              STRING WS-PNT-GRADE
              DELIMITED BY SIZE
              INTO WS-PNT-STRING(23:10)

              WRITE R-OUTPUT FROM WS-PNT-STRING
           END-PERFORM.
       END-TABLE-DETAILS.
           EXIT.
      
      ******************************************************************
      *    Ajoute à la ligne de détails du tableau les notes de        *
      *    l'élève dans chaque matière et effectue la somme de         *
      *    ses notes.                                                  *
      ******************************************************************
       START-TABLE-DETAILS-C.
           SET WS-STRING-POS TO 33.
           PERFORM VARYING G-IDX FROM 1 BY 1 UNTIL G-IDX > G-CNT
              STRING " N/A"
              DELIMITED BY SIZE
              INTO WS-PNT-STRING(WS-STRING-POS:20)

              IF G-S-ID(G-IDX) EQUAL S-ID(S-IDX)
                 INITIALIZE WS-PNT-GRADE
                 MOVE G-GRADE(G-IDX) TO WS-PNT-GRADE
   
                 STRING WS-PNT-GRADE
                 DELIMITED BY SIZE
                 INTO WS-PNT-STRING(WS-STRING-POS:20)
   
                 ADD 10 TO WS-STRING-POS
              END-IF

              STRING " N/A"
              DELIMITED BY SIZE
              INTO WS-PNT-STRING(WS-STRING-POS:20)
           END-PERFORM.
       END-TABLE-DETAILS-C.
           EXIT.

      ******************************************************************
      *    Ecris la dernière du tableau qui contient la moyenne        *
      *    générale de la classe et les moyennes de la classe dans     *
      *    chaque cours.                                               *
      ******************************************************************
       START-TABLE-FOOTER.
           INITIALIZE WS-PNT-STRING.

           STRING "CLASSE"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(1:20).

      *    Calcul de la moyenne générale de la classe   
           COMPUTE WS-NUM-TEMP ROUNDED = WS-SUM-AV-GRADE / (S-CNT)
           MOVE WS-NUM-TEMP TO WS-PNT-GRADE.

           STRING WS-PNT-GRADE
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(23:20).

           SET WS-STRING-POS TO 33.
           PERFORM VARYING C-IDX FROM 1 BY 1 UNTIL C-IDX > C-CNT
               INITIALIZE WS-PNT-GRADE

               MOVE C-AV-GRADE(C-IDX) TO WS-PNT-GRADE

               STRING WS-PNT-GRADE
               DELIMITED BY SIZE
               INTO WS-PNT-STRING(WS-STRING-POS:20)

               ADD 10 TO WS-STRING-POS
           END-PERFORM.

           WRITE R-OUTPUT FROM WS-PNT-EMPTY.
           WRITE R-OUTPUT FROM WS-PNT-STRING.
       END-TABLE-FOOTER.
           EXIT.

      ******************************************************************
      *    Ecris le lexique de la signification de C1, C2, ...         *
      ******************************************************************
       START-LEXIQUE.
           WRITE R-OUTPUT FROM WS-PNT-AST.

           PERFORM VARYING C-IDX FROM 1 BY 1 UNTIL C-IDX > C-CNT
               INITIALIZE WS-PNT-STRING
               INITIALIZE WS-PNT-NBR
               INITIALIZE WS-PNT-COEF

               MOVE C-COEF(C-IDX) TO WS-PNT-COEF

               STRING FUNCTION TRIM(C-ID-NAME(C-IDX))
               SPACE "=> COEF:" SPACE FUNCTION TRIM(WS-PNT-COEF) 
               SPACE "LABEL:" SPACE C-LABEL(C-IDX)
               DELIMITED BY SIZE
               INTO WS-PNT-STRING

               WRITE R-OUTPUT FROM WS-PNT-STRING
           END-PERFORM.
       END-LEXIQUE.
           EXIT.

      ******************************************************************
      *    Ecris le pied de page du rapport qui contient les nombres   *
      *    d'élèves, de cours et de notes et pour finir le message de  * 
      *    fin du rapport                                              *
      ******************************************************************
       START-FOOTER.
      *    Nombre d'élèves
           WRITE R-OUTPUT FROM WS-PNT-AST.

           INITIALIZE WS-NUM-TEMP.
           INITIALIZE WS-PNT-STRING.
           INITIALIZE WS-PNT-NBR.

           STRING "NOMBRE D'ELEVES" SPACE
           DELIMITED BY SIZE 
           INTO WS-PNT-STRING.

           MOVE S-CNT TO WS-NUM-TEMP.
           MOVE WS-NUM-TEMP TO WS-PNT-NBR. 

           STRING SPACE "=>" SPACE WS-PNT-NBR
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(16:7).

           WRITE R-OUTPUT FROM WS-PNT-STRING.

      *    Nombre de cours   
           INITIALIZE WS-NUM-TEMP.
           INITIALIZE WS-PNT-STRING.
           INITIALIZE WS-PNT-NBR.

           STRING "NOMBRE DE COURS" SPACE
           DELIMITED BY SIZE 
           INTO WS-PNT-STRING.

           MOVE C-CNT TO WS-NUM-TEMP.
           MOVE WS-NUM-TEMP TO WS-PNT-NBR.

           STRING SPACE "=>" SPACE WS-PNT-NBR
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(16:7).

           WRITE R-OUTPUT FROM WS-PNT-STRING.

      *    Nombre de notes
           INITIALIZE WS-NUM-TEMP.
           INITIALIZE WS-PNT-STRING.
           INITIALIZE WS-PNT-NBR.

           STRING "NOMBRE DE NOTES" SPACE
           DELIMITED BY SIZE 
           INTO WS-PNT-STRING.

           MOVE G-CNT TO WS-NUM-TEMP.
           MOVE WS-NUM-TEMP TO WS-PNT-NBR.

           STRING SPACE "=>" SPACE WS-PNT-NBR
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(16:7).

           WRITE R-OUTPUT FROM WS-PNT-STRING.
      
      *    Fin de rapport
           WRITE R-OUTPUT FROM WS-PNT-AST.

           INITIALIZE WS-PNT-STRING.

           STRING WS-PNT-BLANK "FIN DU RAPPORT"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING.

           WRITE R-OUTPUT FROM WS-PNT-STRING.
       END-FOOTER.
           EXIT.
           
           
           
