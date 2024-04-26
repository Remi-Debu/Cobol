      ******************************************************************
      *    Le programme lis le fichier "input.dat" pour ensuite        *
      *    écrire dans un fichier "output.dat" un rapport sur les      *
      *    notes des élèves.                                           *
      *    Le rapport contient les notes et les moyennes en fonction   *
      *    des coefficients des matières de chaque élève et de la      *
      *    classe, ainsi que les moyennes générales.                   *
      *    À la fin du rapport on retrouve un résumé des données       *
      *    nombre d'élèves, de matières et de notes.                   *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. notele.
       AUTHOR.     Rémi.

      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT ASSIGN TO "input.dat"
           ACCESS MODE IS SEQUENTIAL
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-INPUT.

           SELECT F-OUTPUT ASSIGN TO "output.dat"
           ACCESS MODE IS SEQUENTIAL
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-OUTPUT.
      
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS
           RECORDING MODE IS V.
       01  REC-F-INPUT-2    PIC X(02).

       01  REC-STUDENT.
           03 R-S-KEY       PIC 9(02).
           03 R-S-LASTNAME  PIC X(07).
           03 R-S-FIRSTNAME PIC X(06).   
           03 R-S-AGE       PIC 9(02).

       01  REC-COURSE.
           03 R-C-KEY       PIC 9(02).
           03 R-C-LABEL     PIC X(21).
           03 R-C-COEF      PIC X(03).
           03 R-C-GRADE     PIC X(05).

       FD  F-OUTPUT
           RECORD CONTAINS 2000 CHARACTERS
           RECORDING MODE IS F.
       01  REC-F-OUTPUT     PIC X(2000).

       WORKING-STORAGE SECTION.
       01  FS-INPUT  PIC X(02).
           88 FS-INPUT-OK  VALUE "0".
           88 FS-INPUT-EOF VALUE "10".
           
       01  FS-OUTPUT PIC X(02).
           88 FS-OUTPUT-OK  VALUE "0".
           88 FS-OUTPUT-EOF VALUE "10".
       
       01  TABLE-STUDENT.
           03  S-CNT  PIC 9(03) VALUE 1.
           03  STUDENT OCCURS 1 TO 200 TIMES
                          DEPENDING ON S-CNT
                          INDEXED BY S-IDX.
               05 S-FIRSTNAME      PIC X(10).
               05 S-LASTNAME       PIC X(10).
               05 S-AGE            PIC 9(02).
               05 S-SUM-GRADE-COEF PIC 9(05)V9(02).
               05 S-AV-GRADE       PIC 9(02)V9(02).
       
       01  TABLE-COURSE.
           03 C-CNT  PIC 9(03) VALUE 1.
           03 COURSE OCCURS 1 TO 200 TIMES
                        DEPENDING ON C-CNT
                        INDEXED BY C-IDX.
               05 C-ID          PIC X(03).
               05 C-ID-NAME     PIC X(04).
               05 C-LABEL       PIC X(21).
               05 C-COEF        PIC 9V9.
               05 C-SUM-GRADE   PIC 9(05)V9(02).
               05 WS-C-AV-GRADE PIC 9(02)V9(02).

       01  TABLE-GRADE.
           03 G-CNT PIC 9(03) VALUE 1.
           03 GRADE OCCURS 1 TO 200 TIMES
                       DEPENDING ON G-CNT
                       INDEXED BY G-IDX.
               05 G-S-FULLNAME PIC X(20).
               05 G-C-LABEL    PIC X(25).
               05 G-COEF       PIC 9V9.
               05 G-GRADE      PIC 99V99.

       01  WS-IS-EXIST      PIC X.
           88 WS-IS-EXIST-Y VALUE "Y".
           88 WS-IS-EXIST-N VALUE "N".

       01  WS-PNT.
           03 WS-PNT-NBR    PIC Z9.
           03 WS-PNT-GRADE  PIC Z9,99.
           03 WS-PNT-COEF   PIC 9,9.
           03 WS-PNT-DASH   PIC X(87).
           03 WS-PNT-AST    PIC X(87).
           03 WS-PNT-BLANK  PIC X(35).
           03 WS-PNT-EMPTY  PIC X.
           03 WS-PNT-STRING PIC X(87).

       01  WS-I             PIC 9(03) VALUE 1.
       01  WS-J             PIC 9(03) VALUE 1.
       01  WS-STRING-POS    PIC 9(03) VALUE 1.
       01  WS-NUM-TEMP      PIC 9(03)V9(02).
       01  WS-FULLNAME-TEMP PIC X(30).
       01  WS-SUM-COEF      PIC 9(10)V9.
       01  WS-SUM-GRADE     PIC 9(10)V9(02).


      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM START-MAIN THRU END-MAIN.
           STOP RUN.

      ******************************************************************
      *    MAIN                                                        *
      ******************************************************************
       START-MAIN.
           MOVE ALL "-" TO WS-PNT-DASH.
           MOVE ALL "*" TO WS-PNT-AST.

           PERFORM START-R-IP THRU END-R-IP.
           PERFORM START-W-OP THRU END-W-OP.
       END-MAIN.
       
      ******************************************************************
      *    Lis le fichier "input.dat" et appels différents paragraphes *
      *    qui vont servir à stocker dans la WS les données lue.       *
      ******************************************************************
       START-R-IP.
           OPEN INPUT F-INPUT.

           SET FS-INPUT-OK TO TRUE.
           PERFORM UNTIL FS-INPUT-EOF
               READ F-INPUT 
               AT END SET FS-INPUT-EOF TO TRUE
               NOT AT END 
                   IF REC-F-INPUT-2 EQUAL "01"
                   PERFORM START-HANDLE-STUDENT THRU END-HANDLE-STUDENT
                   END-IF

                   IF REC-F-INPUT-2 EQUAL "02"
                   PERFORM START-HANDLE-COURSE THRU END-HANDLE-COURSE
                   PERFORM START-HANDLE-GRADE THRU END-HANDLE-GRADE
                   END-IF
               END-READ
           END-PERFORM.

           CLOSE F-INPUT.
       END-R-IP.

      ******************************************************************
      *    Stock les données RECORD STUDENT dans la table STUDENT de   *
      *    la WS.                                                      *
      ******************************************************************
       START-HANDLE-STUDENT.
           MOVE R-S-FIRSTNAME TO S-FIRSTNAME(S-CNT).
           MOVE R-S-LASTNAME  TO S-LASTNAME(S-CNT).
           MOVE R-S-AGE       TO S-AGE(S-CNT).

           ADD 1 TO S-CNT.
       END-HANDLE-STUDENT.

      ******************************************************************
      *    Stock les données RECORD COURSE seulement si le record      *
      *    label n'a pas encore été stocké dans la table COURSE de     *
      *    la WS.                                                      *
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
               MOVE C-CNT  TO C-ID(C-CNT)

               MOVE C-ID(C-CNT) TO WS-PNT-NBR
               STRING "C" FUNCTION TRIM(WS-PNT-NBR) 
               DELIMITED BY SIZE
               INTO C-ID-NAME(C-CNT)

               MOVE R-C-LABEL TO C-LABEL(C-CNT)
               MOVE R-C-COEF  TO C-COEF(C-CNT)

               ADD 1 TO C-CNT
           END-IF.
       END-HANDLE-COURSE.

      ******************************************************************
      *    Stock les données RECORD GRADE dans la table GRADE de       *
      *    la WS.                                                      *
      ******************************************************************
       START-HANDLE-GRADE.
           STRING FUNCTION TRIM(S-FIRSTNAME(S-CNT - 1))
           SPACE FUNCTION TRIM(S-LASTNAME(S-CNT - 1))
           DELIMITED BY SIZE
           INTO G-S-FULLNAME(G-CNT).

           MOVE R-C-LABEL TO G-C-LABEL(G-CNT).
           MOVE R-C-GRADE TO G-GRADE(G-CNT).
           MOVE R-C-COEF  TO G-COEF(G-CNT).

           ADD 1 TO G-CNT.
       END-HANDLE-GRADE.

      ******************************************************************
      *    Appel différents paragraphes qui vont servir à écrire le    *
      *    rapport "output.dat".                                       *
      ******************************************************************
       START-W-OP.
           OPEN OUTPUT F-OUTPUT.
           PERFORM START-HEADER THRU END-HEADER.
           PERFORM START-TABLE-HEADER THRU END-TABLE-HEADER.
           PERFORM START-TABLE-DETAILS THRU END-TABLE-DETAILS.
           PERFORM START-TABLE-FOOTER THRU END-TABLE-FOOTER.
           PERFORM START-FOOTER THRU END-FOOTER.
           CLOSE F-OUTPUT.
       END-W-OP.

      ******************************************************************
      *    Ecris l'en-tête du rapport.                                 *
      ******************************************************************
       START-HEADER.
           INITIALIZE WS-PNT-STRING.

           WRITE REC-F-OUTPUT FROM WS-PNT-AST.

           STRING WS-PNT-BLANK "BULLETIN DE NOTES"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING.

           WRITE REC-F-OUTPUT FROM WS-PNT-STRING.
       END-HEADER.

      ******************************************************************
      *    Ecris l'en-tête de la table (les colonnes.                  *
      ******************************************************************
       START-TABLE-HEADER.
           INITIALIZE WS-PNT-STRING.
           
           WRITE REC-F-OUTPUT FROM WS-PNT-AST.

           STRING "NOM"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING.

           STRING "PRENOM"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(12:10).

           STRING "MOYENNE"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(23:20).

           SET WS-I TO 1.
           SET WS-STRING-POS TO 35.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >= C-CNT
              STRING C-ID-NAME(WS-I)
              DELIMITED BY SIZE
              INTO WS-PNT-STRING(WS-STRING-POS:29)

              ADD C-COEF(WS-I) TO WS-SUM-COEF
              ADD 10 TO WS-STRING-POS
           END-PERFORM.
           
           WRITE REC-F-OUTPUT FROM WS-PNT-STRING.
           WRITE REC-F-OUTPUT FROM WS-PNT-EMPTY.
       END-TABLE-HEADER.

      ******************************************************************
       START-TABLE-DETAILS.
           SET WS-I TO 1.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >= S-CNT
              INITIALIZE WS-FULLNAME-TEMP
              INITIALIZE WS-PNT-STRING

              STRING FUNCTION TRIM(S-FIRSTNAME(WS-I)) 
              SPACE FUNCTION TRIM(S-LASTNAME(WS-I))
              DELIMITED BY SIZE
              INTO WS-FULLNAME-TEMP 

              STRING S-FIRSTNAME(WS-I) SPACE S-LASTNAME(WS-I)
              DELIMITED BY SIZE
              INTO WS-PNT-STRING(1:20) 
              
              PERFORM START-TABLE-DETAILS-C THRU END-TABLE-DETAILS-C
              
              DIVIDE S-SUM-GRADE-COEF(WS-I) BY WS-SUM-COEF 
              GIVING S-AV-GRADE(WS-I) ROUNDED
              
              INITIALIZE WS-PNT-GRADE
              MOVE S-AV-GRADE(WS-I) TO WS-PNT-GRADE
              ADD S-AV-GRADE(WS-I)  TO WS-SUM-GRADE

              STRING WS-PNT-GRADE
              DELIMITED BY SIZE
              INTO WS-PNT-STRING(23:10)

              WRITE REC-F-OUTPUT FROM WS-PNT-STRING
           END-PERFORM.
       END-TABLE-DETAILS.

      ******************************************************************
       START-TABLE-DETAILS-C.
           SET WS-STRING-POS TO 33
           SET WS-J TO 1
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J >= G-CNT
              IF G-S-FULLNAME(WS-J) EQUAL WS-FULLNAME-TEMP
              INITIALIZE WS-PNT-GRADE
              MOVE G-GRADE(WS-J) TO WS-PNT-GRADE

              STRING WS-PNT-GRADE
              DELIMITED BY SIZE
              INTO WS-PNT-STRING(WS-STRING-POS:20)

      *       Ajoute les notes de l'eleve en fonction du coefficient
      *       de la matière
              MULTIPLY G-GRADE(WS-J) BY G-COEF(WS-J) 
              GIVING WS-NUM-TEMP
              ADD WS-NUM-TEMP TO S-SUM-GRADE-COEF(WS-I)

              ADD 10 TO WS-STRING-POS
              END-IF
           END-PERFORM.
       END-TABLE-DETAILS-C.

      ******************************************************************
       START-TABLE-FOOTER.
           INITIALIZE WS-PNT-STRING.
           STRING "CLASSE"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(1:20).
           
           COMPUTE WS-NUM-TEMP ROUNDED = WS-SUM-GRADE / (S-CNT - 1)
           MOVE WS-NUM-TEMP TO WS-PNT-GRADE.

           STRING WS-PNT-GRADE
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(23:20).

           SET WS-STRING-POS TO 33.
           SET WS-I TO 1
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >= C-CNT
               INITIALIZE WS-PNT-GRADE

               SET WS-J TO 1
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J >= G-CNT
                   IF G-C-LABEL(WS-J) EQUAL C-LABEL(WS-I)
                      ADD G-GRADE(WS-J) TO C-SUM-GRADE(WS-I)
                   END-IF
               END-PERFORM
               
               COMPUTE WS-C-AV-GRADE(WS-I) = C-SUM-GRADE(WS-I) /
               (S-CNT - 1)
               MOVE WS-C-AV-GRADE(WS-I) TO WS-PNT-GRADE
               STRING WS-PNT-GRADE
               DELIMITED BY SIZE
               INTO WS-PNT-STRING(WS-STRING-POS:20)

               ADD 10 TO WS-STRING-POS
           END-PERFORM.

           WRITE REC-F-OUTPUT FROM WS-PNT-EMPTY.
           WRITE REC-F-OUTPUT FROM WS-PNT-STRING.
       END-TABLE-FOOTER.

      ******************************************************************
       START-FOOTER.
           WRITE REC-F-OUTPUT FROM WS-PNT-AST.

           PERFORM VARYING C-IDX FROM 1 BY 1 UNTIL C-IDX >= C-CNT
               INITIALIZE WS-PNT-STRING
               INITIALIZE WS-PNT-NBR
               INITIALIZE WS-PNT-COEF

               MOVE C-COEF(C-IDX) TO WS-PNT-COEF

               STRING FUNCTION TRIM(C-ID-NAME(C-IDX))
               SPACE "=> COEF:" SPACE FUNCTION TRIM(WS-PNT-COEF) 
               SPACE "LABEL:" SPACE C-LABEL(C-IDX)
               DELIMITED BY SIZE
               INTO WS-PNT-STRING

               WRITE REC-F-OUTPUT FROM WS-PNT-STRING
           END-PERFORM.

           WRITE REC-F-OUTPUT FROM WS-PNT-AST.

           INITIALIZE WS-PNT-STRING.
           INITIALIZE WS-NUM-TEMP.
           INITIALIZE WS-PNT-NBR.
           STRING "NOMBRE D'ELEVES" SPACE
           DELIMITED BY SIZE INTO WS-PNT-STRING.
           MOVE S-CNT TO WS-NUM-TEMP.
           SUBTRACT 1 FROM WS-NUM-TEMP.
           MOVE WS-NUM-TEMP TO WS-PNT-NBR.  
           STRING SPACE "=>" SPACE WS-PNT-NBR
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(16:7).
           WRITE REC-F-OUTPUT FROM WS-PNT-STRING.
           
           INITIALIZE WS-PNT-STRING.
           INITIALIZE WS-NUM-TEMP.
           INITIALIZE WS-PNT-NBR.
           STRING "NOMBRE DE COURS" SPACE
           DELIMITED BY SIZE INTO WS-PNT-STRING.
           MOVE C-CNT TO WS-NUM-TEMP.
           SUBTRACT 1 FROM WS-NUM-TEMP.
           MOVE WS-NUM-TEMP TO WS-PNT-NBR.
           STRING SPACE "=>" SPACE WS-PNT-NBR
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(16:7).
           WRITE REC-F-OUTPUT FROM WS-PNT-STRING.

           INITIALIZE WS-PNT-STRING.
           INITIALIZE WS-NUM-TEMP.
           INITIALIZE WS-PNT-NBR.
           STRING "NOMBRE DE NOTES" SPACE
           DELIMITED BY SIZE INTO WS-PNT-STRING.
           MOVE G-CNT TO WS-NUM-TEMP.
           SUBTRACT 1 FROM WS-NUM-TEMP.
           MOVE WS-NUM-TEMP TO WS-PNT-NBR.
           STRING SPACE "=>" SPACE WS-PNT-NBR
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(16:7).
           WRITE REC-F-OUTPUT FROM WS-PNT-STRING.

           WRITE REC-F-OUTPUT FROM WS-PNT-AST.

           INITIALIZE WS-PNT-STRING.
           STRING WS-PNT-BLANK "FIN DU RAPPORT"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING.
           WRITE REC-F-OUTPUT FROM WS-PNT-STRING.
       END-FOOTER.
       