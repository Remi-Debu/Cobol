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
       
       01  TABLE-STUDENT.
           03  S-CNT  PIC 9(03) VALUE 1.
           03  STUDENT OCCURS 1 TO 200 TIMES
                          DEPENDING ON S-CNT
                          INDEXED BY S-IDX.
               05 S-LASTNAME       PIC X(10).
               05 S-FIRSTNAME      PIC X(10).
               05 S-AGE            PIC 9(02).
               05 S-SUM-GRADE-COEF PIC 9(05)V9(02).
               05 S-AV-GRADE       PIC 9(02)V9(02).
       
       01  TABLE-COURSE.
           03 C-CNT  PIC 9(03) VALUE 1.
           03 COURSE OCCURS 1 TO 200 TIMES
                        DEPENDING ON C-CNT
                        INDEXED BY C-IDX.
               05 C-ID        PIC X(10).
               05 C-ID-NAME   PIC X(04).
               05 C-LABEL     PIC X(21).
               05 C-COEF      PIC 9V9.
               05 C-SUM-GRADE PIC 9(05)V9(02).
               05 C-AV-GRADE  PIC 9(02)V9(02).

       01  TABLE-GRADE.
           03 G-CNT PIC 9(03) VALUE 1.
           03 GRADE OCCURS 1 TO 200 TIMES
                       DEPENDING ON G-CNT
                       INDEXED BY G-IDX.
               05 G-S-FULLNAME PIC X(20).
               05 G-C-LABEL    PIC X(25).
               05 G-COEF       PIC 9V9.
               05 G-GRADE      PIC 9(02)V9(02).

       01  WS-IS-EXIST      PIC X.
           88 WS-IS-EXIST-Y VALUE "Y".
           88 WS-IS-EXIST-N VALUE "N".

       01  WS-PRINT.
           03 WS-PNT-NBR    PIC Z9.
           03 WS-PNT-GRADE  PIC Z9,99.
           03 WS-PNT-COEF   PIC 9,9.
           03 WS-PNT-AST    PIC X(87) VALUE ALL "*".
           03 WS-PNT-BLANK  PIC X(35) VALUE SPACES.
           03 WS-PNT-EMPTY  PIC X     VALUE SPACE.
           03 WS-PNT-STRING PIC X(87).

       01  WS-STRING-POS    PIC 9(03) VALUE 1.
       01  WS-NUM-TEMP      PIC 9(03)V9(02).
       01  WS-FULLNAME-TEMP PIC X(30).
       01  WS-SUM-COEF      PIC 9(10)V9.
       01  WS-SUM-AV-GRADE  PIC 9(10)V9(02).


      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM START-MAIN THRU END-MAIN.
           STOP RUN.

      ******************************************************************
      *    MAIN qui appel le paragraphe qui s'occupe de la lecture du  *
      *    fichier "input.dat" puis celui qui s'occupe de l'écriture   *
      *    dans le fichier "output.dat".                               *
      ******************************************************************
       START-MAIN.
           PERFORM START-R-IP THRU END-R-IP.
           PERFORM START-SORT THRU END-SORT.
           PERFORM START-W-OP THRU END-W-OP.
       END-MAIN.
           EXIT.
       
      ******************************************************************
      *    Lis le fichier "input.dat" et appels différents paragraphes *
      *    qui vont servir à stocker dans la WS les données lue.       *
      ******************************************************************
       START-R-IP.
           OPEN INPUT F-INPUT.
           IF FS-INPUT EQUAL "00"
              SET FS-INPUT-OK TO TRUE

              PERFORM UNTIL FS-INPUT-EOF
                 READ F-INPUT 
                 AT END 
                    SUBTRACT 1 FROM S-CNT
                    SUBTRACT 1 FROM C-CNT
                    SUBTRACT 1 FROM G-CNT
                    SET FS-INPUT-EOF TO TRUE
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
              DISPLAY "ERREUR :" SPACE FS-INPUT
           END-IF.
           CLOSE F-INPUT.
       END-R-IP.
           EXIT.

      ******************************************************************
      *    Stock les données RECORD STUDENT dans la table STUDENT de   *
      *    la WS.                                                      *
      ******************************************************************
       START-HANDLE-STUDENT.
           MOVE R-S-LASTNAME  TO S-LASTNAME(S-CNT).
           MOVE R-S-FIRSTNAME TO S-FIRSTNAME(S-CNT).
           MOVE R-S-AGE       TO S-AGE(S-CNT).

           ADD 1 TO S-CNT.
       END-HANDLE-STUDENT.
           EXIT.

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
               MOVE R-C-LABEL TO C-LABEL(C-CNT)
               MOVE R-C-COEF  TO C-COEF(C-CNT)

               ADD 1 TO C-CNT
           END-IF.
       END-HANDLE-COURSE.
           EXIT.

      ******************************************************************
      *    Stock les données RECORD GRADE dans la table GRADE de       *
      *    la WS.                                                      *
      ******************************************************************
       START-HANDLE-GRADE.
           STRING FUNCTION TRIM(S-LASTNAME(S-CNT - 1))
           SPACE FUNCTION TRIM(S-FIRSTNAME(S-CNT - 1))
           DELIMITED BY SIZE
           INTO G-S-FULLNAME(G-CNT).

           MOVE R-C-LABEL TO G-C-LABEL(G-CNT).
           MOVE R-C-GRADE TO G-GRADE(G-CNT).
           MOVE R-C-COEF  TO G-COEF(G-CNT).

           ADD 1 TO G-CNT.
       END-HANDLE-GRADE.
           EXIT.

      ******************************************************************
      *    Trie les tableaux COURSE, STUDENT ET GRADE.                 *
      *                  Par LABEL, LASTNAME et LABEL.                 *
      ******************************************************************
       START-SORT.
           SORT COURSE ASCENDING KEY C-LABEL.
           SORT STUDENT ASCENDING KEY S-LASTNAME.
           SORT GRADE ASCENDING KEY G-C-LABEL.
       END-SORT.
           EXIT.

      ******************************************************************
      *    Appel différents paragraphes qui vont servir à écrire le    *
      *    rapport "output.dat".                                       *
      ******************************************************************
       START-W-OP.
           PERFORM START-HEADER THRU END-HEADER.
           PERFORM START-TABLE-HEADER THRU END-TABLE-HEADER.
           PERFORM START-TABLE-DETAILS THRU END-TABLE-DETAILS.
           PERFORM START-TABLE-FOOTER THRU END-TABLE-FOOTER.
           PERFORM START-LEXICON THRU END-LEXICON.
           PERFORM START-FOOTER THRU END-FOOTER.
       END-W-OP.
           EXIT.

      ******************************************************************
      *    Ecris l'en-tête du rapport.                                 *
      ******************************************************************
       START-HEADER.
           OPEN OUTPUT F-OUTPUT.
           INITIALIZE WS-PNT-STRING.

           WRITE REC-F-OUTPUT FROM WS-PNT-AST.

           STRING WS-PNT-BLANK "BULLETIN DE NOTES"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING.

           WRITE REC-F-OUTPUT FROM WS-PNT-STRING.
           WRITE REC-F-OUTPUT FROM WS-PNT-AST.
           CLOSE F-OUTPUT.
       END-HEADER.
           EXIT.

      ******************************************************************
      *    Ecris l'en-tête du tableau qui contient le nom des colonnes *
      *    (NOM, PRENOM, MOYENNE, C1, C2, ...)                         *
      ******************************************************************
       START-TABLE-HEADER.
           OPEN EXTEND F-OUTPUT.
           INITIALIZE WS-PNT-STRING.

           STRING "NOM"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING.

           STRING "PRENOM"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(12:10).

           STRING "MOYENNE"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING(23:20).

           SET WS-STRING-POS TO 35.
           PERFORM VARYING C-IDX FROM 1 BY 1 UNTIL C-IDX > C-CNT
              INITIALIZE WS-PNT-NBR

              MOVE C-IDX TO C-ID(C-IDX)
              MOVE C-ID(C-IDX) TO WS-PNT-NBR

              STRING "C" FUNCTION TRIM(WS-PNT-NBR) 
              DELIMITED BY SIZE
              INTO C-ID-NAME(C-IDX)

              STRING C-ID-NAME(C-IDX)
              DELIMITED BY SIZE
              INTO WS-PNT-STRING(WS-STRING-POS:29)

              ADD C-COEF(C-IDX) TO WS-SUM-COEF
              ADD 10 TO WS-STRING-POS
           END-PERFORM.
           
           WRITE REC-F-OUTPUT FROM WS-PNT-STRING.
           WRITE REC-F-OUTPUT FROM WS-PNT-EMPTY.
           CLOSE F-OUTPUT.
       END-TABLE-HEADER.
           EXIT.

      ******************************************************************
      *    Ecris chaque ligne du tableau qui contient les valeurs qui  *
      *    correspondent au nom des colonnes.                          *
      ******************************************************************
       START-TABLE-DETAILS.
           OPEN EXTEND F-OUTPUT.

           PERFORM VARYING S-IDX FROM 1 BY 1 UNTIL S-IDX > S-CNT
              INITIALIZE WS-FULLNAME-TEMP
              INITIALIZE WS-PNT-STRING

              STRING FUNCTION TRIM(S-LASTNAME(S-IDX)) 
              SPACE FUNCTION TRIM(S-FIRSTNAME(S-IDX))
              DELIMITED BY SIZE
              INTO WS-FULLNAME-TEMP 

              STRING S-LASTNAME(S-IDX) SPACE S-FIRSTNAME(S-IDX)
              DELIMITED BY SIZE
              INTO WS-PNT-STRING(1:20) 
              
              PERFORM START-TABLE-DETAILS-C THRU END-TABLE-DETAILS-C

      *       Calcul la moyenne générale d'un élève. 
              DIVIDE S-SUM-GRADE-COEF(S-IDX) BY WS-SUM-COEF 
              GIVING S-AV-GRADE(S-IDX) ROUNDED
              
      *       Effectue la somme des moyennes générales de chaque élève.
              ADD S-AV-GRADE(S-IDX) TO WS-SUM-AV-GRADE

              INITIALIZE WS-PNT-GRADE
              MOVE S-AV-GRADE(S-IDX) TO WS-PNT-GRADE

              STRING WS-PNT-GRADE
              DELIMITED BY SIZE
              INTO WS-PNT-STRING(23:10)

              WRITE REC-F-OUTPUT FROM WS-PNT-STRING
           END-PERFORM.

           CLOSE F-OUTPUT.
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
              IF G-S-FULLNAME(G-IDX) EQUAL WS-FULLNAME-TEMP
              INITIALIZE WS-PNT-GRADE
              MOVE G-GRADE(G-IDX) TO WS-PNT-GRADE

              STRING WS-PNT-GRADE
              DELIMITED BY SIZE
              INTO WS-PNT-STRING(WS-STRING-POS:20)

      *       Effectue la somme des notes avec le coefficient de la
      *       matière pris en compte pour un élève.
              MULTIPLY G-GRADE(G-IDX) BY G-COEF(G-IDX) 
              GIVING WS-NUM-TEMP
              ADD WS-NUM-TEMP TO S-SUM-GRADE-COEF(S-IDX)

              ADD 10 TO WS-STRING-POS
              END-IF
           END-PERFORM.
       END-TABLE-DETAILS-C.
           EXIT.

      ******************************************************************
      *    Ecris la dernière du tableau qui contient la moyenne        *
      *    générale de la classe et les moyennes de la classe dans     *
      *    chaque cours.                                               *
      ******************************************************************
       START-TABLE-FOOTER.
           OPEN EXTEND F-OUTPUT.
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

               PERFORM VARYING G-IDX FROM 1 BY 1 UNTIL G-IDX > G-CNT
                   IF G-C-LABEL(G-IDX) EQUAL C-LABEL(C-IDX)
      *               Effectue la somme des notes des élèves pour une 
      *               matière.
                      ADD G-GRADE(G-IDX) TO C-SUM-GRADE(C-IDX)
                   END-IF
               END-PERFORM

      *        Calcul la moyenne de la classe dans une matiere       
               COMPUTE C-AV-GRADE(C-IDX) = C-SUM-GRADE(C-IDX) /
               (S-CNT)

               MOVE C-AV-GRADE(C-IDX) TO WS-PNT-GRADE

               STRING WS-PNT-GRADE
               DELIMITED BY SIZE
               INTO WS-PNT-STRING(WS-STRING-POS:20)

               ADD 10 TO WS-STRING-POS
           END-PERFORM.

           WRITE REC-F-OUTPUT FROM WS-PNT-EMPTY.
           WRITE REC-F-OUTPUT FROM WS-PNT-STRING.
           CLOSE F-OUTPUT.
       END-TABLE-FOOTER.
           EXIT.

      ******************************************************************
      *    Ecris le lexique de la signification de C1, C2, ...         *
      ******************************************************************
       START-LEXICON.
           OPEN EXTEND F-OUTPUT.

           WRITE REC-F-OUTPUT FROM WS-PNT-AST.

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

               WRITE REC-F-OUTPUT FROM WS-PNT-STRING
           END-PERFORM.

           CLOSE F-OUTPUT.
       END-LEXICON.
           EXIT.

      ******************************************************************
      *    Ecris le pied de page du rapport qui contient les nombres   *
      *    d'élèves, de cours et de notes et pour finir le message de  * 
      *    fin du rapport                                              *
      ******************************************************************
       START-FOOTER.
           OPEN EXTEND F-OUTPUT.

      *    Nombre d'élèves
           WRITE REC-F-OUTPUT FROM WS-PNT-AST.

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

           WRITE REC-F-OUTPUT FROM WS-PNT-STRING.

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

           WRITE REC-F-OUTPUT FROM WS-PNT-STRING.

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

           WRITE REC-F-OUTPUT FROM WS-PNT-STRING.
      
      *    Fin de rapport
           WRITE REC-F-OUTPUT FROM WS-PNT-AST.

           INITIALIZE WS-PNT-STRING.

           STRING WS-PNT-BLANK "FIN DU RAPPORT"
           DELIMITED BY SIZE
           INTO WS-PNT-STRING.

           WRITE REC-F-OUTPUT FROM WS-PNT-STRING.
           CLOSE F-OUTPUT.
       END-FOOTER.
           EXIT.
       