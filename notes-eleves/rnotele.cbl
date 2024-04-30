      ******************************************************************
      *    Sous programme qui s'occupe de la lecture du fichier        *
      *    "input.dat", de stocker les données dans les tableaux et    * 
      *    trier ces tableaux.                                         *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rnotele.
       AUTHOR         Rémi.

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

       WORKING-STORAGE SECTION.
       01  FS-INPUT  PIC X(02).
           88 FS-INPUT-OK  VALUE "0".
           88 FS-INPUT-EOF VALUE "10".

       01  WS-IS-EXIST      PIC X.
           88 WS-IS-EXIST-Y VALUE "Y".
           88 WS-IS-EXIST-N VALUE "N".

       LINKAGE SECTION.
       01  LK-TABLE-STUDENT.
           03  S-CNT  PIC 9(03) VALUE 1.
           03  STUDENT OCCURS 1 TO 200 TIMES
                          DEPENDING ON S-CNT
                          INDEXED BY S-IDX.
               05 S-LASTNAME       PIC X(10).
               05 S-FIRSTNAME      PIC X(10).
               05 S-AGE            PIC 9(02).
               05 S-SUM-GRADE-COEF PIC 9(05)V9(02).
               05 S-AV-GRADE       PIC 9(02)V9(02).
       
       01  LK-TABLE-COURSE.
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

       01  LK-TABLE-GRADE.
           03 G-CNT PIC 9(03) VALUE 1.
           03 GRADE OCCURS 1 TO 200 TIMES
                       DEPENDING ON G-CNT
                       INDEXED BY G-IDX.
               05 G-S-FULLNAME PIC X(20).
               05 G-C-LABEL    PIC X(25).
               05 G-COEF       PIC 9V9.
               05 G-GRADE      PIC 9(02)V9(02).
       

      ****************************************************************** 
       PROCEDURE DIVISION USING LK-TABLE-STUDENT, LK-TABLE-COURSE, 
           LK-TABLE-GRADE.

       START-MAIN.
           PERFORM START-R-IP THRU END-R-IP.
           PERFORM START-SORT THRU END-SORT.
       END-MAIN.
           GOBACK.

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
           