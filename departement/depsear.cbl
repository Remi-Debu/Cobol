      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. depsear.
       AUTHOR         Rémi.

      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT-DEPT ASSIGN TO "departement.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-I-DEPT.

           SELECT F-OUTPUT-DEPT ASSIGN TO "sort-departement.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-O-DEPT.

           SELECT F-WORK-DEPT ASSIGN TO "work-departement.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-W-DEPT.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  F-INPUT-DEPT
           RECORD CONTAINS 52 CHARACTERS
           RECORDING MODE IS F.
       01  R-I-DEPT.
           03 R-I-D-NUM    PIC 9(03).
           03 R-I-D-NAME   PIC X(23).
           03 R-I-D-REGION PIC X(26).

       FD  F-OUTPUT-DEPT
           RECORD CONTAINS 52 CHARACTERS
           RECORDING MODE IS F.
       01  R-O-DEPT PIC X(52).

       SD  F-WORK-DEPT
           RECORD CONTAINS 52 CHARACTERS
           RECORDING MODE IS F.
       01  R-W-DEPT.
           03 R-W-D-NUM    PIC 9(03).
           03 R-W-D-NAME   PIC X(23).
           03 R-W-D-REGION PIC X(26).

       WORKING-STORAGE SECTION.
       01  FS-I-DEPT PIC X(02).
           88 FS-I-DEPT-OK  VALUE "00".
           88 FS-I-DEPT-EOF VALUE "10".

       01  FS-O-DEPT PIC X(02).
           88 FS-O-DEPT-OK  VALUE "00".
           88 FS-O-DEPT-EOF VALUE "10".

       01  FS-W-DEPT PIC X(02).
           88 FS-W-DEPT-OK  VALUE "00".
           88 FS-W-DEPT-EOF VALUE "10".

       01  TABLE-DEPT.
           03  D-CNT PIC 9(04) VALUE 1.
           03  DEPT OCCURS 1 TO 1000 TIMES
                    DEPENDING ON D-CNT
                    ASCENDING KEY D-NUM
                    INDEXED BY D-IDX.
               05 D-NUM    PIC 9(03).
               05 D-NAME   PIC X(30).
               05 D-REGION PIC X(30).
    
       01  USER-INPUT.
           03 UI-D-NUM    PIC 9(03).
           03 UI-D-NAME   PIC X(30).
           03 UI-D-REGION PIC 9(03).

      ****************************************************************** 
       PROCEDURE DIVISION.
       START-MAIN.
           SORT F-WORK-DEPT 
           ON ASCENDING KEY R-W-D-NAME
           INPUT PROCEDURE IS START-R-DEPT THRU END-R-DEPT
           OUTPUT PROCEDURE IS START-W-DEPT THRU END-W-DEPT.
           
           PERFORM START-UI-SEARCH THRU END-UI-SEARCH.
       END-MAIN.
           STOP RUN.
            
      ******************************************************************
      *    Lis le fichier.
      ******************************************************************
       START-R-DEPT.
           OPEN INPUT F-INPUT-DEPT.
           IF FS-I-DEPT EQUAL "00"
              SET FS-I-DEPT-OK TO TRUE

              PERFORM UNTIL FS-I-DEPT-EOF
                 READ F-INPUT-DEPT 
                 AT END 
                    SUBTRACT 1 FROM D-CNT
                    DISPLAY D-CNT
                    SET FS-I-DEPT-EOF TO TRUE
                 NOT AT END 
                    PERFORM START-HANDLE-DEPT THRU END-HANDLE-DEPT
                  END-READ
              END-PERFORM
           ELSE
              DISPLAY "ERREUR :" SPACE FS-I-DEPT
           END-IF.
           CLOSE F-INPUT-DEPT.
       END-R-DEPT.
           EXIT.

      ******************************************************************
      *    Stock les données lus dans la table de la WS.               *
      ******************************************************************
       START-HANDLE-DEPT.
           MOVE R-I-D-NUM    TO D-NUM(D-CNT).
           MOVE R-I-D-NAME   TO D-NAME(D-CNT).
           MOVE R-I-D-REGION TO D-REGION(D-CNT).

           ADD 1 TO D-CNT.
       END-HANDLE-DEPT.
           EXIT.

      ******************************************************************
      *    Trie et écris le fichier.
      ******************************************************************
       START-W-DEPT.
           SORT DEPT ASCENDING KEY D-NAME.

           OPEN OUTPUT F-OUTPUT-DEPT.

           PERFORM VARYING D-IDX FROM 1 BY 1 UNTIL D-IDX > D-CNT
               WRITE R-O-DEPT FROM D-NAME(D-IDX)
           END-PERFORM.

           CLOSE F-OUTPUT-DEPT.
       END-W-DEPT.
           EXIT.


      ******************************************************************
      *    Recherche par rapport au numéro de département saisi par    *
      *    l'utilisateur, le département correspondant dans le tableau *
      *    des départements.                                           *
      ******************************************************************
       START-UI-SEARCH.
           DISPLAY "Entrer un numero de departement :" 
           SPACE WITH NO ADVANCING.
           ACCEPT UI-D-NUM.

           SEARCH DEPT VARYING D-IDX
               WHEN D-NUM(D-IDX) EQUAL UI-D-NUM
                   DISPLAY "SEARCH"
                   DISPLAY FUNCTION TRIM(D-NAME(D-IDX)) 
                   SPACE "-" SPACE 
                   FUNCTION TRIM(D-REGION(D-IDX))
           END-SEARCH.

           SEARCH ALL DEPT
               WHEN D-NUM(D-IDX) EQUAL UI-D-NUM
                   DISPLAY "SEARCH ALL"
                   DISPLAY FUNCTION TRIM(D-NAME(D-IDX)) 
                   SPACE "-" SPACE 
                   FUNCTION TRIM(D-REGION(D-IDX))
           END-SEARCH.
       END-UI-SEARCH.
           EXIT.

           