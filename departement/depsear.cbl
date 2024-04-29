      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. depsear.
       AUTHOR         Rémi.

      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-DEPT ASSIGN TO "departement.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-DEPT.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  F-DEPT
           RECORD CONTAINS 52 CHARACTERS
           RECORDING MODE IS F.
       01  R-DEPT.
           03 R-D-NUM    PIC 9(03).
           03 R-D-NAME   PIC X(23).
           03 R-D-REGION PIC X(26).

       WORKING-STORAGE SECTION.
       01  FS-DEPT PIC X(02).
           88 FS-DEPT-OK VALUE "00".
           88 FS-DEPT-EOF VALUE "10".

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
           PERFORM START-R-DEPT THRU END-R-DEPT.
           PERFORM START-UI-SEARCH THRU END-UI-SEARCH.
       END-MAIN.
           GOBACK.
            
      ******************************************************************
      *    Lis le fichier.
      ******************************************************************
       START-R-DEPT.
           OPEN INPUT F-DEPT.
           IF FS-DEPT EQUAL "00"
              SET FS-DEPT-OK TO TRUE

              PERFORM UNTIL FS-DEPT-EOF
                 READ F-DEPT 
                 AT END 
                    SET FS-DEPT-EOF TO TRUE
                 NOT AT END 
                    PERFORM START-HANDLE-DEPT THRU END-HANDLE-DEPT
                  END-READ
              END-PERFORM
           ELSE
              DISPLAY "ERREUR :" SPACE FS-DEPT
           END-IF.
           CLOSE F-DEPT.
       END-R-DEPT.
           EXIT.

      ******************************************************************
      *    Stock les données lus dans la table de la WS.               *
      ******************************************************************
       START-HANDLE-DEPT.
           MOVE R-D-NUM    TO D-NUM(D-CNT).
           MOVE R-D-NAME   TO D-NAME(D-CNT).
           MOVE R-D-REGION TO D-REGION(D-CNT).

           ADD 1 TO D-CNT.
       END-HANDLE-DEPT.
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

           