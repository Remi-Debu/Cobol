      ******************************************************************
      *    Le programme lis un fichier client puis affiche la liste    *
      *    des salaires des clients ainsi que le plus grand et le plus *
      *    petit salaire.                                              *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. flclmm.
       AUTHOR        Rémi.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-CLIENT ASSIGN TO "fichierclient.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-CLIENT.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
       COPY FCLIENT REPLACING ==:CLIENT:== BY ==CLIENT==.

       WORKING-STORAGE SECTION.
       01  FS-CLIENT PIC X(02).
           88 FS-CLIENT-OK VALUE "00".
           88 FS-CLIENT-EOF VALUE "10".
       
       01  TABLE-CLIENT.
           03  C-CNT PIC 9(04) VALUE 1.
           03  CLIENT OCCURS 1 TO 1000
                      DEPENDING ON C-CNT
                      INDEXED BY C-IDX.
	          05 C-ID      PIC X(09).
              05 C-NOM     PIC X(20).
              05 C-PRENOM  PIC X(20).
              05 C-POSTE   PIC X(14).
              05 C-CODE    PIC X(03).
              05 C-SALAIRE PIC 9(04)V99.
              05 C-AGENCE  PIC X(05).
       
       01  Z-IDX PIC Z(10)9.
       01  WS-SALAIRE-MAX PIC 9(04)V99 VALUE 0.
       01  WS-SALAIRE-MIN PIC 9(04)V99 VALUE 9999.99.

      ****************************************************************** 
       PROCEDURE DIVISION.
      ******************************************************************
       START-MAIN.
           PERFORM START-R-IP THRU END-R-IP.
           PERFORM START-DISPLAY-CLIENT THRU END-DISPLAY-CLIENT.
       END-MAIN.
           STOP RUN.

      ******************************************************************
      *    Lis le fichier client et appel le paragraphe HANDLE-CLIENT. *
      ******************************************************************
       START-R-IP.
           OPEN INPUT F-CLIENT.
           IF FS-CLIENT EQUAL "00"
              SET FS-CLIENT-OK TO TRUE

              PERFORM UNTIL FS-CLIENT-EOF
                 READ F-CLIENT 
                 AT END 
                    SUBTRACT 1 FROM C-CNT
                    SET FS-CLIENT-EOF TO TRUE
                 NOT AT END 
                    PERFORM START-HANDLE-CLIENT THRU END-HANDLE-CLIENT
                 END-READ
              END-PERFORM
           ELSE
              DISPLAY "ERREUR :" SPACE FS-CLIENT
           END-IF.
           CLOSE F-CLIENT.
       END-R-IP.
           EXIT.

      ******************************************************************
      *    Stock les données des clients dans le tableau client de la  *
      *    WS. Stock également le plus grand et le plus petit des      *
      *    salaires des clients dans la WS.                            *
      ******************************************************************
       START-HANDLE-CLIENT.
           MOVE RCLIENT TO CLIENT(C-CNT).

      *    Si le salaire lu est + grand que le salaire max
      *    alors remplace sa valeur
           IF C-SALAIRE(C-CNT) > WS-SALAIRE-MAX
              INITIALIZE WS-SALAIRE-MAX
              MOVE C-SALAIRE(C-CNT) TO WS-SALAIRE-MAX
           END-IF.

      *    Si le salaire lu est + petit que le salaire min
      *    alors remplace sa valeur
           IF C-SALAIRE(C-CNT) < WS-SALAIRE-MIN
              INITIALIZE WS-SALAIRE-MIN
              MOVE C-SALAIRE(C-CNT) TO WS-SALAIRE-MIN
           END-IF.

           ADD 1 TO C-CNT.
       END-HANDLE-CLIENT.
           EXIT.

      ******************************************************************
      *    Affiche la liste des salaires ainsi que le plus grand et le *
      *    plus petit salaire.                                         *
      ******************************************************************
       START-DISPLAY-CLIENT.
           DISPLAY "Liste des salaires :".
           DISPLAY SPACE.
           PERFORM VARYING C-IDX FROM 1 BY 1 UNTIL C-IDX > C-CNT
           INITIALIZE Z-IDX
           MOVE C-IDX TO Z-IDX
           DISPLAY FUNCTION TRIM(Z-IDX) ":" SPACE C-SALAIRE(C-IDX)
           END-PERFORM.
           DISPLAY SPACE.
           DISPLAY "Le plus grand salaire est de" SPACE WS-SALAIRE-MAX.
           DISPLAY "Le plus petit salaire est de" SPACE WS-SALAIRE-MIN.  
       END-DISPLAY-CLIENT.
           EXIT.
           