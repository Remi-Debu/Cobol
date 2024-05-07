      ******************************************************************
      *    Le programme demande à l'utilisateur de saisir la position  *
      *    des reines blanche et noire. Puis indique si elles peuvent  *
      *    s'attaquer.                                                 *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. dame.
       AUTHOR      Rémi.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  CHECK-WQ-INPUT PIC X(02) VALUE "NO".
           88 WQ-OK VALUE "OK".
           88 WQ-NO VALUE "NO".

       01  CHECK-BQ-INPUT PIC X(02) VALUE "NO".
           88 BQ-OK VALUE "OK".
           88 BQ-NO VALUE "NO".

       01  WHITE-QUEEN.
           03 WQ-POS    PIC X(10).
           03 WQ-COLUMN PIC 9.
           03 WQ-ROW    PIC 9(09).

       01  BLACK-QUEEN.
           03 BQ-POS    PIC X(10).
           03 BQ-COLUMN PIC 9.
           03 BQ-ROW    PIC 9(09).

       01  WS-ALPHABET PIC X(26) VALUE "ABCDEFGH".
       01  WS-AST      PIC X(50) VALUE ALL "*".
       01  A-IDX       PIC 9(02).
      ****************************************************************** 
       PROCEDURE DIVISION.
      ******************************************************************
       START-MAIN.
           PERFORM START-HEADER THRU END-HEADER.
           PERFORM START-USER-INPUT-WQ THRU END-USER-INPUT-WQ.
           PERFORM START-USER-INPUT-BQ THRU END-USER-INPUT-BQ.
           PERFORM START-CHECK-QUEEN-ATTACK THRU END-CHECK-QUEEN-ATTACK.
       END-MAIN.
           STOP RUN.

      ******************************************************************
      *    Affiche à l'utilisateur les instructions à suivre pour      *
      *    utiliser le programme.                                      *
      ******************************************************************
       START-HEADER.
           DISPLAY WS-AST.
           DISPLAY "LA DAME ATTAQUE !".
           DISPLAY WS-AST.
           DISPLAY SPACE.
           DISPLAY "L'echiquier est represente par un tableau de 8"
           SPACE "par 8.".
           DISPLAY "Les colonnes vont de A a H.".
           DISPLAY "Les lignes vont de 1 a 8.".
           DISPLAY "Veuillez renseigner les positions des reines" 
           SPACE "sur l'echiquier.".
           DISPLAY "Exemple de position : B4".
           DISPLAY SPACE.
       END-HEADER.
           EXIT.

      ******************************************************************
      *    Demande à l'utilisateur de saisir les positions de la reine *
      *    blanche et appel le paragraphe CHECK-HANDLE-INPUT.          *
      ******************************************************************
       START-USER-INPUT-WQ.
           INITIALIZE WQ-ROW.
           DISPLAY "Position de la reine blanche :" 
           SPACE WITH NO ADVANCING.
           ACCEPT WQ-POS.

           PERFORM START-CHECK-HANDLE-INPUT THRU END-CHECK-HANDLE-INPUT.
           
           IF WQ-NO
              DISPLAY "Saisie incorrecte."
              GO TO START-USER-INPUT-WQ
           END-IF.
       END-USER-INPUT-WQ.
           EXIT.

      ******************************************************************
      *    Demande à l'utilisateur de saisir les positions de la reine *
      *    noire et appel le paragraphe CHECK-HANDLE-INPUT.            *
      ******************************************************************
       START-USER-INPUT-BQ.
           INITIALIZE BQ-ROW.
           DISPLAY "Position de la reine noire :" 
           SPACE WITH NO ADVANCING.
           ACCEPT BQ-POS.

           PERFORM START-CHECK-HANDLE-INPUT THRU END-CHECK-HANDLE-INPUT.
           
           IF BQ-NO
              DISPLAY "Saisie incorrecte."
              GO TO START-USER-INPUT-BQ
           END-IF.
       END-USER-INPUT-BQ.
           EXIT.

      ******************************************************************
      *    Vérifie si la saisie de l'utilisateur est correct, si oui   *
      *    insére le numéro de colonne et de ligne en fonction de la   *
      *    position renseignée par l'utilisateur.                      *
      ******************************************************************
       START-CHECK-HANDLE-INPUT.
           INITIALIZE WQ-ROW.
           INITIALIZE BQ-ROW.
           MOVE WQ-POS(2:9) TO WQ-ROW.
           MOVE BQ-POS(2:9) TO BQ-ROW.

           SET WQ-NO TO TRUE.
           SET BQ-NO TO TRUE.

           PERFORM VARYING A-IDX FROM 1 BY 1 UNTIL A-IDX > 8
              IF WS-ALPHABET(A-IDX:1) EQUAL WQ-POS(1:1)
                 AND WQ-ROW > 0
                 AND WQ-ROW < 9

                 MOVE A-IDX TO WQ-COLUMN
                 MOVE WQ-POS(2:1) TO WQ-ROW
                 SET WQ-OK TO TRUE
              END-IF
              
              IF WS-ALPHABET(A-IDX:1) EQUAL BQ-POS(1:1)
                 AND BQ-ROW > 0
                 AND BQ-ROW < 9

                 MOVE A-IDX TO BQ-COLUMN
                 MOVE BQ-POS(2:1) TO BQ-ROW
                 SET BQ-OK TO TRUE
              END-IF
           END-PERFORM.
       END-CHECK-HANDLE-INPUT.
           EXIT.

      ******************************************************************
      *    Affiche si les reines peuvent s'attaquer ou non, en fonction*
      *    de si elles sont sur la même colonne, ligne ou diagonale.   *
      ******************************************************************
       START-CHECK-QUEEN-ATTACK.
           DISPLAY SPACE.
           IF WQ-COLUMN EQUAL BQ-COLUMN
              OR WQ-ROW EQUAl BQ-ROW 
              OR WQ-COLUMN - BQ-COLUMN = WQ-ROW - BQ-ROW
           
              DISPLAY "Les reines peuvent s'attaquer !"
           ELSE
              DISPLAY "Les reines ne peuvent pas s'attaquer."
           END-IF.
       END-CHECK-QUEEN-ATTACK.
    