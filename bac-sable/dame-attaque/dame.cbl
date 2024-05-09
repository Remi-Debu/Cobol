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
       01  CHECK-WQ-INPUT PIC X(03).
           88 WQ-OK      VALUE "OK".
           88 WQ-ERR-ROW VALUE "ROW".
           88 WQ-ERR-COL VALUE "COL".

       01  CHECK-BQ-INPUT PIC X(03).
           88 BQ-OK      VALUE "OK".
           88 BQ-ERR-ROW VALUE "ROW".
           88 BQ-ERR-COL VALUE "COL".
           88 BQ-ERR-EQU VALUE "EQU".

       01  CHESSBOARD.
           03 TABLE-ROW OCCURS 8 TIMES.
              05 FILLER PIC XX VALUE "| ".
              05 CB-ROW-NUM PIC 9.
              05 FILLER PIC XXX VALUE " | ".
              05 TABLE-COLUMN OCCURS 8 TIMES.
                 07 CB-SQUARE PIC X.
                 07 FILLER PIC XXX VALUE " | ".

       01  R-CNT PIC 9.
       01  C-CNT PIC 9.

       01  WHITE-QUEEN.
           03 WQ-POS    PIC X(10).
           03 WQ-COLUMN PIC 9.
           03 WQ-ROW    PIC 9(09).

       01  BLACK-QUEEN.
           03 BQ-POS    PIC X(10).
           03 BQ-COLUMN PIC 9.
           03 BQ-ROW    PIC 9(09).

       01  WS-ALPHABET PIC X(26) VALUE "ABCDEFGH".
       01  A-IDX       PIC 9.

       01  START-POS PIC 9(03).
       01  PNT-CHESSBOARD PIC X(100).
       01  WS-BLANK    PIC X(15) VALUE ALL SPACES.
       01  WS-AST      PIC X(50) VALUE ALL "*".
       01  WS-DASH     PIC X(37) VALUE ALL "-".
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
           DISPLAY WS-BLANK "LA DAME ATTAQUE !".
           DISPLAY WS-AST.
           PERFORM START-PRINT-CHESSBOARD THRU END-PRINT-CHESSBOARD.
       END-HEADER.
           EXIT.

      ******************************************************************
      *    Affiche l'échiquier.                                        *
      ******************************************************************
       START-PRINT-CHESSBOARD.
           DISPLAY SPACE.
      *    Affiche la 1ere ligne du chessboard (les noms de colonnes)
           STRING "|   |" SPACE DELIMITED BY SIZE INTO PNT-CHESSBOARD.
           INITIALIZE A-IDX.
           SET START-POS TO 7.
           PERFORM 8 TIMES 
              ADD 1 TO A-IDX

              STRING WS-ALPHABET(A-IDX :1) SPACE "|" 
              DELIMITED BY SIZE
              INTO PNT-CHESSBOARD(START-POS:4)

              ADD 4 TO START-POS
           END-PERFORM.
           DISPLAY WS-DASH.
           DISPLAY PNT-CHESSBOARD.

      *    Affiche le reste du chessboard
           INITIALIZE R-CNT.
           PERFORM 8 TIMES
              INITIALIZE PNT-CHESSBOARD
              ADD 1 TO R-CNT 

      *       Numéro de la ligne
              SET CB-ROW-NUM(R-CNT) TO R-CNT
              STRING TABLE-ROW(R-CNT) 
              DELIMITED BY SIZE
              INTO PNT-CHESSBOARD

      *       Affiche le reste des cases du chessboard
              INITIALIZE C-CNT
              SET START-POS TO 7
              PERFORM 8 TIMES 
              ADD 1 TO C-CNT

              STRING TABLE-COLUMN(R-CNT, C-CNT)
              DELIMITED BY SIZE
              INTO PNT-CHESSBOARD(START-POS:4)

              ADD 4 TO START-POS
              END-PERFORM
           
           DISPLAY WS-DASH
           DISPLAY PNT-CHESSBOARD
           END-PERFORM.
           DISPLAY WS-DASH.
           DISPLAY SPACE.
       END-PRINT-CHESSBOARD.
           EXIT.

      ******************************************************************
      *    Demande à l'utilisateur de saisir les positions de la reine *
      *    blanche et appel le paragraphe CHECK-HANDLE-INPUT.          *
      ******************************************************************
       START-USER-INPUT-WQ.
           INITIALIZE WQ-ROW.
           INITIALIZE WQ-COLUMN.
           INITIALIZE WQ-POS.
           DISPLAY "Position de la reine blanche :" 
           SPACE WITH NO ADVANCING.
           ACCEPT WQ-POS.

           PERFORM START-CHECK-HANDLE-WQ THRU END-CHECK-HANDLE-WQ.

           EVALUATE CHECK-WQ-INPUT
               WHEN "COL"
                  DISPLAY "Colonne de la reine blanche incorrecte."
                  GO TO START-USER-INPUT-WQ
               WHEN "ROW"
                  DISPLAY "Ligne de la reine blanche incorrecte."
                  GO TO START-USER-INPUT-WQ
              WHEN "OK"
                  CONTINUE
               WHEN OTHER
                  DISPLAY "Erreur imprévue."
                  GO TO START-USER-INPUT-WQ
           END-EVALUATE.
       END-USER-INPUT-WQ.
           EXIT.

      ******************************************************************
      *    Demande à l'utilisateur de saisir les positions de la reine *
      *    noire et appel le paragraphe CHECK-HANDLE-INPUT.            *
      ******************************************************************
       START-USER-INPUT-BQ.
           PERFORM START-PRINT-CHESSBOARD THRU END-PRINT-CHESSBOARD.

           INITIALIZE BQ-ROW.
           INITIALIZE BQ-COLUMN.
           INITIALIZE BQ-POS.
           DISPLAY SPACE.
           DISPLAY "Position de la reine noire :" 
           SPACE WITH NO ADVANCING.
           ACCEPT BQ-POS.

           PERFORM START-CHECK-HANDLE-BQ THRU END-CHECK-HANDLE-BQ.

           EVALUATE CHECK-BQ-INPUT
               WHEN "COL"
                  DISPLAY "Colonne de la reine noire incorrecte."
                  GO TO START-USER-INPUT-BQ
               WHEN "ROW"
                  DISPLAY "Ligne de la reine noire incorrecte."
                  GO TO START-USER-INPUT-BQ
               WHEN "EQU"
                  DISPLAY "La reine blanche est sur cette case."
                  GO TO START-USER-INPUT-BQ
               WHEN "OK"
                  CONTINUE
               WHEN OTHER
                  DISPLAY "Erreur imprévue."
                  GO TO START-USER-INPUT-BQ
           END-EVALUATE.
       END-USER-INPUT-BQ.
           EXIT.

      ******************************************************************
      *    Vérifie si la position de la reine blanche est correct,     *
      *    si oui insère le numéro de colonne et de ligne en fonction  *
      *    de la position renseignée par l'utilisateur.                *
      ******************************************************************
       START-CHECK-HANDLE-WQ.
           INITIALIZE CHECK-WQ-INPUT.
           INITIALIZE A-IDX.
           INITIALIZE WQ-ROW.

           MOVE WQ-POS(2:9) TO WQ-ROW.

           IF WQ-ROW > 0 AND WQ-ROW < 9 
              PERFORM VARYING A-IDX FROM 1 BY 1 UNTIL A-IDX > 8
                 IF WS-ALPHABET(A-IDX:1) EQUAL WQ-POS(1:1)
                    MOVE A-IDX TO WQ-COLUMN
                    MOVE WQ-POS(2:1) TO WQ-ROW
                    MOVE "W" TO CB-SQUARE(WQ-ROW, WQ-COLUMN)
                    SET WQ-OK TO TRUE
                    GO TO END-CHECK-HANDLE-WQ
                 ELSE
                    SET WQ-ERR-COL TO TRUE 
                 END-IF
              END-PERFORM
           ELSE 
              SET WQ-ERR-ROW TO TRUE
           END-IF.
       END-CHECK-HANDLE-WQ.
           EXIT.
              
      ******************************************************************
      *    Vérifie si la position de la reine noire est correct,       *
      *    si oui insère le numéro de colonne et de ligne en fonction  *
      *    de la position renseignée par l'utilisateur.                *
      ******************************************************************
       START-CHECK-HANDLE-BQ.
           INITIALIZE CHECK-WQ-INPUT.
           INITIALIZE A-IDX.
           INITIALIZE BQ-ROW.

           MOVE BQ-POS(2:9) TO BQ-ROW.

           IF BQ-ROW > 0 AND BQ-ROW < 9 
              PERFORM VARYING A-IDX FROM 1 BY 1 UNTIL A-IDX > 8
                 IF WS-ALPHABET(A-IDX:1) EQUAL BQ-POS(1:1)
                    MOVE A-IDX TO BQ-COLUMN
                    MOVE BQ-POS(2:1) TO BQ-ROW
                       IF BQ-ROW EQUAL WQ-ROW 
                          AND BQ-COLUMN EQUAL WQ-COLUMN
                          SET BQ-ERR-EQU TO TRUE
                          GO TO END-CHECK-HANDLE-BQ
                       ELSE
                          MOVE "B" TO CB-SQUARE(BQ-ROW, BQ-COLUMN)
                          SET BQ-OK TO TRUE
                          GO TO END-CHECK-HANDLE-BQ
                       END-IF
                 ELSE
                    SET BQ-ERR-COL TO TRUE 
                 END-IF
              END-PERFORM
           ELSE 
              SET BQ-ERR-ROW TO TRUE
           END-IF.
       END-CHECK-HANDLE-BQ.
           EXIT.

      ******************************************************************
      *    Affiche si les reines peuvent s'attaquer ou non, en fonction*
      *    de si elles sont sur la même colonne, ligne ou diagonale.   *
      ******************************************************************
       START-CHECK-QUEEN-ATTACK.
           PERFORM START-PRINT-CHESSBOARD THRU END-PRINT-CHESSBOARD.

           IF WQ-COLUMN EQUAL BQ-COLUMN
              OR WQ-ROW EQUAl BQ-ROW 
              OR WQ-COLUMN - BQ-COLUMN = WQ-ROW - BQ-ROW
           
              DISPLAY "Les reines peuvent s'attaquer !"
           ELSE
              DISPLAY "Les reines ne peuvent pas s'attaquer."
           END-IF.
       END-CHECK-QUEEN-ATTACK.
    