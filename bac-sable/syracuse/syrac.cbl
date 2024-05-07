      ******************************************************************
      *    Le programme effectue la conjecture de Syracuse à partir    *
      *    d'un nombre entier non nul saisi par l'utilisateur.         *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
      ****************************************************************** 
       PROGRAM-ID. syrac.
       AUTHOR       Rémi.

      ******************************************************************
       DATA DIVISION.
      ****************************************************************** 
       WORKING-STORAGE SECTION.
       01  WS-NUM    PIC 9(05).
       01  WS-Z-NUM  PIC Z(04)9.
       01  WS-RESULT PIC 9(10).
       01  WS-REST   PIC 9(10).
           

      ****************************************************************** 
       PROCEDURE DIVISION.
      ******************************************************************
           DISPLAY "Saisir un nombre entier :" SPACE WITH NO ADVANCING.
           ACCEPT WS-NUM.
           DISPLAY SPACE.
           DISPLAY "Details des calculs effectues :".

           PERFORM UNTIL WS-NUM = 1
           DIVIDE WS-NUM BY 2 GIVING WS-RESULT REMAINDER WS-REST

      *    Si nombre est pair
           IF WS-REST EQUAL 0 
              INITIALIZE WS-Z-NUM
              MOVE WS-NUM TO WS-Z-NUM
              DISPLAY FUNCTION TRIM(WS-Z-NUM) SPACE "/ 2 =" 
              SPACE WITH NO ADVANCING

              DIVIDE WS-NUM BY 2 GIVING WS-NUM
              INITIALIZE WS-Z-NUM
              MOVE WS-NUM TO WS-Z-NUM
              DISPLAY FUNCTION TRIM(WS-Z-NUM)

      *    Si nombre impair
           ELSE
              INITIALIZE WS-Z-NUM
              MOVE WS-NUM TO WS-Z-NUM
              DISPLAY FUNCTION TRIM(WS-Z-NUM) SPACE "x 3 + 1 =" 
              SPACE WITH NO ADVANCING

              MULTIPLY WS-NUM BY 3 GIVING WS-NUM
              ADD 1 TO WS-NUM
              INITIALIZE WS-Z-NUM
              MOVE WS-NUM TO WS-Z-NUM
              DISPLAY FUNCTION TRIM(WS-Z-NUM)
           END-IF

      *    DISPLAY WS-NUM
           END-PERFORM.

           STOP RUN.

           