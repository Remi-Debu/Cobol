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
       01  WS-NUM PIC 9(05).
       01  WS-RESULT PIC 9(10).
       01  WS-REST PIC 9(10).
           

      ****************************************************************** 
       PROCEDURE DIVISION.
      ******************************************************************
           DISPLAY "Saisir un nombre entier :" SPACE WITH NO ADVANCING.
           ACCEPT WS-NUM.
           DISPLAY SPACE.

           PERFORM UNTIL WS-NUM = 1
           DIVIDE WS-NUM BY 2 GIVING WS-RESULT REMAINDER WS-REST

      *    Si nombre est pair
           IF WS-REST EQUAL 0 
              DISPLAY WS-NUM SPACE "/ 2 =" SPACE WITH NO ADVANCING
              DIVIDE WS-NUM BY 2 GIVING WS-NUM
              DISPLAY WS-NUM

      *    Si nombre impair
           ELSE
              DISPLAY WS-NUM SPACE "x 3 + 1 =" SPACE WITH NO ADVANCING
              MULTIPLY WS-NUM BY 3 GIVING WS-NUM
              ADD 1 TO WS-NUM
              DISPLAY WS-NUM
           END-IF

      *    DISPLAY WS-NUM
           END-PERFORM.

           STOP RUN.

           