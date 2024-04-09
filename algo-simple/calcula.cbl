      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                                       *
      *    Le programme est une calculatrice.
      *                                                                       *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. calcula.
       AUTHOR.     Remi.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUM-A            PIC S9(10)V9(2).
       01  WS-NUM-B            PIC S9(10)V9(2).
       01  WS-OPER             PIC X.
       01  WS-RESULT           PIC S9(11)V9(2).

       PROCEDURE DIVISION.
           DISPLAY "Saisir le premier operateur :" 
           SPACE WITH NO ADVANCING.
           ACCEPT WS-NUM-A.

           DISPLAY "Saisir l'operande :"
           SPACE WITH NO ADVANCING.
           ACCEPT WS-OPER.
           MOVE FUNCTION LOWER-CASE(WS-OPER) TO WS-OPER.

           DISPLAY "Saisir le deuxiemen operateur :" 
           SPACE WITH NO ADVANCING.
           ACCEPT WS-NUM-B.

           EVALUATE WS-OPER
               WHEN "+"
                  COMPUTE WS-RESULT = WS-NUM-A + WS-NUM-B
               WHEN "-"
                  COMPUTE WS-RESULT = WS-NUM-A - WS-NUM-B
               WHEN "/"
                  COMPUTE WS-RESULT = WS-NUM-A / WS-NUM-B
               WHEN "x"
                  COMPUTE WS-RESULT = WS-NUM-A * WS-NUM-B
           END-EVALUATE.

           DISPLAY WS-NUM-A SPACE WS-OPER SPACE WS-NUM-B SPACE 
           "=" SPACE WS-RESULT

           STOP RUN.
