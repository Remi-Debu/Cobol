      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                                       *
      *    Le programme est une calculatrice simple qui permet                *
      *    d'effectuer des calculs en continue d'addtion, soustraction,       *
      *    division, multiplication et puissance. Jusqu'à ce que              *
      *    l'utilisateur décide de stopper le programme.                      *
      *                                                                       *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. calcula.
       AUTHOR.     Remi.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUM-A    PIC S9(3)V9(2).
       01  WS-NUM-B    PIC S9(3)V9(2).
       01  WS-NUM-ZA   PIC Z(2)9.99.
       01  WS-NUM-ZB   PIC Z(2)9.99.
       01  WS-OPER     PIC X.
       01  WS-RESULT   PIC S9(6)V9(2).
       01  WS-ZRESULT  PIC Z(8)9.99.
       01  WS-CONTINUE PIC A VALUE "y".
       01  WS-ITER     PIC 9(10) VALUE 0.

       PROCEDURE DIVISION.
      *    Affichage des paragraphes tant que l'utilisateur souhaite
      *    continuer
       0000-MAIN.
           PERFORM UNTIL WS-CONTINUE NOT EQUAL "y"
               PERFORM 1000-DISPLAY
               PERFORM 2000-EVALUATE
               PERFORM 1001-DISPLAY-CALCUL
               PERFORM 1002-DISPLAY-CONTINUE
           END-PERFORM.

           STOP RUN.

      *    Affichage des saisis utilisateur
       1000-DISPLAY.
           IF WS-ITER = 0
               DISPLAY "Saisir un operateur :"
               SPACE WITH NO ADVANCING
               ACCEPT WS-NUM-A
           END-IF.

           DISPLAY "Saisir un operande (+, -, x, /, ^) :"
           SPACE WITH NO ADVANCING.
           ACCEPT WS-OPER.
           MOVE FUNCTION LOWER-CASE(WS-OPER) TO WS-OPER.

           DISPLAY "Saisir un operateur :"
           SPACE WITH NO ADVANCING.
           ACCEPT WS-NUM-B.

      *    Affiche le détail du calcul
       1001-DISPLAY-CALCUL.
           MOVE WS-NUM-A  TO WS-NUM-ZA.
           MOVE WS-NUM-B  TO WS-NUM-ZB.
           MOVE WS-RESULT TO WS-ZRESULT.

           IF WS-RESULT < 0
               DISPLAY FUNCTION TRIM(WS-NUM-ZA) SPACE WS-OPER SPACE
               FUNCTION TRIM(WS-NUM-ZB) SPACE
               "= -" FUNCTION TRIM(WS-ZRESULT)
           ELSE
               DISPLAY FUNCTION TRIM(WS-NUM-ZA) SPACE WS-OPER SPACE
               FUNCTION TRIM(WS-NUM-ZB) SPACE
               "=" SPACE FUNCTION TRIM(WS-ZRESULT)
           END-IF.

       1002-DISPLAY-CONTINUE.
           DISPLAY "Continuer ? (y/n)" SPACE WITH NO ADVANCING.
               ACCEPT WS-CONTINUE.
               COMPUTE WS-ITER = 1.

      *    Condition qui affiche le paragraphe adéquat par rapport
      *    à l'opérande WS-OPER
       2000-EVALUATE.
           EVALUATE WS-OPER
               WHEN "+"
                  PERFORM 2001-ADD
               WHEN "-"
                  PERFORM 2002-SUBTRACT
               WHEN "/"
                  PERFORM 2003-DIVIDE
               WHEN "x"
                  PERFORM 2004-MULTIPLY
               WHEN "^"
                  PERFORM 2005-EXPO
               WHEN OTHER
                  DISPLAY "Error"
           END-EVALUATE.

      *    CALCUL ADDITION
       2001-ADD.
           IF WS-ITER = 1
               MOVE WS-RESULT TO WS-NUM-A
               COMPUTE WS-RESULT = WS-NUM-A + WS-NUM-B
           ELSE
               COMPUTE WS-RESULT = WS-NUM-A + WS-NUM-B
           END-IF.

      *    CALCUL SOUSTRACTION
       2002-SUBTRACT.
           IF WS-ITER = 1
               MOVE WS-RESULT TO WS-NUM-A
               COMPUTE WS-RESULT = WS-NUM-A - WS-NUM-B
           ELSE
               COMPUTE WS-RESULT = WS-NUM-A - WS-NUM-B
           END-IF.

      *    CALCUL DIVISION
       2003-DIVIDE.
           IF WS-ITER = 1
               MOVE WS-RESULT TO WS-NUM-A
               COMPUTE WS-RESULT = WS-NUM-A / WS-NUM-B
           ELSE
               COMPUTE WS-RESULT = WS-NUM-A / WS-NUM-B
           END-IF.

      *    CALCUL MULTIPLICATION
       2004-MULTIPLY.
           IF WS-ITER = 1
               MOVE WS-RESULT TO WS-NUM-A
               COMPUTE WS-RESULT = WS-NUM-A * WS-NUM-B
           ELSE
               COMPUTE WS-RESULT = WS-NUM-A * WS-NUM-B
           END-IF.

      *    CALCUL PUISSANCE
       2005-EXPO.
           IF WS-ITER = 1
               MOVE WS-RESULT TO WS-NUM-A
               COMPUTE WS-RESULT = WS-NUM-A ** WS-NUM-B
           ELSE
              COMPUTE WS-RESULT = WS-NUM-A ** WS-NUM-B
           END-IF.

