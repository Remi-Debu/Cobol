      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                                       *
      *    Le programme demande à l'utilisateur de saisir 2 nombres et        *
      *    additionne ces 2 nombres pour ensuite afficher le calcul et        *
      *    son resultat.                                                      *
      *                                                                       *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. addition.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUM-1  PIC 9(10)V9(2).
       01  WS-NUM-2  PIC 9(10)V9(2).
       01  WS-RESULT PIC 9(11)V9(2).

       PROCEDURE DIVISION.
           DISPLAY "Saisir un premier nombre : " ACCEPT WS-NUM-1.
           DISPLAY "Saisir un deuxieme nombre : " ACCEPT WS-NUM-2.

           ADD WS-NUM-1 TO WS-NUM-2 GIVING WS-RESULT.

           DISPLAY WS-NUM-1 " + " WS-NUM-2 " = " WS-RESULT.

           STOP RUN.
