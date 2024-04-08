      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                                       *
      *    Le programme demande à l'utilisateur de saisir 2 nombres puis      *
      *    calcul et affiche le PGCD (Plus Grand Commun Diviseur).            *
      *                                                                       *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. pgcd.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUM-A PIC 9(5).
       01  WS-NUM-B PIC 9(5).
       01  WS-QUOT  PIC 9(5).
       01  WS-REST  PIC 9(5).
       01  WS-TEMP  PIC 9(5).

       PROCEDURE DIVISION.
           DISPLAY "Saisir un nombre :" SPACE
           NO ADVANCING
           ACCEPT WS-NUM-A.

           DISPLAY "Saisir un autre nombre :" SPACE
           NO ADVANCING.
           ACCEPT WS-NUM-B.

      *    Inverse les valeurs WS-NUM-A et WS-NUM-B
      *    si WS-NUM-A est plus petit que WS-NUM-B
           IF WS-NUM-A < WS-NUM-B
               MOVE WS-NUM-A TO WS-TEMP
               MOVE WS-NUM-B TO WS-NUM-A
               MOVE WS-TEMP  TO WS-NUM-B
           END-IF.

           DIVIDE    WS-NUM-A
           BY        WS-NUM-B
           GIVING    WS-QUOT
           REMAINDER WS-REST.

      *    Boucle tant que le reste n'est pas égal à 0
      *    remplace les valeurs WS-NUM-A et WS-NUM-B
      *    par les valeurs WS-NUM-B et WS-REST
      *    pour ensuite refaire la division
           PERFORM UNTIL WS-REST <= 0
               MOVE WS-NUM-B TO WS-NUM-A
               MOVE WS-REST  TO WS-NUM-B

               DIVIDE    WS-NUM-A
               BY        WS-NUM-B
               GIVING    WS-QUOT
               REMAINDER WS-REST
           END-PERFORM

           DISPLAY "PGCD =" SPACE WS-NUM-B.

           STOP RUN.
