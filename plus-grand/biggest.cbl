      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                                       *
      *    Le programme demande à l'utilisateur de saisir 3 nombres puis      *
      *    affiche le nombre le plus grand.                                   *
      *                                                                       *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. biggest.
       AUTHOR. Remi Debusschere.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TABLE.
           05 WS-NUM      PIC S9(5)V9(2) OCCURS 3 TIMES.
       01  WS-I           PIC 9 VALUE 1.
       01  WS-NUM-BIGGEST PIC S9(5)V9(2).

       PROCEDURE DIVISION.
           PERFORM 3 TIMES

      *    Stocke la saisie utilisateur dans la table à l'index WS-I.
           DISPLAY "Saisir un nombre : "
           NO ADVANCING ACCEPT WS-NUM(WS-I)

      *    Donne à WS-NUM-BIGGEST la valeur du premier nombre saisi.
           IF WS-I = 1
               MOVE WS-NUM(1) TO WS-NUM-BIGGEST
           END-IF

      *    Acutalise la valeur de WS-NUM-BIGGEST
      *    si WS-NUM(WS-I) est plus grand que l'ancienne valeur de
      *    WS-NUM-BIGGEST.
           IF WS-NUM(WS-I) > WS-NUM-BIGGEST
              MOVE WS-NUM(WS-I) TO WS-NUM-BIGGEST
           END-IF

      *    Incrementation de WS-I.
           COMPUTE WS-I = WS-I + 1 + FUNCTION RANDOM

           END-PERFORM.

           DISPLAY "Le plus grand nombre saisi est " WS-NUM-BIGGEST.

           STOP RUN.
