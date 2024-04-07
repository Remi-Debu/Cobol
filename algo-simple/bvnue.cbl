      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                                       *
      *    Le programme demande à l'utilisateur de saisir son nom             *
      *    et l'affiche avec un message de bienvenue.                         *
      *                                                                       *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. bvnue.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NAME PIC X(20).

       PROCEDURE DIVISION.
           DISPLAY "Saisir votre nom : " NO ADVANCING ACCEPT NAME.
           DISPLAY "Bienvenue, " FUNCTION TRIM(NAME) " !".

           STOP RUN.
