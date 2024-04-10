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
       01  WS-NAME PIC X(20).

       PROCEDURE DIVISION.
           DISPLAY "Saisir votre nom :" SPACE 
           NO ADVANCING.
           ACCEPT WS-NAME.

           DISPLAY "Bienvenue," SPACE FUNCTION TRIM(WS-NAME) SPACE "!".

           STOP RUN.
