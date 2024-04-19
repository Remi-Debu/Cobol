      ******************************************************************
      *                                                                *
      *    Le programme demande à l'utilisateur de saisir son nom      *
      *    et l'affiche avec un message de bienvenue.                  *
      *                                                                *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. bvnue.
       AUTHOR.     Rémi.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NOM PIC X(20).

       PROCEDURE DIVISION.
           DISPLAY "Saisir votre nom :" SPACE NO ADVANCING.
           ACCEPT WS-NOM.

           DISPLAY "Bienvenue," SPACE FUNCTION TRIM(WS-NOM) SPACE "!".

           STOP RUN.
