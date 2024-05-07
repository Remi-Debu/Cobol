      ******************************************************************
      *    Le programme demande à l'utilisateur de saisir le nombre de *
      *    convives et lui affiche le nombre de pizza qu'il doit       *
      *    commander sachant que 1 convive mange 1.1 pizza.            *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. pizza.
       AUTHOR       Rémi.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WS-N-CONV PIC 9(05)V9(02).
       01  WS-RESULT PIC 9(10)V9(02).
       01  WS-Z-RESULT PIC Z(09)9.

      ****************************************************************** 
       PROCEDURE DIVISION.
      ******************************************************************
           DISPLAY "Nombre de convives :" SPACE WITH NO ADVANCING.
           ACCEPT WS-N-CONV.

           MULTIPLY WS-N-CONV BY 1.1 GIVING WS-RESULT.
           ADD 0.9 TO WS-RESULT. 
           MOVE WS-RESULT TO WS-Z-RESULT.

           DISPLAY "Nombre de pizza :" SPACE FUNCTION TRIM(WS-Z-RESULT).
           STOP RUN.
           