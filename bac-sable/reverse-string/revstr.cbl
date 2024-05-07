      *******************************************************************
      *    Le programme inverse une chaîne de caractères saisie par    *
      *    l'utilisateur.                                              *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. revstr.
       AUTHOR        Rémi.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WS-STRING          PIC X(20).
       01  WS-STRING-LENGTH   PIC 9(02).
       01  WS-STRING-LAST-POS PIC 9(02).
       01  WS-IDX             PIC 9(02).
       01  WS-REVERSE-STRING  PIC X(20).

      ****************************************************************** 
       PROCEDURE DIVISION.
      ******************************************************************
           DISPLAY "Saisir un mot :" SPACE WITH NO ADVANCING.
           ACCEPT WS-STRING.

      *    Longueur du string
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-STRING)) 
           TO WS-STRING-LENGTH.
           MOVE WS-STRING-LENGTH TO WS-STRING-LAST-POS.

      *    Parcours chaque caractère du string
           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-STRING-LENGTH
           
      *    Place le dernier caractère du string au début du reverse
      *    string
           MOVE WS-STRING(WS-STRING-LAST-POS:1) 
           TO WS-REVERSE-STRING(WS-IDX:1)
      
           SUBTRACT 1 FROM WS-STRING-LAST-POS
           END-PERFORM.
           
           DISPLAY "Mot inverse :" SPACE WS-REVERSE-STRING.
           STOP RUN.
           