      ******************************************************************
      *    Le programme demande à l'utilisateur de saisir une année    *
      *    et affiche si l'année est bissextile ou non.                *
      *    Puis demande à l'utilisateur s'il souhaite continuer.       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. bissext.
       AUTHOR.     Rémi.
       
      ****************************************************************** 
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-YEAR   PIC 9(04).
       01  WS-RESULT PIC X(40).

       01  WS-CONTINUE PIC X(03) VALUE "Y".
           88 WS-YES VALUE "Y".
           88 WS-NO  VALUE "N".
       
      ****************************************************************** 
       PROCEDURE DIVISION.
           PERFORM UNTIL WS-NO
           DISPLAY "Saisir une annee :" SPACE WITH NO ADVANCING 
           ACCEPT WS-YEAR
       
           IF FUNCTION MOD(WS-YEAR, 4) EQUAL ZERO
               IF FUNCTION MOD(WS-YEAR, 100) EQUAL ZERO
                   IF FUNCTION MOD(WS-YEAR, 400) EQUAL ZERO
                       MOVE "Oui, c'est une annee bissextile" 
                       TO WS-RESULT
                   ELSE
                       MOVE "Non, ce n'est pas une annee bissextile" 
                       TO WS-RESULT
                   END-IF
               ELSE
                   MOVE "Oui, c'est une annee bissextile" 
                   TO WS-RESULT
               END-IF
           ELSE
               MOVE "Non, ce n'est pas une annee bissextile" 
               TO WS-RESULT
           END-IF
       
           DISPLAY WS-RESULT

           DISPLAY "Continuer (Y/N) ?" SPACE WITH NO ADVANCING 
           ACCEPT WS-CONTINUE
           DISPLAY SPACE
           END-PERFORM.

           STOP RUN.
