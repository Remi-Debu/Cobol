       IDENTIFICATION DIVISION.
       PROGRAM-ID. bissext.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ANNEE            PIC 9(4).
       01  WS-RESULTAT         PIC X(40).

       01  WS-CONTINUER PIC X(03) VALUE "Y".
           88 WS-YES VALUE "Y".
           88 WS-NO  VALUE "N".
       
       PROCEDURE DIVISION.
           PERFORM UNTIL WS-NO
           DISPLAY "Entrez une annee :" SPACE WITH NO ADVANCING 
           ACCEPT WS-ANNEE
       
           IF FUNCTION MOD(WS-ANNEE, 4) EQUAL ZERO
               IF FUNCTION MOD(WS-ANNEE, 100) EQUAL ZERO
                   IF FUNCTION MOD(WS-ANNEE, 400) EQUAL ZERO
                       MOVE "Oui, c'est une annee bissextile" 
                       TO WS-RESULTAT
                   ELSE
                       MOVE "Non, ce n'est pas une annee bissextile" 
                       TO WS-RESULTAT
                   END-IF
               ELSE
                   MOVE "Oui, c'est une annee bissextile" 
                   TO WS-RESULTAT
               END-IF
           ELSE
               MOVE "Non, ce n'est pas une annee bissextile" 
               TO WS-RESULTAT
           END-IF
       
           DISPLAY WS-RESULTAT

           DISPLAY "Continuer (Y/N) ?" SPACE WITH NO ADVANCING 
           ACCEPT WS-CONTINUER
           DISPLAY SPACE
           END-PERFORM.

           STOP RUN.
