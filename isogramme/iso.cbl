      ******************************************************************
      *                                                                *
      *    Le programme demande à l'utilisateur de saisir des mots ou  *
      *    des phrase (de longueur 25 max), il peut en saisir jusqu'à  *
      *    5 ou décidé d'arrêter avant. Ensuite affiche selon si les   *
      *    mots ou les phrases contiennent 2 caractères ou plus        *
      *    identiques.                                                 *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. iso.
       AUTHOR.    Rémi.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TABLE-WORD OCCURS 5 TIMES.
         03 WS-WORD           PIC X(25).
         03 WS-WORD-UPPER     PIC X(25).
         03  WS-TABLE-COUNTER OCCURS 26 TIMES.
             05 WS-COUNT-CHAR PIC 9(01).
         03  WS-ISO           PIC X(01) VALUE "N".
           88 WS-ISO-YES                VALUE "Y".
           88 WS-ISO-NO                 VALUE "N".

       01  WS-ALPHABET PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

       01  WS-WORD-I            PIC 9(02) VALUE 1.
       01  WS-COUNT-CHAR-I      PIC 9(02).
       01  WS-TABLE-WORD-LENGTH PIC 9(02).
       01  WS-STOP              PIC X(01) VALUE "Y".

       PROCEDURE DIVISION.
           PERFORM START-MAIN THRU END-MAIN.
           STOP RUN.

       START-MAIN.
           PERFORM UNTIL WS-STOP EQUAL "N" OR WS-WORD-I > 5
               DISPLAY SPACE
               DISPLAY "Saisi un mot :" SPACE WITH NO ADVANCING
               ACCEPT WS-WORD(WS-WORD-I)
      
      *        Stock le mot saisi en majuscule dans une variable
               MOVE FUNCTION UPPER-CASE(WS-WORD(WS-WORD-I)) 
               TO WS-WORD-UPPER(WS-WORD-I)

               PERFORM START-INSPECT-WORD 
               THRU END-INSPECT-WORD

               ADD 1 TO WS-WORD-I

               IF WS-WORD-I <= 5
                   DISPLAY "Continuer (Y/N):" SPACE WITH NO ADVANCING
                   ACCEPT WS-STOP
               ELSE    
                   DISPLAY "Limite de demande atteinte."
               END-IF

           END-PERFORM.

           DISPLAY SPACE.
           PERFORM START-CHECK-ISO THRU END-CHECK-ISO.
       END-MAIN.

      *    Analyse le mot saisi pour incrémenter les différents 
      *    compteurs 
      *    Et si un des compteurs est supérieur à 1 
      *    set la variable is-iso à "Y" car le mot est isogramme.
       START-INSPECT-WORD.
           PERFORM VARYING WS-COUNT-CHAR-I FROM 1 BY 1 
                   UNTIL WS-COUNT-CHAR-I > 26

               INSPECT WS-WORD-UPPER(WS-WORD-I)
               TALLYING WS-COUNT-CHAR(WS-WORD-I, WS-COUNT-CHAR-I) 
               FOR ALL WS-ALPHABET(WS-COUNT-CHAR-I:1)
               
               IF WS-COUNT-CHAR(WS-WORD-I, WS-COUNT-CHAR-I) > 1
                   SET WS-ISO-YES(WS-WORD-I) TO TRUE
               END-IF
           END-PERFORM.
       END-INSPECT-WORD.

      *    Vérifie dans la table qui contient les mots saisis
      *    s'ils sont isogrammes et affiche le message qui correspond.
       START-CHECK-ISO.
           MOVE WS-WORD-I TO WS-TABLE-WORD-LENGTH.
           PERFORM VARYING WS-WORD-I FROM 1 BY 1 
                   UNTIL WS-WORD-I >= WS-TABLE-WORD-LENGTH

               IF WS-ISO-YES(WS-WORD-I)
                  DISPLAY FUNCTION TRIM(WS-WORD(WS-WORD-I))
                  SPACE "est un isogramme."
               ELSE
                  DISPLAY FUNCTION TRIM(WS-WORD(WS-WORD-I))
                  SPACE "n'est pas un isogramme."
               END-IF
           END-PERFORM.
       END-CHECK-ISO.
