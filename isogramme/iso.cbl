       IDENTIFICATION DIVISION.
       PROGRAM-ID. iso.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TABLE-WORD OCCURS 5 TIMES.
           03 WS-WORD PIC X(25).
           03 WS-WORD-UPPER PIC X(25).
           03  WS-ALPHABETS OCCURS 26 TIMES.
               05 WS-CHAR         PIC X(01).
               05 WS-CHAR-COUNTER PIC 9(01).
           03  WS-IS-ISO PIC X(01) VALUE "N".

       01  WS-INDEX1 PIC 9(02) VALUE 1.
       01  WS-INDEX2 PIC 9(02).
       01  WS-TABLE-WORD-LENGTH PIC 9(02).
       01  WS-STOP   PIC X(01) VALUE "Y".

       PROCEDURE DIVISION.
           PERFORM MAIN THRU MAIN-END.
           STOP RUN.

       MAIN.
           PERFORM UNTIL WS-STOP EQUAL "N" OR WS-INDEX1 > 5
               DISPLAY SPACE
               DISPLAY "Saisi un mot :" SPACE WITH NO ADVANCING
               ACCEPT WS-WORD(WS-INDEX1)
      
      *        Stock le mot saisi en majuscule dans une variable
               MOVE FUNCTION UPPER-CASE(WS-WORD(WS-INDEX1)) 
               TO WS-WORD-UPPER(WS-INDEX1)

               PERFORM MOVE-ALPHABET THRU MOVE-ALPHABET-END

               PERFORM INSPECT-WORD 
               THRU INSPECT-WORD-END

               ADD 1 TO WS-INDEX1

               IF WS-INDEX1 <= 5
                   DISPLAY "Continuer (Y/N):" SPACE WITH NO ADVANCING
                   ACCEPT WS-STOP
               ELSE    
                   DISPLAY "Limite de demande atteinte."
               END-IF

           END-PERFORM.

           DISPLAY SPACE.
           PERFORM CHECK-ISO THRU CHECK-ISO-END.
       MAIN-END.

      *    Insère les lettres de l'alphabet 
      *    dans le sous tableau WS-CHAR.
       MOVE-ALPHABET.
           MOVE "A" TO WS-CHAR(WS-INDEX1, 1).
           MOVE "B" TO WS-CHAR(WS-INDEX1, 2).
           MOVE "C" TO WS-CHAR(WS-INDEX1, 3).
           MOVE "D" TO WS-CHAR(WS-INDEX1, 4).
           MOVE "E" TO WS-CHAR(WS-INDEX1, 5).
           MOVE "F" TO WS-CHAR(WS-INDEX1, 6).
           MOVE "G" TO WS-CHAR(WS-INDEX1, 7).
           MOVE "H" TO WS-CHAR(WS-INDEX1, 8).
           MOVE "I" TO WS-CHAR(WS-INDEX1, 9).
           MOVE "J" TO WS-CHAR(WS-INDEX1, 10).
           MOVE "K" TO WS-CHAR(WS-INDEX1, 11).
           MOVE "L" TO WS-CHAR(WS-INDEX1, 12).
           MOVE "M" TO WS-CHAR(WS-INDEX1, 13).
           MOVE "N" TO WS-CHAR(WS-INDEX1, 14).
           MOVE "O" TO WS-CHAR(WS-INDEX1, 15).
           MOVE "P" TO WS-CHAR(WS-INDEX1, 16).
           MOVE "Q" TO WS-CHAR(WS-INDEX1, 17).
           MOVE "R" TO WS-CHAR(WS-INDEX1, 18).
           MOVE "S" TO WS-CHAR(WS-INDEX1, 19).
           MOVE "T" TO WS-CHAR(WS-INDEX1, 20).
           MOVE "U" TO WS-CHAR(WS-INDEX1, 21).
           MOVE "V" TO WS-CHAR(WS-INDEX1, 22).
           MOVE "W" TO WS-CHAR(WS-INDEX1, 23).
           MOVE "X" TO WS-CHAR(WS-INDEX1, 24).
           MOVE "Y" TO WS-CHAR(WS-INDEX1, 25).
           MOVE "Z" TO WS-CHAR(WS-INDEX1, 26).
       MOVE-ALPHABET-END.

      *    Analyse le mot saisi pour incrémenter les différents 
      *    compteurs 
      *    Et si un des compteurs est supérieur à 1 
      *    set la variable is-iso à "Y" car le mot est isogramme.
       INSPECT-WORD.
           PERFORM VARYING WS-INDEX2 FROM 1 BY 1 
                   UNTIL WS-INDEX2 > 26

               INSPECT WS-WORD-UPPER(WS-INDEX1)
               TALLYING WS-CHAR-COUNTER(WS-INDEX1, WS-INDEX2) 
               FOR ALL WS-CHAR(WS-INDEX1, WS-INDEX2)
               
               IF WS-CHAR-COUNTER(WS-INDEX1, WS-INDEX2) > 1
                   SET WS-IS-ISO(WS-INDEX1) TO "Y"
               END-IF
           END-PERFORM.
       INSPECT-WORD-END.

      *    Vérifie dans tableau qui contient les mots saisis
      *    si ils sont isogrammes et affiche le message qui correspond.
       CHECK-ISO.
           MOVE WS-INDEX1 TO WS-TABLE-WORD-LENGTH.
           PERFORM VARYING WS-INDEX1 FROM 1 BY 1 
                   UNTIL WS-INDEX1 >= WS-TABLE-WORD-LENGTH
               IF WS-IS-ISO(WS-INDEX1) EQUAL "Y"
                  DISPLAY FUNCTION TRIM(WS-WORD(WS-INDEX1))
                  SPACE "est un isogramme."
               ELSE
                  DISPLAY FUNCTION TRIM(WS-WORD(WS-INDEX1))
                  SPACE "n'est pas un isogramme."
               END-IF
           END-PERFORM.
       CHECK-ISO-END.
