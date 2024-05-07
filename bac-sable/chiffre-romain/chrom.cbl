      ******************************************************************
      *    Le programme demande à l'utilisateur de saisir un nombre et *
      *    l'affiche en chiffre Romain.                                *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. chrom.
       AUTHOR       Rémi.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  TABLE-NUM.
           03 NUM   PIC 9(04) OCCURS 13 TIMES.

       01  TABLE-LETTER.
           03 LETTER PIC X(02) OCCURS 13 TIMES.

       01  WS-IDX           PIC 9(02) VALUE 1.
       01  WS-LETTER-LENGTH PIC 9(02).
       01  WS-START-POS     PIC 9(04) VALUE 1.
       01  WS-INPUT         PIC 9(10).
       01  WS-OUTPUT        PIC X(1000).

      ****************************************************************** 
       PROCEDURE DIVISION.
      ******************************************************************
       START-MAIN.
           PERFORM START-INSERT-TABLE THRU END-INSERT-TABLE.
           PERFORM START-INPUT THRU END-INPUT.
           PERFORM START-HANDLE-OUTPUT THRU END-HANDLE-OUTPUT.
           PERFORM START-DISPLAY-OUTPUT THRU END-DISPLAY-OUTPUT.
       END-MAIN.
           STOP RUN.

      ******************************************************************
      *    Parcours les 2 tableaux NUM et LETTER et si le nombre saisi *
      *    par l'utilisateur est + grand que le nombre du tableau à    *
      *    l'emplacement selon l'index alors 
      ******************************************************************
       START-HANDLE-OUTPUT.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 13
              IF WS-INPUT >= NUM(WS-IDX)
                 SUBTRACT NUM(WS-IDX) FROM WS-INPUT

                 MOVE FUNCTION LENGTH(FUNCTION TRIM(LETTER(WS-IDX))) 
                 TO WS-LETTER-LENGTH

                 MOVE LETTER(WS-IDX) 
                 TO WS-OUTPUT(WS-START-POS:WS-LETTER-LENGTH) 

                 SUBTRACT 1 FROM WS-IDX 
                 ADD WS-LETTER-LENGTH TO WS-START-POS
              END-IF
           END-PERFORM.
       END-HANDLE-OUTPUT.
           EXIT.

      ******************************************************************
      *    Demande à l'utilisateur de saisir un nombe.                 *
      ******************************************************************
       START-INPUT.
           DISPLAY "Saisir un nombre :" SPACE WITH NO ADVANCING.
           ACCEPT WS-INPUT.
       END-INPUT.
           EXIT.

      ******************************************************************
      *    Affiche le chiffre romain par rapport au nombre saisi.      *
      ******************************************************************
       START-DISPLAY-OUTPUT.
           DISPLAY "Chiffre romain :" SPACE FUNCTION TRIM(WS-OUTPUT).
       END-DISPLAY-OUTPUT.
           EXIT.
       
      ******************************************************************
      *    Insère les différentes valeurs des tableaux NUM et LETTER.  * 
      ******************************************************************
       START-INSERT-TABLE.
           MOVE 1000 TO NUM(1).
           MOVE 900  TO NUM(2).
           MOVE 500  TO NUM(3).
           MOVE 400  TO NUM(4).
           MOVE 100  TO NUM(5).
           MOVE 90   TO NUM(6).
           MOVE 50   TO NUM(7).
           MOVE 40   TO NUM(8).
           MOVE 10   TO NUM(9).
           MOVE 9    TO NUM(10).
           MOVE 5    TO NUM(11).
           MOVE 4    TO NUM(12).
           MOVE 1    TO NUM(13).

           MOVE "M"  TO LETTER(1).
           MOVE "CM" TO LETTER(2).
           MOVE "D"  TO LETTER(3).
           MOVE "CD" TO LETTER(4).
           MOVE "C"  TO LETTER(5).
           MOVE "XC" TO LETTER(6).
           MOVE "L"  TO LETTER(7).
           MOVE "XL" TO LETTER(8).
           MOVE "X"  TO LETTER(9).
           MOVE "IX" TO LETTER(10).
           MOVE "V"  TO LETTER(11).
           MOVE "IV" TO LETTER(12).
           MOVE "I"  TO LETTER(13).
       END-INSERT-TABLE.
           EXIT.
           