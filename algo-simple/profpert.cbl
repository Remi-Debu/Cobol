      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                                       *
      *    Le programme demande à l'utilisateur de saisir le prix de          *
      *    fabrication et le prix de vente pour ensuite afficher le           *
      *    montant des profits ou gains.                                      *
      *                                                                       *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       IDENTIFICATION DIVISION.
       PROGRAM-ID. profpert.
       AUTHOR. Remi Debusschere.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PRICE.
           05  WS-MANU-PRICE  PIC 9(5)V9(2).
           05  WS-SELL-PRICE  PIC 9(5)V9(2).
           05  WS-PROFIT-LOSS PIC 9(6)V9(2).

       PROCEDURE DIVISION.
           DISPLAY "Saisir le prix de fabrication : " NO ADVANCING
           ACCEPT WS-MANU-PRICE.

           DISPLAY "Saisir le prix de vente : " NO ADVANCING ACCEPT
           WS-SELL-PRICE.

           SUBTRACT WS-MANU-PRICE FROM WS-SELL-PRICE GIVING
           WS-PROFIT-LOSS.

           IF WS-SELL-PRICE > WS-MANU-PRICE
              DISPLAY "Profit de " WS-PROFIT-LOSS " euros."
           ELSE
              DISPLAY "Perte de " WS-PROFIT-LOSS " euros."
           END-IF

           STOP RUN.
