      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                                       *
      *    Le programme affiche la date du jour et la date de                 *
      *    compilation au format FR.                                          *
      *                                                                       *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mvdate.
       AUTHOR. Remi.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE-YMD.
           05  WS-CURRENT-YEAR    PIC  9(04).
           05  WS-CURRENT-MONTH   PIC  9(02).
           05  WS-CURRENT-DAY     PIC  9(02).

       01  WS-CURRENT-DATE-DMY.
           05  WS-CURRENT-DAY     PIC  9(02).
           05  WS-FILLER          PIC  X(01) VALUE "/".
           05  WS-CURRENT-MONTH   PIC  9(02).
           05  WS-FILLER          PIC  X(01) VALUE "/".
           05  WS-CURRENT-YEAR    PIC  9(04).

       01  WS-COMPILED-DATETIME-YMD PIC X(16).

       01  WS-COMPILED-DATETIME-DMY.
           05  WS-COMPILED-DATE-DMY.
               10  WS-COMPILED-DAY     PIC  9(02).
               10  FILLER              PIC  X VALUE "/".
               10  WS-COMPILED-MONTH   PIC  9(02).
               10  FILLER              PIC  X VALUE "/".
               10  WS-COMPILED-YEAR    PIC  9(04).
           05  WS-COMPILED-TIME.
               10  WS-FILLER           PIC  X(01).
               10  WS-COMPILED-HOUR    PIC  9(02).
               10  FILLER              PIC  X VALUE ":".
               10  WS-COMPILED-MINUTE  PIC  9(02).
               10  FILLER              PIC  X VALUE ":".
               10  WS-COMPILED-SECOND  PIC  9(02).

       PROCEDURE DIVISION.
      *    Transfert de la date du jour vers la date du jour formatée 
           MOVE FUNCTION CURRENT-DATE
           TO WS-CURRENT-DATE-YMD.

           MOVE CORR WS-CURRENT-DATE-YMD
           TO WS-CURRENT-DATE-DMY.

      *    Transfert de la date de compilation vers date de compilation
      *    formatée
           MOVE FUNCTION WHEN-COMPILED
           TO WS-COMPILED-DATETIME-YMD.

           MOVE WS-COMPILED-DATETIME-YMD(1:4)  TO WS-COMPILED-YEAR.
           MOVE WS-COMPILED-DATETIME-YMD(5:2)  TO WS-COMPILED-MONTH.
           MOVE WS-COMPILED-DATETIME-YMD(7:2)  TO WS-COMPILED-DAY.
           MOVE WS-COMPILED-DATETIME-YMD(9:2)  TO WS-COMPILED-HOUR.
           MOVE WS-COMPILED-DATETIME-YMD(11:2) TO WS-COMPILED-MINUTE.
           MOVE WS-COMPILED-DATETIME-YMD(13:2) TO WS-COMPILED-SECOND.

      *    Affichage des dates formatées
           DISPLAY "--------------------------------------------------".
           DISPLAY "Date du jour :" SPACE WS-CURRENT-DATE-DMY.
           DISPLAY "Date et heure de compilation :" SPACE
           WS-COMPILED-DATETIME-DMY.
           DISPLAY "--------------------------------------------------".

           STOP RUN.
