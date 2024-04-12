       IDENTIFICATION DIVISION.
       PROGRAM-ID. rfl.
       AUTHOR.     Remi.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ASSU-FILE ASSIGN TO "assurances.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ASSU-CODE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ASSU-FILE.

       01  ASSU-RECORD PIC X(121).


       WORKING-STORAGE SECTION.
       01  ASSU-CODE-STATUS PIC X(02).
       01  WS-STOP        PIC X(01) VALUE "N".
       01  WS-INDEX         PIC 9(02) VALUE 1.

       01  WS-TABLE-ASSU. 
           05 WS-ASSU  OCCURS 1 TO 99 TIMES
                       DEPENDING ON WS-INDEX.
               10 WS-ID     PIC X(08).
               10 WS-NAME-A PIC X(14).
               10 WS-NAME-B PIC X(14).
               10 WS-NAME-C PIC X(41).
               10 WS-STATUS PIC X(08).
               10 WS-NUM-A  PIC X(08).
               10 WS-NUM-B  PIC X(08).
               10 WS-AMOUNT PIC 9(06)V9(02).
               10 WS-EURO   PIC X(03).

       PROCEDURE DIVISION.
           OPEN INPUT ASSU-FILE.
           
           DISPLAY "--------------------------------------------------".
           DISPLAY "FILE STATUS CODE :" SPACE ASSU-CODE-STATUS.
           DISPLAY "--------------------------------------------------".

           PERFORM UNTIL WS-STOP = "Y"
               READ ASSU-FILE
               AT END 
                   SET WS-STOP TO "Y"
               NOT AT END 
                   UNSTRING ASSU-RECORD 
                   DELIMITED BY "*"
                   INTO WS-ID(WS-INDEX) WS-NAME-A(WS-INDEX) 
                   WS-NAME-B(WS-INDEX) WS-NAME-C(WS-INDEX) 
                   WS-STATUS(WS-INDEX) WS-NUM-A(WS-INDEX) 
                   WS-NUM-B(WS-INDEX) WS-AMOUNT(WS-INDEX)
                   WS-EURO(WS-INDEX)

                   DISPLAY WS-ASSU(WS-INDEX)
                   ADD 1 TO WS-INDEX
           END-PERFORM.
               
           CLOSE ASSU-FILE.

      *    Affichage des données 3 et 7
           DISPLAY "--------------------------------------------------".
           DISPLAY "ID       :" SPACE WS-ID(3).
           DISPLAY "GROUP    :" SPACE WS-NAME-A(3).
           DISPLAY "IRP      :" SPACE WS-NAME-B(3).
           DISPLAY "INTITULE :" SPACE WS-NAME-C(3).
           DISPLAY "CONTRAT  :" SPACE WS-STATUS(3).
           DISPLAY "NUM A    :" SPACE WS-NUM-A(3).
           DISPLAY "NUM B    :" SPACE WS-NUM-B(3).
           DISPLAY "MONTANT  :" SPACE WS-AMOUNT(3) WS-EURO(WS-INDEX).
           DISPLAY "--------------------------------------------------".
           DISPLAY "ID       :" SPACE WS-ID(7).
           DISPLAY "GROUP    :" SPACE WS-NAME-A(7).
           DISPLAY "IRP      :" SPACE WS-NAME-B(7).
           DISPLAY "INTITULE :" SPACE WS-NAME-C(7).
           DISPLAY "CONTRAT  :" SPACE WS-STATUS(7).
           DISPLAY "NUM A    :" SPACE WS-NUM-A(7).
           DISPLAY "NUM B    :" SPACE WS-NUM-B(7).
           DISPLAY "MONTANT  :" SPACE WS-AMOUNT(7) WS-EURO(WS-INDEX).
           DISPLAY "--------------------------------------------------".

           STOP RUN.
