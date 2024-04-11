       IDENTIFICATION DIVISION.
       PROGRAM-ID. rfl.
       AUTHOR. Remi.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ASSURANCE-FILE ASSIGN TO "assurances.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-ASSURANCE-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ASSURANCE-FILE.
       01  ASSURANCE-RECORD PIC X(118).


       WORKING-STORAGE SECTION.
       01  WS-ASSURANCE-FILE-STATUS PIC X(02).
       01  WS-SWITCH PIC X(01) VALUE "N".
       01  WS-INDEX PIC 9(02) VALUE 1.

       01  WS-TABLE. 
           05 WS-ASSU  OCCURS 1 TO 99 TIMES
                       DEPENDING ON WS-INDEX.
               10 WS-ID PIC X(08).
               10 WS-GROUP PIC X(14).
               10 WS-IRP PIC X(14).
               10 WS-INTITULE PIC X(41).
               10 WS-CONTRAT PIC X(08).
               10 WS-NUM-A PIC X(08).
               10 WS-NUM-B PIC 9(06)V9(02).
               10 WS-MONTANT PIC X(09).


       PROCEDURE DIVISION.
           OPEN INPUT ASSURANCE-FILE.
           DISPLAY WS-ASSURANCE-FILE-STATUS.

           PERFORM UNTIL WS-SWITCH = "Y"
               READ ASSURANCE-FILE
               AT END 
                   SET WS-SWITCH TO "Y"
               NOT AT END 
                   UNSTRING ASSURANCE-RECORD 
                   DELIMITED BY "*"
                   INTO WS-ID(WS-INDEX) WS-GROUP(WS-INDEX) 
                   WS-IRP(WS-INDEX) WS-INTITULE(WS-INDEX) 
                   WS-CONTRAT(WS-INDEX) WS-NUM-A(WS-INDEX) 
                   WS-NUM-B(WS-INDEX) WS-MONTANT(WS-INDEX)
                   DISPLAY WS-ASSU(WS-INDEX)

                   ADD 1 TO WS-INDEX
           END-PERFORM.
               
           CLOSE ASSURANCE-FILE.

      *    Affichage des données 3 et 7
           DISPLAY "---------------------------------------------------"
           DISPLAY "ID :" SPACE WS-ID(3).
           DISPLAY "GROUP :" SPACE WS-GROUP(3).
           DISPLAY "IRP :" SPACE WS-IRP(3).
           DISPLAY "INTITULE :" SPACE WS-INTITULE(3).
           DISPLAY "CONTRAT :" SPACE WS-CONTRAT(3).
           DISPLAY "NUM A :" SPACE WS-NUM-A(3).
           DISPLAY "NUM B :" SPACE WS-NUM-B(3).
           DISPLAY "MONTANT :" SPACE WS-MONTANT(3).
           DISPLAY "---------------------------------------------------"
           DISPLAY "ID :" SPACE WS-ID(7).
           DISPLAY "GROUP :" SPACE WS-GROUP(7).
           DISPLAY "IRP :" SPACE WS-IRP(7).
           DISPLAY "INTITULE :" SPACE WS-INTITULE(7).
           DISPLAY "CONTRAT :" SPACE WS-CONTRAT(7).
           DISPLAY "NUM A :" SPACE WS-NUM-A(7).
           DISPLAY "NUM B :" SPACE WS-NUM-B(7).
           DISPLAY "MONTANT :" SPACE WS-MONTANT(7).
           DISPLAY "---------------------------------------------------"

           STOP RUN.
