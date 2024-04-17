      ****************************************************************** 
      *                                                                *
      *    Le programme lis 2 fichiers puis ecrit dans un nouveau      *
      *    fichier un rapport de synthèse.                             *
      *    Dans ce rapport on retrouve les données des 2 fichiers,     *
      *    la date de compilation, le nombre d'enregistrements         *
      *    et le nombre d'occurence des différents status.             *
      *                                                                *
      ******************************************************************
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. insucli.
       AUTHOR.     Remi.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INSU-P1 ASSIGN TO "assurances-part1.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-INSU1.

           SELECT INSU-P2 ASSIGN TO "assurances-part2.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-INSU2.

           SELECT INSU-REP ASSIGN TO "rapport-assurances.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-INSU-REP.

       DATA DIVISION.
       FILE SECTION.
       FD  INSU-P1.
       01  INSU-RECORD-P1 PIC X(123).

       FD  INSU-P2.
       01  INSU-RECORD-P2 PIC X(123).

       FD  INSU-REP.
       01  INSU-REP-RECORD PIC X(200).


       WORKING-STORAGE SECTION.
       01  WS-DISPLAY.
           03 WS-DASH      PIC X(200).
           03 WS-AST       PIC X(200).
           03 WS-STRING    PIC X(123).
           03 WS-NUM       PIC 9(08).
           03 WS-NUM-CLEAN PIC ZZZZZZZZZZZ9,99.

       01  WS-FILE-STATUS.
           03  FS-INSU1    PIC X(02).
           03  FS-INSU2    PIC X(02).
           03  FS-INSU-REP PIC X(02).

       01  WS-COMP-DATE-YMD.
           03  WS-CURRENT-YEAR    PIC  9(04).
           03  WS-CURRENT-MONTH   PIC  9(02).
           03  WS-CURRENT-DAY     PIC  9(02).

       01  WS-COMP-DATE-DMY.
           03  WS-CURRENT-DAY     PIC  9(02).
           03  WS-FILLER          PIC  X(01) VALUE "/".
           03  WS-CURRENT-MONTH   PIC  9(02).
           03  WS-FILLER          PIC  X(01) VALUE "/".
           03  WS-CURRENT-YEAR    PIC  9(04).

       01  WS-COUNTERS.
           03  WS-COUNT-RECORD1   PIC 9(03) VALUE 0.
           03  WS-COUNT-RECORD2   PIC 9(03) VALUE 0.
           03  WS-COUNT-TOTAL     PIC 9(04) VALUE 0.
           03  WS-COUNT-ACTIVE    PIC 9(03) VALUE 0.
           03  WS-COUNT-SUSPENDED PIC 9(03) VALUE 0.
           03  WS-COUNT-CANCELED  PIC 9(03) VALUE 0.
       
       01  WS-TOTAL-AMOUT.
           03 WS-TOTAL-AMOUNT-P1 PIC 9(10).
           03 WS-TOTAL-AMOUNT-P2 PIC 9(10).
           03 WS-TOTAL-AMOUNT    PIC 9(10).

       01  WS-INSU-TABLE. 
           03 WS-INSU  OCCURS 1 TO 99 TIMES
                       DEPENDING ON WS-INDEX.
               05 FILLER       PIC X(03) VALUE "|".
               05 WS-ID        PIC X(04).
               05 FILLER       PIC X(03) VALUE "|".
               05 WS-ID-CLIENT PIC X(12).
               05 FILLER       PIC X(07) VALUE "|".
               05 WS-GROUP     PIC X(18).
               05 FILLER       PIC X(03) VALUE "|".
               05 WS-NAME      PIC X(29).
               05 FILLER       PIC X(03) VALUE "|".
               05 WS-LABEL     PIC X(41).
               05 FILLER       PIC X(07) VALUE "|".
               05 WS-STATUS    PIC X(14).
               05 FILLER       PIC X(06) VALUE "|".
               05 WS-FROM      PIC X(13).
               05 FILLER       PIC X(06) VALUE "|".
               05 WS-TO        PIC X(13).
               05 FILLER       PIC X(03) VALUE "|".
               05 WS-AMOUNT    PIC X(09).
               05 WS-EURO      PIC X(05).
               05 FILLER       PIC X(01) VALUE "|".

       01  WS-LABEL-STATUS-TABLE.
           03 WS-LABEL-STATUS OCCURS 1 TO 99 TIMES   
                              DEPENDING ON WS-LS-INDEX.
              05 WS-LS-ID     PIC X(10).
              05 WS-LS-LABEL  PIC X(60).
              05 WS-LS-STATUS PIC X(30).

       01  WS-STOP     PIC 9(01) VALUE 0.
       01  WS-INDEX    PIC 9(02) VALUE 1.
       01  WS-LS-INDEX PIC 9(02) VALUE 1.
                                  

       PROCEDURE DIVISION.
           PERFORM 0000-MAIN THRU 0000-MAIN-END.
           STOP RUN.

      ****************************************************************** 
      *    0000 = MAIN                                                 *
      *    1000 = PART 1                                               *
      *    2000 = PART 2                                               *
      *    3000 = PART LABEL STATUS                                    *
      *    4000 = ALL                                                  *
      ******************************************************************

      *    APPEL DE TOUT LES PARAGRAPHES.
       0000-MAIN.
           MOVE ALL "-" TO WS-DASH.
           MOVE ALL "*" TO WS-AST.

           PERFORM 4000-HEADER-REPORT THRU 4000-HEADER-REPORT-END.
           PERFORM 1000-PART1-HEADER  THRU 1000-PART1-HEADER-END.
           PERFORM 1000-PART1-READ    THRU 1000-PART1-READ-END.
           PERFORM 1000-PART1-WRITE   THRU 1000-PART1-WRITE-END.
           PERFORM 2000-PART2-HEADER  THRU 2000-PART2-HEADER-END.
           PERFORM 2000-PART2-READ    THRU 2000-PART2-READ-END.
           PERFORM 2000-PART2-WRITE   THRU 2000-PART2-WRITE-END.

           PERFORM 3000-PART-LABEL-STATUS-WRITE 
              THRU 3000-PART-LABEL-STATUS-WRITE-END.

           PERFORM 4000-FOOTER-REPORT THRU 4000-FOOTER-REPORT-END.
       0000-MAIN-END.

      *    ECRIS LE HEADER (TITRE ET DATE DE COMPILATION)
      *    DANS LE RAPORT D'ASSURANCES. 
       4000-HEADER-REPORT.
           OPEN OUTPUT INSU-REP.
           DISPLAY "FS INSU REP WRITE :" SPACE FS-INSU-REP.

           WRITE INSU-REP-RECORD 
           FROM "INSURANCE CLIENTS SUMMARY REPORT".
           WRITE INSU-REP-RECORD FROM SPACE.

           MOVE FUNCTION CURRENT-DATE
           TO WS-COMP-DATE-YMD.

           MOVE CORR WS-COMP-DATE-YMD
           TO WS-COMP-DATE-DMY.

           STRING "Report Generation Date :" DELIMITED BY SIZE,
                  SPACE,
                  WS-COMP-DATE-DMY DELIMITED BY SIZE,
                  INTO WS-STRING
           END-STRING.
           
           WRITE INSU-REP-RECORD FROM WS-STRING.
           WRITE INSU-REP-RECORD FROM SPACE.

           CLOSE INSU-REP.
       4000-HEADER-REPORT-END.

      *    ECRIS LE HEADER (NOM DES COLONNES) 
      *    DE LA PARTIE 1 DU RAPPORT.
       1000-PART1-HEADER. 
           OPEN EXTEND INSU-REP.
           DISPLAY "FS INSU REP WRITE :" SPACE FS-INSU-REP.

           WRITE INSU-REP-RECORD FROM WS-AST.
           WRITE INSU-REP-RECORD FROM "PART 1".
           WRITE INSU-REP-RECORD FROM WS-AST.
           WRITE INSU-REP-RECORD FROM WS-DASH.

           MOVE "ID"        TO WS-ID(1).
           MOVE "ID CLIENT" TO WS-ID-CLIENT(1).
           MOVE "GROUP"     TO WS-GROUP(1).
           MOVE "NAME"      TO WS-NAME(1).
           MOVE "LABEL"     TO WS-LABEL(1).
           MOVE "STATUS"    TO WS-STATUS(1).
           MOVE "FROM"      TO WS-FROM(1).
           MOVE "TO"        TO WS-TO(1).
           MOVE "AMOUNT"    TO WS-AMOUNT(1).

           WRITE INSU-REP-RECORD FROM WS-INSU(1).
           WRITE INSU-REP-RECORD FROM WS-DASH.
           CLOSE INSU-REP.
       1000-PART1-HEADER-END.

      *    LIS LE FICHIER D'ASSURANCES PARTIE 1
      *    PUIS STOCK LES DONNEES DANS MA WORKING STORAGE SECTION.
       1000-PART1-READ.
           OPEN INPUT INSU-P1.
        
           DISPLAY "FS INSU P1 READ :" SPACE FS-INSU1.
           
           INITIALIZE WS-INSU-TABLE.
           PERFORM UNTIL WS-STOP = 1
               READ INSU-P1
               AT END 
                   SET WS-STOP TO 1
               NOT AT END 
                   UNSTRING INSU-RECORD-P1 
                   DELIMITED BY "*"
                   INTO WS-ID-CLIENT(WS-INDEX) WS-GROUP(WS-INDEX) 
                   WS-NAME(WS-INDEX) WS-LABEL(WS-INDEX) 
                   WS-STATUS(WS-INDEX) WS-FROM(WS-INDEX) 
                   WS-TO(WS-INDEX) WS-AMOUNT(WS-INDEX)
                   WS-EURO(WS-INDEX)

                   EVALUATE WS-STATUS(WS-INDEX)
                       WHEN "Actif"
                           ADD 1 TO WS-COUNT-ACTIVE
                       WHEN "Suspendu"
                          ADD 1 TO WS-COUNT-SUSPENDED
                       WHEN "Resilie"
                          ADD 1 TO WS-COUNT-CANCELED
                       WHEN "Resilié"
                          ADD 1 TO WS-COUNT-CANCELED
                       WHEN OTHER
                          CONTINUE
                   END-EVALUATE
                    
                   MOVE WS-LABEL(WS-INDEX) 
                   TO WS-LS-LABEL(WS-LS-INDEX)

                   MOVE WS-STATUS(WS-INDEX) 
                   TO WS-LS-STATUS(WS-LS-INDEX) 

                   INITIALIZE WS-NUM
                   MOVE WS-AMOUNT(WS-INDEX) TO WS-NUM
                   ADD WS-NUM TO WS-TOTAL-AMOUNT-P1

                   MOVE WS-INDEX TO WS-ID(WS-INDEX) 

                   ADD 1 TO WS-INDEX
                   ADD 1 TO WS-LS-INDEX
                   ADD 1 TO WS-COUNT-RECORD1
           END-PERFORM.           
           CLOSE INSU-P1.
       1000-PART1-READ-END.

      *    ECRIS LES DONNEES DE MA TABLE DANS LE RAPPORT. 
       1000-PART1-WRITE.
           OPEN EXTEND INSU-REP.

           SET WS-INDEX TO 1.
           PERFORM WS-COUNT-RECORD1 TIMES
               WRITE INSU-REP-RECORD FROM WS-INSU(WS-INDEX)
               WRITE INSU-REP-RECORD FROM WS-DASH 

               ADD 1 TO WS-INDEX
           END-PERFORM.
           WRITE INSU-REP-RECORD FROM SPACE.

           CLOSE INSU-REP.
       1000-PART1-WRITE-END.

      *    ECRIS LE HEADER (NOM DES COLONNES) 
      *    DE LA PARTIE 2 DU RAPPORT.
       2000-PART2-HEADER. 
           INITIALIZE WS-INSU-TABLE.
           OPEN EXTEND INSU-REP.
           DISPLAY "FS INSU REP WRITE :" SPACE FS-INSU-REP.

           WRITE INSU-REP-RECORD FROM WS-AST.
           WRITE INSU-REP-RECORD FROM "PART 2".
           WRITE INSU-REP-RECORD FROM WS-AST.
           WRITE INSU-REP-RECORD FROM WS-DASH.

           MOVE "ID"        TO WS-ID(1).
           MOVE "ID CLIENT" TO WS-ID-CLIENT(1).
           MOVE "GROUP"     TO WS-GROUP(1).
           MOVE "NAME"      TO WS-NAME(1).
           MOVE "LABEL"     TO WS-LABEL(1).
           MOVE "STATUS"    TO WS-STATUS(1).
           MOVE "FROM"      TO WS-FROM(1).
           MOVE "TO"        TO WS-TO(1).
           MOVE "AMOUNT"    TO WS-AMOUNT(1).

           WRITE INSU-REP-RECORD FROM WS-INSU(1).
           WRITE INSU-REP-RECORD FROM WS-DASH.
           CLOSE INSU-REP.
       2000-PART2-HEADER-END.

      *    LIS LE FICHIER D'ASSURANCES PARTIE 2
      *    PUIS STOCK LES DONNEES DANS MA WORKING STORAGE SECTION.
       2000-PART2-READ.
           OPEN INPUT INSU-P2.
        
           DISPLAY "FS INSU P2 READ :" SPACE FS-INSU2.
           
           INITIALIZE WS-INSU-TABLE.
           SET WS-INDEX TO 1.
           SET WS-STOP TO 0.
           PERFORM UNTIL WS-STOP = 1
               READ INSU-P2
               AT END 
                   SET WS-STOP TO 1
               NOT AT END 
                   UNSTRING INSU-RECORD-P2 
                   DELIMITED BY "*"
                   INTO WS-ID-CLIENT(WS-INDEX) WS-GROUP(WS-INDEX) 
                   WS-NAME(WS-INDEX) WS-LABEL(WS-INDEX) 
                   WS-STATUS(WS-INDEX) WS-FROM(WS-INDEX) 
                   WS-TO(WS-INDEX) WS-AMOUNT(WS-INDEX)
                   WS-EURO(WS-INDEX)

                   EVALUATE WS-STATUS(WS-INDEX)
                       WHEN "Actif"
                           ADD 1 TO WS-COUNT-ACTIVE
                       WHEN "Suspendu"
                          ADD 1 TO WS-COUNT-SUSPENDED
                       WHEN "Resilie"
                          ADD 1 TO WS-COUNT-CANCELED
                       WHEN "Resilié"
                          ADD 1 TO WS-COUNT-CANCELED
                       WHEN OTHER
                          CONTINUE
                   END-EVALUATE

                   MOVE WS-LABEL(WS-INDEX) 
                   TO WS-LS-LABEL(WS-LS-INDEX)

                   MOVE WS-STATUS(WS-INDEX) 
                   TO WS-LS-STATUS(WS-LS-INDEX) 

                   INITIALIZE WS-NUM
                   MOVE WS-AMOUNT(WS-INDEX) TO WS-NUM
                   ADD WS-NUM TO WS-TOTAL-AMOUNT-P2
                   
                   MOVE WS-INDEX TO WS-ID(WS-INDEX) 

                   ADD 1 TO WS-INDEX
                   ADD 1 TO WS-LS-INDEX
                   ADD 1 TO WS-COUNT-RECORD2
           END-PERFORM.           
           CLOSE INSU-P2.
       2000-PART2-READ-END.

      *    ECRIS LES DONNEES DE MA TABLE DANS LE RAPPORT.
       2000-PART2-WRITE.
           OPEN EXTEND INSU-REP.

           SET WS-INDEX TO 1.
           PERFORM WS-COUNT-RECORD2 TIMES
               WRITE INSU-REP-RECORD FROM WS-INSU(WS-INDEX)
               WRITE INSU-REP-RECORD FROM WS-DASH 

               ADD 1 TO WS-INDEX
           END-PERFORM.
           WRITE INSU-REP-RECORD FROM SPACE.
           CLOSE INSU-REP.
       2000-PART2-WRITE-END.

      *    ECRIS LA SECTION LABEL ET STATUS DANS LE RAPPORT.
       3000-PART-LABEL-STATUS-WRITE.
           OPEN EXTEND INSU-REP.

           DISPLAY "FS WRITE RAP ASSU :" SPACE FS-INSU-REP.

           WRITE INSU-REP-RECORD FROM WS-AST.
           WRITE INSU-REP-RECORD FROM "DISPLAY LABEL AND STATUS".
           WRITE INSU-REP-RECORD FROM WS-AST.

           COMPUTE WS-COUNT-TOTAL = WS-COUNT-RECORD1 + WS-COUNT-RECORD2.

           SET WS-LS-INDEX TO 1.
           PERFORM WS-COUNT-TOTAL TIMES
               MOVE WS-LS-INDEX TO WS-LS-ID(WS-LS-INDEX)

               STRING 
               "ID :" SPACE WS-LS-ID(WS-LS-INDEX)
               "LIBELLE :" SPACE WS-LS-LABEL(WS-LS-INDEX) SPACE 
               "STATUS :" SPACE WS-LS-STATUS(WS-LS-INDEX)
               DELIMITED BY SIZE 
               INTO WS-STRING
               END-STRING

               WRITE INSU-REP-RECORD FROM WS-STRING

               WRITE INSU-REP-RECORD FROM WS-DASH 
               ADD 1 TO WS-LS-INDEX
           END-PERFORM.
       3000-PART-LABEL-STATUS-WRITE-END.

      *    ECRIS LE FOOTER DU RAPPORT
      *    QUI APPEL AUSSI D'AUTRES PARAGRAPHES.
       4000-FOOTER-REPORT.
           WRITE INSU-REP-RECORD FROM SPACE.
           WRITE INSU-REP-RECORD FROM WS-AST.
           WRITE INSU-REP-RECORD FROM "SUMMARY".
           WRITE INSU-REP-RECORD FROM WS-AST.
           WRITE INSU-REP-RECORD FROM WS-DASH.

           PERFORM 4000-NUMBER-RECORDS-PART1 
              THRU 4000-NUMBER-RECORDS-PART1-END.

           PERFORM 4000-TOTAL-AMOUNT-PART1 
              THRU 4000-TOTAL-AMOUNT-PART1-END.

           PERFORM 4000-NUMBER-RECORDS-PART2 
              THRU 4000-NUMBER-RECORDS-PART2-END.

           PERFORM 4000-TOTAL-AMOUNT-PART2 
              THRU 4000-TOTAL-AMOUNT-PART2-END.
           
           PERFORM 4000-TOTAL-AMOUNT THRU 4000-TOTAL-AMOUNT-END

           PERFORM 4000-TOTAL-NUMBER-RECORDS 
              THRU 4000-TOTAL-NUMBER-RECORDS-END.

           PERFORM 4000-TOTAL-ACTIVE    THRU 4000-TOTAL-ACTIVE-END.
           PERFORM 4000-TOTAL-SUSPENDED THRU 4000-TOTAL-SUSPENDED-END.
           PERFORM 4000-TOTAL-CANCELED  THRU 4000-TOTAL-CANCELED-END.
       4000-FOOTER-REPORT-END.

      *    ECRIS LE NUMBER RECORDS PART 1 DANS LE RAPPORT.
       4000-NUMBER-RECORDS-PART1.
           INITIALIZE WS-STRING.
           STRING "NUMBER RECORDS PART 1 :" SPACE
           WS-COUNT-RECORD1
           DELIMITED BY SIZE 
           INTO WS-STRING.

           WRITE INSU-REP-RECORD FROM WS-STRING.
           WRITE INSU-REP-RECORD FROM WS-DASH.
       4000-NUMBER-RECORDS-PART1-END.

      *    ECRIS TOTAL AMOUNT PART 1 DANS LE RAPPORT.
       4000-TOTAL-AMOUNT-PART1.
           INITIALIZE WS-NUM-CLEAN
           MOVE WS-TOTAL-AMOUNT-P1 TO WS-NUM-CLEAN

           INITIALIZE WS-STRING.
           STRING "TOTAL AMOUNT PART 1   :" SPACE
           FUNCTION TRIM(WS-NUM-CLEAN) WS-EURO(1)
           DELIMITED BY SIZE 
           INTO WS-STRING.

           WRITE INSU-REP-RECORD FROM WS-STRING.
           WRITE INSU-REP-RECORD FROM WS-DASH.
       4000-TOTAL-AMOUNT-PART1-END.

      *    ECRIS LE NUMBER RECORDS PART 2 DANS LE RAPPORT.
       4000-NUMBER-RECORDS-PART2.
           INITIALIZE WS-STRING.
           STRING "NUMBER RECORDS PART 2 :" SPACE
           WS-COUNT-RECORD2
           DELIMITED BY SIZE 
           INTO WS-STRING.

           WRITE INSU-REP-RECORD FROM WS-STRING.
           WRITE INSU-REP-RECORD FROM WS-DASH.
       4000-NUMBER-RECORDS-PART2-END.

      *    ECRIS TOTAL AMOUNT PART 2 DANS LE RAPPORT.
       4000-TOTAL-AMOUNT-PART2.
           INITIALIZE WS-NUM-CLEAN
           MOVE WS-TOTAL-AMOUNT-P2 TO WS-NUM-CLEAN

           INITIALIZE WS-STRING.
           STRING "TOTAL AMOUNT PART 2   :" SPACE
           FUNCTION TRIM(WS-NUM-CLEAN) WS-EURO(1)
           DELIMITED BY SIZE 
           INTO WS-STRING.

           WRITE INSU-REP-RECORD FROM WS-STRING.
           WRITE INSU-REP-RECORD FROM WS-DASH.
       4000-TOTAL-AMOUNT-PART2-END.

      *    ECRIS TOTAL AMOUNT DANS LE RAPPORT.
       4000-TOTAL-AMOUNT.
           INITIALIZE WS-NUM-CLEAN
           COMPUTE WS-TOTAL-AMOUNT = WS-TOTAL-AMOUNT-P1 
                                   + WS-TOTAL-AMOUNT-P2
           MOVE WS-TOTAL-AMOUNT TO WS-NUM-CLEAN

           INITIALIZE WS-STRING.
           STRING "TOTAL AMOUNT PART 2   :" SPACE
           FUNCTION TRIM(WS-NUM-CLEAN) WS-EURO(1)
           DELIMITED BY SIZE 
           INTO WS-STRING.

           WRITE INSU-REP-RECORD FROM WS-STRING.
           WRITE INSU-REP-RECORD FROM WS-DASH.
       4000-TOTAL-AMOUNT-END.

      *    ECRIS LE TOTAL NUMBER RECORDS DANS LE RAPPORT.
       4000-TOTAL-NUMBER-RECORDS.
           INITIALIZE WS-STRING.
           STRING "TOTAL NUMBER RECORDS  :" SPACE
           WS-COUNT-TOTAL
           DELIMITED BY SIZE 
           INTO WS-STRING.

           WRITE INSU-REP-RECORD FROM WS-STRING.
           WRITE INSU-REP-RECORD FROM WS-DASH.
       4000-TOTAL-NUMBER-RECORDS-END.

      *    ECRIS LE NOMBRE TOTAL ACTIVE DANS LE RAPPORT.
       4000-TOTAL-ACTIVE.
           INITIALIZE WS-STRING.
           STRING "TOTAL ACTIVE          :" SPACE WS-COUNT-ACTIVE
           DELIMITED BY SIZE 
           INTO WS-STRING.
           
           WRITE INSU-REP-RECORD FROM WS-STRING.
           WRITE INSU-REP-RECORD FROM WS-DASH.
       4000-TOTAL-ACTIVE-END.

      *    ECRIS LE NOMBRE TOTAL SUSPENDED DANS LE RAPPORT.
       4000-TOTAL-SUSPENDED.
           INITIALIZE WS-STRING.
           STRING "TOTAL SUSPENDED       :" SPACE WS-COUNT-SUSPENDED
           DELIMITED BY SIZE 
           INTO WS-STRING.

           WRITE INSU-REP-RECORD FROM WS-STRING.
           WRITE INSU-REP-RECORD FROM WS-DASH.
       4000-TOTAL-SUSPENDED-END.
      
      *    ECRIS LE NOMBRE TOTAL CANCELED DANS LE RAPPORT.
       4000-TOTAL-CANCELED.
           INITIALIZE WS-STRING.
           STRING "TOTAL CANCELED        :" SPACE WS-COUNT-CANCELED
           DELIMITED BY SIZE 
           INTO WS-STRING.

           WRITE INSU-REP-RECORD FROM WS-STRING.
           WRITE INSU-REP-RECORD FROM WS-DASH.

           CLOSE INSU-REP.
       4000-TOTAL-CANCELED-END.
