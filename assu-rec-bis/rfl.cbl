      ****************************************************************** 
      *                                                                *
      *    Le programme lis 2 fichiers puis ecrit dans un nouveau      *
      *    fichier les données "Libelle et status" des 2 fichiers,     *
      *    ainsi que le nombre d'enregistrement dans le fichier 1      *
      *    puis 2. Et les occurences des status.                       *
      *                                                                *
      ******************************************************************
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rfl.
       AUTHOR.     Remi.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ASSU-PART1 ASSIGN TO "assurances-part1.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-ASSU1.

           SELECT ASSU-PART2 ASSIGN TO "assurances-part2.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-ASSU2.

           SELECT RAP-ASSU ASSIGN TO "rapport-assurances.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-RAP-ASSU.

       DATA DIVISION.
       FILE SECTION.
       FD  ASSU-PART1.
       01  ASSU-RECORD1 PIC X(123).

       FD  ASSU-PART2.
       01  ASSU-RECORD2 PIC X(123).

       FD  RAP-ASSU.
       01  RAP-ASSU-RECORD PIC X(123).


       WORKING-STORAGE SECTION.
       01  WS-DISPLAY.
           05 WS-DASH      PIC X(200).
           05 WS-AST       PIC X(200).
           05 WS-STRING PIC X(123).

       01  WS-FILE-STATUS.
           05  FS-ASSU1    PIC X(02).
           05  FS-ASSU2    PIC X(02).
           05  FS-RAP-ASSU PIC X(02).

       01  WS-COUNTERS.
           05  WS-COUNT-RECORD1  PIC 9(03) VALUE 0.
           05  WS-COUNT-RECORD2  PIC 9(03) VALUE 0.
           05  WS-COUNT-TOTAL    PIC 9(04) VALUE 0.
           05  WS-COUNT-ACTIF    PIC 9(03) VALUE 0.
           05  WS-COUNT-SUSPENDU PIC 9(03) VALUE 0.
           05  WS-COUNT-RESILIE  PIC 9(03) VALUE 0.

       01  WS-TABLE-ASSU. 
           05 WS-ASSU  OCCURS 1 TO 99 TIMES
                       DEPENDING ON WS-INDEX.
               10 WS-ID     PIC X(08).
               10 WS-NAME-A PIC X(14).
               10 WS-NAME-B PIC X(15).
               10 WS-NAME-C PIC X(41).
               10 WS-STATUS PIC X(09).
               10 WS-NUM-A  PIC X(08).
               10 WS-NUM-B  PIC X(08).
               10 WS-AMOUNT PIC 9(06)V9(02).
               10 WS-EURO   PIC X(03).

       01  WS-STOP  PIC 9(01) VALUE 0.
       01  WS-INDEX PIC 9(02) VALUE 1.
                                  

       PROCEDURE DIVISION.
           MOVE ALL "-" TO WS-DASH.
           MOVE ALL "*" TO WS-AST.

           DISPLAY WS-DASH.
           DISPLAY "PROGRAMME RAPPORT DE SYNTHESE".
           DISPLAY WS-DASH.

      *    HEADER RAPPORT
           OPEN OUTPUT RAP-ASSU.

           DISPLAY "FS WRITE RAP ASSU :" SPACE FS-RAP-ASSU.

           WRITE RAP-ASSU-RECORD FROM WS-AST.
           WRITE RAP-ASSU-RECORD FROM "RAPPORT DE SYNTHESE".
           WRITE RAP-ASSU-RECORD FROM WS-AST.

           CLOSE RAP-ASSU.

      *    SECTION 1
      *    LECTURE 1
           OPEN INPUT ASSU-PART1.
        
           DISPLAY "FS READ ASSU1 :" SPACE FS-ASSU1.

           PERFORM UNTIL WS-STOP = 1
               READ ASSU-PART1
               AT END 
                   SET WS-STOP TO 1
               NOT AT END 
                   UNSTRING ASSU-RECORD1 
                   DELIMITED BY "*"
                   INTO WS-ID(WS-INDEX) WS-NAME-A(WS-INDEX) 
                   WS-NAME-B(WS-INDEX) WS-NAME-C(WS-INDEX) 
                   WS-STATUS(WS-INDEX) WS-NUM-A(WS-INDEX) 
                   WS-NUM-B(WS-INDEX) WS-AMOUNT(WS-INDEX)
                   WS-EURO(WS-INDEX)

                   EVALUATE WS-STATUS(WS-INDEX)
                       WHEN "Actif"
                           ADD 1 TO WS-COUNT-ACTIF
                       WHEN "Suspendu"
                          ADD 1 TO WS-COUNT-SUSPENDU
                       WHEN "Resilie"
                          ADD 1 TO WS-COUNT-RESILIE
                       WHEN "Resilié"
                          ADD 1 TO WS-COUNT-RESILIE
                       WHEN OTHER
                          CONTINUE
                   END-EVALUATE
                
                   ADD 1 TO WS-INDEX
                   ADD 1 TO WS-COUNT-RECORD1
           END-PERFORM.           
           CLOSE ASSU-PART1.

      *    ECRITURE 1
           OPEN EXTEND RAP-ASSU.

           DISPLAY "FS WRITE RAP ASSU :" SPACE FS-RAP-ASSU.

           WRITE RAP-ASSU-RECORD FROM WS-AST.
           WRITE RAP-ASSU-RECORD FROM "SECTION FICHIER 1".
           WRITE RAP-ASSU-RECORD FROM WS-AST.

           SET WS-INDEX TO 1.
           PERFORM WS-COUNT-RECORD1 TIMES
               STRING "LIBELLE :" SPACE WS-NAME-C(WS-INDEX) SPACE 
               "STATUS :" SPACE WS-STATUS(WS-INDEX)
               DELIMITED BY SIZE 
               INTO WS-STRING

               WRITE RAP-ASSU-RECORD FROM WS-STRING
               WRITE RAP-ASSU-RECORD FROM WS-DASH 

               ADD 1 TO WS-INDEX
           END-PERFORM.

           CLOSE RAP-ASSU.

      *    SECTION 2
      *    LECTURE 2
           OPEN INPUT ASSU-PART2.
        
           DISPLAY "FS READ ASSU2 :" SPACE FS-ASSU2.

           SET WS-INDEX TO 1.
           INITIALIZE WS-TABLE-ASSU.
           INITIALIZE WS-STOP.
           PERFORM UNTIL WS-STOP = 1
               READ ASSU-PART2
               AT END 
                   SET WS-STOP TO 1
               NOT AT END 
                   UNSTRING ASSU-RECORD2
                   DELIMITED BY "*"
                   INTO WS-ID(WS-INDEX) WS-NAME-A(WS-INDEX) 
                   WS-NAME-B(WS-INDEX) WS-NAME-C(WS-INDEX) 
                   WS-STATUS(WS-INDEX) WS-NUM-A(WS-INDEX) 
                   WS-NUM-B(WS-INDEX) WS-AMOUNT(WS-INDEX)
                   WS-EURO(WS-INDEX)

                   EVALUATE WS-STATUS(WS-INDEX)
                       WHEN "Actif"
                           ADD 1 TO WS-COUNT-ACTIF
                       WHEN "Suspendu"
                          ADD 1 TO WS-COUNT-SUSPENDU
                       WHEN "Resilie"
                          ADD 1 TO WS-COUNT-RESILIE
                       WHEN "Resilié"
                          ADD 1 TO WS-COUNT-RESILIE
                       WHEN OTHER
                          CONTINUE
                   END-EVALUATE

                   ADD 1 TO WS-INDEX
                   ADD 1 TO WS-COUNT-RECORD2
           END-PERFORM.           
           CLOSE ASSU-PART2.

      *    ECRITURE 2
           OPEN EXTEND RAP-ASSU.

           DISPLAY "FS WRITE RAP ASSU :" SPACE FS-RAP-ASSU.

           WRITE RAP-ASSU-RECORD FROM WS-AST.
           WRITE RAP-ASSU-RECORD FROM "SECTION FICHIER 2".
           WRITE RAP-ASSU-RECORD FROM WS-AST.

           SET WS-INDEX TO 1.
           PERFORM WS-COUNT-RECORD2 TIMES
               STRING "LIBELLE :" SPACE WS-NAME-C(WS-INDEX) SPACE 
               "STATUS :" SPACE WS-STATUS(WS-INDEX)
               DELIMITED BY SIZE 
               INTO WS-STRING

               WRITE RAP-ASSU-RECORD FROM WS-STRING

               WRITE RAP-ASSU-RECORD FROM WS-DASH 
               ADD 1 TO WS-INDEX
           END-PERFORM.

      *    FOOTER RAPPORT
           WRITE RAP-ASSU-RECORD FROM WS-AST.

      *    NOMBRE D'ENREGISTREMENTS SECTION 1
           INITIALIZE WS-STRING.
           STRING "Nombre d'enregistrements section 1 :" SPACE
           WS-COUNT-RECORD1
           DELIMITED BY SIZE 
           INTO WS-STRING.

           WRITE RAP-ASSU-RECORD FROM WS-STRING.

      *    NOMBRE D'ENREGISTREMENTS SECTION 2
           INITIALIZE WS-STRING.
           STRING "Nombre d'enregistrements section 2 :" SPACE
           WS-COUNT-RECORD2
           DELIMITED BY SIZE 
           INTO WS-STRING.

           WRITE RAP-ASSU-RECORD FROM WS-STRING.

      *    NOMBRE TOTAL D'ENREGISTREMENTS
           COMPUTE WS-COUNT-TOTAL = WS-COUNT-RECORD1 + WS-COUNT-RECORD2.

           INITIALIZE WS-STRING.
           STRING "Nombre total d'enregistrements :" SPACE
           WS-COUNT-TOTAL
           DELIMITED BY SIZE 
           INTO WS-STRING.

           WRITE RAP-ASSU-RECORD FROM WS-STRING.
           WRITE RAP-ASSU-RECORD FROM WS-AST.

      *    QUANTITE DE STATUS ACTIF
           INITIALIZE WS-STRING.
           STRING "Total d'actif :" SPACE WS-COUNT-ACTIF
           DELIMITED BY SIZE 
           INTO WS-STRING.

           WRITE RAP-ASSU-RECORD FROM WS-STRING.

      *    QUANTITE DE STATUS SUSPENDU
           INITIALIZE WS-STRING.
           STRING "Total de suspendu :" SPACE WS-COUNT-SUSPENDU
           DELIMITED BY SIZE 
           INTO WS-STRING.

           WRITE RAP-ASSU-RECORD FROM WS-STRING.

      *    QUANTITE DE STATUS RESILIE
           INITIALIZE WS-STRING.
           STRING "Total de resilie :" SPACE WS-COUNT-RESILIE
           DELIMITED BY SIZE 
           INTO WS-STRING.

           WRITE RAP-ASSU-RECORD FROM WS-STRING.
           WRITE RAP-ASSU-RECORD FROM WS-AST.

           CLOSE RAP-ASSU.

           STOP RUN.
