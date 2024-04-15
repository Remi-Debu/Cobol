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
           SELECT ASSU-FILE1 ASSIGN TO "assurances-part1.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-CODE-ASSU1.

           SELECT ASSU-FILE2 ASSIGN TO "assurances-part2.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-CODE-ASSU2.

           SELECT RAP-ASSU-FILE ASSIGN TO "rapport-assurances.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-CODE-RAP-ASSU.

       DATA DIVISION.
       FILE SECTION.
       FD  ASSU-FILE1.
       01  ASSU-RECORD1 PIC X(123).

       FD  ASSU-FILE2.
       01  ASSU-RECORD2 PIC X(123).

       FD  RAP-ASSU-FILE.
       01  RAP-ASSU-RECORD PIC X(123).


       WORKING-STORAGE SECTION.
       01  WS-DISPLAY.
           05 WS-LINE      PIC X(200).
           05 WS-AST       PIC X(200).
           05 WS-WRITE-RAP PIC X(123).

       01  WS-CODE-STATUS.
           05  WS-CODE-ASSU1    PIC X(02).
           05  WS-CODE-ASSU2    PIC X(02).
           05  WS-CODE-RAP-ASSU PIC X(02).

       01  WS-COUNTER.
           05  WS-COUNT-RECORD1 PIC 9(03) VALUE 0.
           05  WS-COUNT-RECORD2 PIC 9(03) VALUE 0.
           05  WS-COUNT-TOTAL   PIC 9(04) VALUE 0.
           05  WS-ACTIF         PIC 9(03) VALUE 0.
           05  WS-SUSPENDU      PIC 9(03) VALUE 0.
           05  WS-RESILIE       PIC 9(03) VALUE 0.

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
    
       01  WS-TABLE-RAP-ASSU.
           05 WS-RAP-ASSU OCCURS 1 TO 99 TIMES
                          DEPENDING ON WS-INDEX.
               10  WS-RAP-NAME-B PIC X(41)   .          
               10  WS-RAP-STATUS PIC X(09).

       01  WS-STOP  PIC 9(01) VALUE 0.
       01  WS-INDEX PIC 9(02) VALUE 1.
                                  

       PROCEDURE DIVISION.
           MOVE ALL "-" TO WS-LINE.
           MOVE ALL "*" TO WS-AST.
           DISPLAY WS-LINE.
           DISPLAY "PROGRAMME ASSURANCES".
           DISPLAY WS-LINE.

      *    HEADER RAPPORT
           OPEN OUTPUT RAP-ASSU-FILE.

           DISPLAY "WRITE FILE STATUS :" SPACE WS-CODE-RAP-ASSU.

           WRITE RAP-ASSU-RECORD FROM WS-AST.
           WRITE RAP-ASSU-RECORD
           FROM "RAPPORT ENREGISTREMENT ASSURANCE".
           WRITE RAP-ASSU-RECORD FROM WS-AST.

           CLOSE RAP-ASSU-FILE.

      *    SECTION 1
      *    LECTURE 1
           OPEN INPUT ASSU-FILE1.
        
           DISPLAY "READ FILE STATUS :" SPACE WS-CODE-ASSU1.

           PERFORM UNTIL WS-STOP = 1
               READ ASSU-FILE1
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

                   MOVE WS-NAME-C(WS-INDEX) TO WS-RAP-NAME-B(WS-INDEX)
                   MOVE WS-STATUS(WS-INDEX) TO WS-RAP-STATUS(WS-INDEX)

                   EVALUATE WS-STATUS(WS-INDEX)
                       WHEN "Actif"
                           ADD 1 TO WS-ACTIF
                       WHEN "Suspendu"
                          ADD 1 TO WS-SUSPENDU
                       WHEN "Resilie"
                          ADD 1 TO WS-RESILIE
                       WHEN "Resilié"
                          ADD 1 TO WS-RESILIE
                       WHEN OTHER
                          CONTINUE
                   END-EVALUATE
                
                   ADD 1 TO WS-INDEX
                   ADD 1 TO WS-COUNT-RECORD1
           END-PERFORM.           
           CLOSE ASSU-FILE1.

           DISPLAY WS-ACTIF.
           DISPLAY WS-SUSPENDU.
           DISPLAY WS-RESILIE.

      *    ECRITURE 1
           OPEN EXTEND RAP-ASSU-FILE.

           DISPLAY "WRITE FILE STATUS :" SPACE WS-CODE-RAP-ASSU.

           WRITE RAP-ASSU-RECORD FROM WS-AST.
           WRITE RAP-ASSU-RECORD FROM "SECTION FICHIER 1".
           WRITE RAP-ASSU-RECORD FROM WS-AST.

           INITIALIZE WS-INDEX.
           PERFORM WS-COUNT-RECORD1 TIMES
               DISPLAY "WS-COUNT-RECORD1 : " WS-COUNT-RECORD1
               DISPLAY "WS-INDEX : " WS-INDEX

               STRING "LIBELLE :" SPACE WS-RAP-NAME-B(WS-INDEX) SPACE 
               "STATUS :" SPACE WS-RAP-STATUS(WS-INDEX)
               DELIMITED BY SIZE 
               INTO WS-WRITE-RAP

               WRITE RAP-ASSU-RECORD FROM WS-WRITE-RAP

               WRITE RAP-ASSU-RECORD FROM WS-LINE 
               ADD 1 TO WS-INDEX
           END-PERFORM.

           CLOSE RAP-ASSU-FILE.

      *    SECTION 2
      *    LECTURE 2
           OPEN INPUT ASSU-FILE2.
        
           DISPLAY "READ FILE STATUS :" SPACE WS-CODE-ASSU2.

           INITIALIZE WS-INDEX.
           INITIALIZE WS-TABLE-ASSU.
           INITIALIZE WS-STOP.
           PERFORM UNTIL WS-STOP = 1
               READ ASSU-FILE2
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

                   MOVE WS-NAME-C(WS-INDEX) TO WS-RAP-NAME-B(WS-INDEX)
                   MOVE WS-STATUS(WS-INDEX) TO WS-RAP-STATUS(WS-INDEX)

                   ADD 1 TO WS-INDEX
                   ADD 1 TO WS-COUNT-RECORD2
           END-PERFORM.           
           CLOSE ASSU-FILE2.

      *    ECRITURE 2
           OPEN EXTEND RAP-ASSU-FILE.

           DISPLAY "WRITE FILE STATUS :" SPACE WS-CODE-RAP-ASSU.

           WRITE RAP-ASSU-RECORD FROM WS-AST.
           WRITE RAP-ASSU-RECORD FROM "SECTION FICHIER 2".
           WRITE RAP-ASSU-RECORD FROM WS-AST.

           INITIALIZE WS-INDEX.
           PERFORM WS-COUNT-RECORD2 TIMES
               STRING "LIBELLE :" SPACE WS-RAP-NAME-B(WS-INDEX) SPACE 
               "STATUS :" SPACE WS-RAP-STATUS(WS-INDEX)
               DELIMITED BY SIZE 
               INTO WS-WRITE-RAP

               WRITE RAP-ASSU-RECORD FROM WS-WRITE-RAP

               WRITE RAP-ASSU-RECORD FROM WS-LINE 
               ADD 1 TO WS-INDEX
           END-PERFORM.

      *    FOOTER RAPPORT
           WRITE RAP-ASSU-RECORD FROM WS-AST.

      *    NOMBRE D'ENREGISTREMENTS SECTION 1
           INITIALIZE WS-WRITE-RAP.
           STRING "Nombre d'enregistrements de la section 1 :" SPACE
           WS-COUNT-RECORD1
           DELIMITED BY SIZE 
           INTO WS-WRITE-RAP.

           WRITE RAP-ASSU-RECORD FROM WS-WRITE-RAP.

      *    NOMBRE D'ENREGISTREMENTS SECTION 2
           INITIALIZE WS-WRITE-RAP.
           STRING "Nombre d'enregistrements de la section 2 :" SPACE
           WS-COUNT-RECORD2
           DELIMITED BY SIZE 
           INTO WS-WRITE-RAP.

           WRITE RAP-ASSU-RECORD FROM WS-WRITE-RAP.

      *    NOMBRE TOTAL D'ENREGISTREMENTS
           COMPUTE WS-COUNT-TOTAL = WS-COUNT-RECORD1 + WS-COUNT-RECORD2.

           INITIALIZE WS-WRITE-RAP.
           STRING "Nombre total d'enregistrements :" SPACE
           WS-COUNT-TOTAL
           DELIMITED BY SIZE 
           INTO WS-WRITE-RAP.

           WRITE RAP-ASSU-RECORD FROM WS-WRITE-RAP.
           WRITE RAP-ASSU-RECORD FROM WS-AST.

           CLOSE RAP-ASSU-FILE.

           STOP RUN.
