      ****************************************************************** 
      *                                                                *
      *    Le programme lis le fichier "assurances.dat" puis ecrit     *
      *    dans un nouveau fichier "rapport-assurances.dat" les        *
      *    enregistrements 3 et 7 de "assurances.dat".                 *
      *    Pour finir il affiche les enregistrements en détails.       *
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
           FILE STATUS IS ASSU1-CODE-STATUS.

      *    SELECT ASSU-FILE2 ASSIGN TO "assurances-part2.dat"
      *    ORGANIZATION IS LINE SEQUENTIAL
      *    ACCESS MODE IS SEQUENTIAL
      *    FILE STATUS IS ASSU2-CODE-STATUS.

           SELECT RAP-ASSU-FILE ASSIGN TO "rapport-assurances.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS RAP-ASSU-CODE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ASSU-FILE1.
       01  ASSU-RECORD PIC X(121).

       FD  RAP-ASSU-FILE.
       01  RAP-ASSU-RECORD PIC X(121).


       WORKING-STORAGE SECTION.
       01  WS-LINE              PIC X(121).
       01  ASSU1-CODE-STATUS    PIC X(02).
       01  RAP-ASSU-CODE-STATUS PIC X(02).
       01  WS-STOP              PIC 9(01) VALUE 0.
       01  WS-INDEX             PIC 9(02) VALUE 1.

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
    
       01  WS-TABLE-RAP-ASSU.
           05 WS-RAP-ASSU PIC X(121) OCCURS 1 TO 99 TIMES
                                     DEPENDING ON WS-INDEX.

       PROCEDURE DIVISION.
           MOVE ALL "-" TO WS-LINE.
           DISPLAY WS-LINE.
           DISPLAY "PROGRAMME ASSURANCES".
           DISPLAY WS-LINE.

      *    ECRITURE
           OPEN OUTPUT RAP-ASSU-FILE.

           DISPLAY WS-LINE.
           DISPLAY "WRITE FILE STATUS :" SPACE RAP-ASSU-CODE-STATUS.

           WRITE RAP-ASSU-RECORD FROM WS-LINE.
           WRITE RAP-ASSU-RECORD
           FROM "RAPPORT ENREGISTREMENT ASSURANCE".
           WRITE RAP-ASSU-RECORD FROM WS-LINE.

           CLOSE RAP-ASSU-FILE.

      *    LECTURE du fichier et stock les donnees dans la table
      *    (séparateur "*")
           OPEN INPUT ASSU-FILE1.
        
           DISPLAY "READ FILE STATUS :" SPACE ASSU1-CODE-STATUS.

           PERFORM UNTIL WS-STOP = 1
               READ ASSU-FILE1
               AT END 
                   SET WS-STOP TO 1
               NOT AT END 
                   MOVE ASSU-RECORD TO WS-RAP-ASSU(WS-INDEX)

                   UNSTRING ASSU-RECORD 
                   DELIMITED BY "*"
                   INTO WS-ID(WS-INDEX) WS-NAME-A(WS-INDEX) 
                   WS-NAME-B(WS-INDEX) WS-NAME-C(WS-INDEX) 
                   WS-STATUS(WS-INDEX) WS-NUM-A(WS-INDEX) 
                   WS-NUM-B(WS-INDEX) WS-AMOUNT(WS-INDEX)
                   WS-EURO(WS-INDEX)

                   ADD 1 TO WS-INDEX
           END-PERFORM.
               
           CLOSE ASSU-FILE1.

           STOP RUN.
