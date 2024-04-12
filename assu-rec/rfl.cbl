      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                                       *
      *    Le programme lis le fichier "assurances.dat" puis ecrit dans       *
      *    un nouveau fichier "rapport-assurances.dat" les                    *
      *    enregistrements 3 et 7 de "assurances.dat".                        *
      *    Pour finir il affiche les enregistrements en détails.              *
      *                                                                       *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rfl.
       AUTHOR.     Remi.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ASSU-FILE ASSIGN TO "assurances.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ASSU-CODE-STATUS.

           SELECT RAP-ASSU-FILE ASSIGN TO "rapport-assurances.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS RAP-ASSU-CODE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ASSU-FILE.
       01  ASSU-RECORD PIC X(121).

       FD  RAP-ASSU-FILE.
       01  RAP-ASSU-RECORD PIC X(121).


       WORKING-STORAGE SECTION.
       01  ASSU-CODE-STATUS     PIC X(02).
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
      *    LECTURE du fichier et stock les donnees dans la table
      *    (séparateur "*")
           OPEN INPUT ASSU-FILE.
           
           DISPLAY "--------------------------------------------------".
           DISPLAY "READ FILE STATUS :" SPACE ASSU-CODE-STATUS.

           PERFORM UNTIL WS-STOP = 1
               READ ASSU-FILE
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
               
           CLOSE ASSU-FILE.

      *    ECRITURE des données 3 et 7
           OPEN OUTPUT RAP-ASSU-FILE.

           DISPLAY "--------------------------------------------------".
           DISPLAY "WRITE FILE STATUS :" SPACE RAP-ASSU-CODE-STATUS.

           WRITE RAP-ASSU-RECORD FROM WS-RAP-ASSU(3).
           WRITE RAP-ASSU-RECORD FROM WS-RAP-ASSU(7).
           CLOSE RAP-ASSU-FILE.

      *    AFFICHAGE des données 3 et 7
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
