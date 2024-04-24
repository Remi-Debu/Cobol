       IDENTIFICATION DIVISION.
       PROGRAM-ID. train.
       AUTHOR.     Remi.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRAIN1 ASSIGN TO "train1.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

           SELECT TRAIN3 ASSIGN TO "train3.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TRAIN1
           RECORD CONTAINS 27 TO 37 CHARACTERS
           RECORDING MODE IS V.   
       01  RTRAIN1 PIC X(37).
       COPY train1fd.

       FD  TRAIN3
           RECORD CONTAINS 33 TO 43 CHARACTERS
           RECORDING MODE IS F.   
       01  RTRAIN3 PIC X(43).

       WORKING-STORAGE SECTION.
       COPY train3fd.

       01  WS-DISPLAY.
           03 WS-AST PIC X(80).
           03 WS-STRING PIC X(50).

       01  TRAIN1-LENGTH PIC 9(02).
       01  TRAIN3-LENGTH PIC 9(02).

       01  WS-COUNTER.
           03  WS-COUNT-READ  PIC 9(02).
           03  WS-COUNT-WRITE PIC 9(02).

       01  WS-STOP PIC 9(01).
       01  WS-INDEX PIC 9(02).

       PROCEDURE DIVISION.
           MOVE ALL "*" TO WS-AST.

       TRAIN-READWRITE.
           OPEN INPUT TRAIN1
                OUTPUT TRAIN3.

           SET WS-STOP TO 0.
           PERFORM UNTIL WS-STOP = 1
              READ TRAIN1
                  AT END
                 SET WS-STOP TO 1
              NOT AT END
                 ADD 1 TO WS-COUNT-READ
       
                 MOVE CORR TRAIN-PLANNING TO TRAIN-PLANNING-DETAILS
       
                 PERFORM NB-ARRET THRU NB-ARRET-END
                 
                 WRITE RTRAIN3 FROM TRAIN-PLANNING-DETAILS
           END-PERFORM.

           CLOSE TRAIN1
                 TRAIN3.
       TRAIN-READWRITE-END.

           OPEN INPUT TRAIN3.

       TRAIN3-WRITE-COUNT.
           SET WS-STOP TO 0.
           PERFORM UNTIL WS-STOP = 1
              READ TRAIN3
              AT END
                 SET WS-STOP TO 1
              NOT AT END
                 ADD 1 TO WS-COUNT-WRITE
           END-PERFORM.

           CLOSE TRAIN3.
       TRAIN3-WRITE-COUNT.

       DISPLAY-READ-RECORD.
           STRING WS-COUNT-READ SPACE 
           "enregistrements lus dans train1."    
           DELIMITED BY SIZE INTO WS-STRING
           DISPLAY WS-STRING.

           INITIALIZE WS-STRING.
           STRING WS-COUNT-WRITE SPACE
           "enregistrements ecris dans train3."
           DELIMITED BY SIZE INTO WS-STRING
           DISPLAY WS-STRING.
       DISPLAY-READ-RECORD-END.

           STOP RUN.

       NB-ARRET.
           SET WS-INDEX TO 0
           SET TRAIN-NBRE-ARRET TO 0
           PERFORM UNTIL WS-INDEX > 10
               IF TRAIN-STOPS-HERE(WS-INDEX)
               ADD 1 TO TRAIN-NBRE-ARRET
               END-IF

               ADD 1 TO WS-INDEX
           END-PERFORM.
       NB-ARRET-END.
