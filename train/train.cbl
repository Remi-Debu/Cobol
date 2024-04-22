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
           RECORD CONTAINS 37 CHARACTERS
           RECORDING MODE IS F.   
       COPY TRAIN1FD.
      *01  RTRAIN1        PIC X(37).

       FD  TRAIN3
           RECORD IS VARYING IN SIZE FROM 14 TO 43 CHARACTERS
                DEPENDING ON TRAIN3-LENGTH
           RECORDING MODE IS V.   
       01  RTRAIN3        PIC X(43).


       WORKING-STORAGE SECTION.
      *COPY TRAIN1FD.
       COPY TRAIN3FD.

       01  WS-DISPLAY.
           03 WS-AST PIC X(80).
           03 WS-STRING PIC X(50).

       01  TRAIN3-LENGTH PIC 9(02).

       01  WS-COUNTER.
           03  WS-COUNT-READ  PIC 9(02).
           03  WS-COUNT-WRITE PIC 9(02).

       01  WS-STOP PIC 9(01).
       01  WS-INDEX PIC 9(02).

       PROCEDURE DIVISION.
           MOVE ALL "*" TO WS-AST.

           OPEN INPUT TRAIN1
                OUTPUT TRAIN3.

           SET WS-STOP TO 0.
           PERFORM UNTIL WS-STOP = 1
              READ TRAIN1
              AT END
                 SET WS-STOP TO 1
              NOT AT END
                 ADD 1 TO WS-COUNT-READ

                 SET TRAIN3-LENGTH 
                 TO FUNCTION LENGTH(TRAIN-PLANNING)

                 

                 WRITE RTRAIN3 FROM TRAIN-PLANNING
           END-PERFORM.

           CLOSE TRAIN1
                 TRAIN3.

           OPEN INPUT TRAIN3.

           SET WS-STOP TO 0.
           PERFORM UNTIL WS-STOP = 1
              READ TRAIN3
              AT END
                 SET WS-STOP TO 1
              NOT AT END
                 ADD 1 TO WS-COUNT-WRITE
                 
      *          PERFORM 1000-TEST-PARA THRU 1000-TEST-PARA-END
                 
           END-PERFORM.

           CLOSE TRAIN3.

           STRING WS-COUNT-READ SPACE 
           "enregistrements lus dans train1."    
           DELIMITED BY SIZE INTO WS-STRING
           DISPLAY WS-STRING

           INITIALIZE WS-STRING.
           STRING WS-COUNT-WRITE SPACE
           "enregistrements ecris dans train3."
           DELIMITED BY SIZE INTO WS-STRING
           DISPLAY WS-STRING

           STOP RUN.

      *1000-TEST-PARA.
      *    PERFORM UNTIL WS-INDEX > 10
      *       IF TRAIN-HALT-FLAG(WS-INDEX) IN TRAIN-PLANNING-DETAILS
      *             DISPLAY TRAIN-STOPS-HERE(WS-INDEX)
      *             IN TRAIN-PLANNING-DETAILS
      *                ADD 1 TO TRAIN-NBRE-ARRET
      *       END-IF
      *    END-PERFORM.
      *1000-TEST-PARA-END.
