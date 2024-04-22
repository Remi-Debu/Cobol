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
       01  RTRAIN1 PIC X(37).

       FD  TRAIN3
           RECORD IS VARYING IN SIZE FROM 14 TO 37 CHARACTERS
                DEPENDING ON TRAIN3-LENGTH
           RECORDING MODE IS V.   
       01  RTRAIN3 PIC X(37).


       WORKING-STORAGE SECTION.
       COPY train1fd.

       01  TRAIN3-LENGTH PIC 9(02).
       01  WS-STOP       PIC 9(01).

       PROCEDURE DIVISION.
           OPEN INPUT TRAIN1
                OUTPUT TRAIN3.

           SET WS-STOP TO 0.
           PERFORM UNTIL WS-STOP = 1
               READ TRAIN1
               AT END
                   SET WS-STOP TO 1
               NOT AT END
               MOVE RTRAIN1 TO TRAIN-PLANNING
               DISPLAY TRAIN3-LENGTH
           END-PERFORM.

           CLOSE TRAIN1
                 TRAIN3.
       