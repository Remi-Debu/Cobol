      ******************************************************************
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. programID.
       AUTHOR         Rémi.

      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT ASSIGN TO "filename.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-INPUT.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 100 CHARACTERS
           RECORDING MODE IS F.
       01  R-NAME PIC X(100).

       WORKING-STORAGE SECTION.
       01  FS-INPUT PIC X(02).
           88 FS-INPUT-OK VALUE "00".
           88 FS-INPUT-EOF VALUE "10".

      ****************************************************************** 
       PROCEDURE DIVISION.
      ******************************************************************
       START-R-IP.
           OPEN INPUT F-INPUT.
           IF FS-INPUT EQUAL "00"
              SET FS-INPUT-OK TO TRUE

              PERFORM UNTIL FS-INPUT-EOF
                 READ F-INPUT 
                 AT END 
                    SET FS-INPUT-EOF TO TRUE
                 NOT AT END 
    
                  END-READ
              END-PERFORM
           ELSE
              DISPLAY "ERREUR :" SPACE FS-INPUT
           END-IF.
           CLOSE F-INPUT.
       END-R-IP.
           EXIT.

           STOP RUN.
           