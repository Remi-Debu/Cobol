      ******************************************************************
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. aaaa.
       AUTHOR. .

      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT ASSIGN TO "aze.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-INPUT.

      ******************************************************************

       DATA DIVISION.
       FILE SECTION.
       FD F-INPUT
           RECORD CONTAINS 145 CHARACTERS
           RECORDING MODE IS V.
       01  R-INPUT PIC X(145).

       WORKING-STORAGE SECTION.
       01  FS-INPUT PIC X(02).
           88 FS-INPUT-OK VALUE "00".
           88 FS-INPUT-EOF VALUE "10".
      ******************************************************************

       PROCEDURE DIVISION.
       0000-START-MAIN.
           DISPLAY "HELLO COBOL".
       END-0000-MAIN.
           STOP RUN.
           START-READ-INPUT.
           OPEN INPUT F-INPUT.
           IF FS-INPUT EQUAL "00"
              SET FS-INPUT-OK TO TRUE

              PERFORM UNTIL FS-INPUT-EOF
                 READ F-INPUT
                 AT END
                    SET FS-INPUT-EOF TO TRUE
                 NOT AT END
      *             Traitement...
                 END-READ
              END-PERFORM
           ELSE
              DISPLAY "ERROR :" SPACE FS-INPUT
           END-IF.
           CLOSE F-INPUT.
           END-READ-INPUT.
           EXIT.
