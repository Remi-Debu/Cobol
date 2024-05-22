      ******************************************************************
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. monprog.
       AUTHOR. Remi.

      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT F-INPUT ASSIGN TO "monfichier.txxt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-INPUT.

           SELECT F-OUTPUT ASSIGN TO "newfichier.txxt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-OUTPUT.

      ******************************************************************

       DATA DIVISION.
       FILE SECTION.
       FD F-OUTPUT
           RECORD CONTAINS 1000 CHARACTERS
           RECORDING MODE IS F.
       01  R-OUTPUT PIC X(1000).

       WORKING-STORAGE SECTION.
       01  FS-OUTPUT PIC X(02).
           88 FS-OUTPUT-OK VALUE "00".
           88 FS-OUTPUT-EOF VALUE "10".

      ******************************************************************

       PROCEDURE DIVISION.
       0000-START-MAIN.
           DISPLAY "HELLO COBOL".
       END-0000-MAIN.
           STOP RUN.

      ******************************************************************
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

      ******************************************************************
       1000-START-init.
      *    Paragraphe...
       END-1000-init.
           EXIT.

      ******************************************************************
       2000-START-read-file.
      *    Paragraphe...
       END-2000-read-file.
           EXIT.
