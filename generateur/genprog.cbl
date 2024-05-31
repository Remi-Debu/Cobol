      ******************************************************************
      *    Le programme génére un programme COBOL qui contient divers  *
      *    éléments en fonction des options que l'utilisateur a choisi *
      *    (interface utilisateur SCREEN SECTION).                     *
      ****************************************************************** 

       IDENTIFICATION DIVISION.
       PROGRAM-ID. genprog.
       AUTHOR         Rémi.

      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-OUTPUT ASSIGN TO "myprog.cbl"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-OUTPUT.

      ******************************************************************

       DATA DIVISION.
       FILE SECTION.
       FD  F-OUTPUT
           RECORD CONTAINS 72 CHARACTERS
           RECORDING MODE IS F.
       01  R-OUTPUT PIC X(72).

       WORKING-STORAGE SECTION.
       01  FS-OUTPUT PIC X(02).
           88 FS-OUTPUT-OK VALUE "00".

       01  OPTIONS-NUM.
           03 OPTION-1  PIC 9(02).
           03 OPTION-2  PIC 9(02).
           03 OPTION-3  PIC 9(02).
           03 OPTION-4  PIC 9(02).
           03 OPTION-5  PIC 9(02).
           03 OPTION-6  PIC 9(02).
           03 OPTION-7  PIC 9(02).
           03 OPTION-8  PIC 9(02).
           03 OPTION-9  PIC 9(02).
           03 OPTION-10 PIC 9(02).
           03 OPTION-11 PIC 9(02).
           03 OPTION-12 PIC 9(02).
           03 OPTION-13 PIC 9(02).
           03 OPTION-14 PIC 9(02).

       01  WS-FILE-NAME PIC X(10).

       SCREEN SECTION.
       01  SCREEN-OPTIONS FOREGROUND-COLOR 2.
           03 LINE 02 COL 02 
              VALUE "PROGRAM GENERATOR" FOREGROUND-COLOR 3.
           03 LINE 04 COL 02 VALUE "FILE :".
           03 LINE 05 COL 05 VALUE "01. Read a file.".
           03 LINE 05 COL 02 PIC X(02) TO OPTION-1 FOREGROUND-COLOR 7.
           03 LINE 06 COL 05 VALUE "02. Write a file.".
           03 LINE 06 COL 02 PIC X(02) TO OPTION-2 FOREGROUND-COLOR 7.
           03 LINE 07 COL 05 VALUE "03. Sort a file.".
           03 LINE 07 COL 02 PIC X(02) TO OPTION-3 FOREGROUND-COLOR 7.
           03 LINE 08 COL 05 VALUE "04. Merge multiple files.".
           03 LINE 08 COL 02 PIC X(02) TO OPTION-4 FOREGROUND-COLOR 7.
           03 LINE 10 COL 02 VALUE "SQL :".
           03 LINE 11 COL 05 VALUE "05. DB connection.".
           03 LINE 11 COL 02 PIC X(02) TO OPTION-5 FOREGROUND-COLOR 7.
           03 LINE 12 COL 05 VALUE "06. Add a COUNT(*) request.".
           03 LINE 12 COL 02 PIC X(02) TO OPTION-6 FOREGROUND-COLOR 7.
           03 LINE 13 COL 05 VALUE "07. Add a cursor.".
           03 LINE 13 COL 02 PIC X(02) TO OPTION-7 FOREGROUND-COLOR 7.
           03 LINE 14 COL 05 VALUE "08. Add a UPDATE request.".
           03 LINE 14 COL 02 PIC X(02) TO OPTION-8 FOREGROUND-COLOR 7.
           03 LINE 16 COL 02 VALUE "SUBPROGRAM :".
           03 LINE 17 COL 05 VALUE "09. Create a subprogram.".
           03 LINE 17 COL 02 PIC X(02) TO OPTION-9 FOREGROUND-COLOR 7.
           03 LINE 18 COL 05 VALUE "10. Add a CALL statement.".
           03 LINE 18 COL 02 PIC X(02) TO OPTION-10 FOREGROUND-COLOR 7.
           03 LINE 20 COL 02 VALUE "COPYBOOK :".
           03 LINE 21 COL 05 VALUE "11. Create a copybook.".
           03 LINE 21 COL 02 PIC X(02) TO OPTION-11 FOREGROUND-COLOR 7.
           03 LINE 22 COL 05 VALUE "12. Add a COPY statement.".
           03 LINE 22 COL 02 PIC X(02) TO OPTION-12 FOREGROUND-COLOR 7.
           03 LINE 24 COL 02 VALUE "PROCEDURE :".
           03 LINE 25 COL 05 VALUE '13. Add a "Hello Cobol".'.
           03 LINE 25 COL 02 PIC X(02) TO OPTION-13 FOREGROUND-COLOR 7.
           03 LINE 26 COL 05 VALUE "14. Add paragraphs.".
           03 LINE 26 COL 02 PIC X(02) TO OPTION-14 FOREGROUND-COLOR 7.

      ****************************************************************** 

       PROCEDURE DIVISION.
       0000-START-MAIN.
           ACCEPT SCREEN-OPTIONS.
           
           CALL "identdiv".   
           CALL "envirdiv" USING OPTIONS-NUM.
           CALL "datadiv"  USING OPTIONS-NUM.
           CALL "procediv" USING OPTIONS-NUM.
       END-0000-MAIN.
           STOP RUN.
