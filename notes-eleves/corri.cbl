       IDENTIFICATION DIVISION.
       PROGRAM-ID. corri.
       AUTHOR.     Rémi.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT ASSIGN TO "input.dat"
           ACCESS MODE IS SEQUENTIAL
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS F-INPUT-STATUS.

           SELECT F-OUTPUT ASSIGN TO "output.dat"
           ACCESS MODE IS SEQUENTIAL
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS F-OUTPUT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS
           RECORDING MODE IS V.
       01  REC-F-INPUT-2    PIC 9(02).
       01  REC-F-INPUT-10   PIC X(10).
       01  REC-F-INPUT-100  PIC X(100).
       01  REC-F-INPUT-1000 PIC X(1000).

       01  REC-STUD.
           03 R-S-KEY       PIC 9(02).
           03 R-LASTNAME    PIC X(07).
           03 R-FIRSTNAME   PIC X(06).   
           03 R-AGE         PIC 9(02).

       01  REC-COURSE.
           03 R-C-KEY       PIC 9(02).
           03 R-LABEL       PIC X(21).
           03 R-COEF        PIC 9V9.
           03 R-GRADE       PIC 99V99.

       FD  F-OUTPUT
           RECORD CONTAINS 250 CHARACTERS
           RECORDING MODE IS F.
       01  REC-F-OUTPUT     PIC X(250).

       WORKING-STORAGE SECTION.
       01  F-INPUT-STATUS  PIC X(02).
           88 F-INPUT-STATUS-OK  VALUE "0".
           88 F-INPUT-STATUS-EOF VALUE "10".
           
       01  F-OUTPUT-STATUS PIC X(02).
           88 F-OUTPUT-STATUS-OK  VALUE "0".
           88 F-OUTPUT-STATUS-EOF VALUE "10".

       PROCEDURE DIVISION.
           OPEN INPUT F-INPUT.

           SET F-INPUT-STATUS-OK TO TRUE.
           PERFORM UNTIL F-INPUT-STATUS-EOF
               READ F-INPUT 
               AT END SET F-INPUT-STATUS-EOF TO TRUE
               NOT AT END 
                   DISPLAY R-COEF
               END-READ
           END-PERFORM.
           STOP RUN.
       