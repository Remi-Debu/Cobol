       IDENTIFICATION DIVISION.
       PROGRAM-ID. notele.
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
       
       01  WS-BULLETIN.
           03  WS-STUD OCCURS 20 TIMES.
               05 WS-ELEVE      PIC X(14).
               05 WS-MOYENNE-G  PIC 99,99.
               05  WS-COURS OCCURS 6 TIMES.
                   07 WS-LABEL  PIC X(21).
                   07 WS-COEF   PIC 9,9.
                   07 WS-NOTE   PIC 99,99.

       01  WS-I PIC 99 VALUE 1.
       01  WS-J PIC 99 VALUE 1.
       01  WS-TABLE-LENGTH PIC 99.
       01  WS-STRING PIC X(250).
       
       PROCEDURE DIVISION.
           OPEN INPUT F-INPUT
                OUTPUT F-OUTPUT.

           SET F-INPUT-STATUS-OK TO TRUE.
           PERFORM UNTIL F-INPUT-STATUS-EOF
               READ F-INPUT 
               AT END SET F-INPUT-STATUS-EOF TO TRUE
               NOT AT END 
                   IF REC-F-INPUT-2 EQUAL 01
                   STRING FUNCTION TRIM(R-FIRSTNAME) SPACE 
                   FUNCTION TRIM(R-LASTNAME)
                   DELIMITED BY SIZE INTO WS-ELEVE(WS-I)

                   SET WS-J TO 1
                   ADD 1 TO WS-I
                   END-IF

                   IF REC-F-INPUT-2 EQUAL 02
                   SUBTRACT 1 FROM WS-I
                   DISPLAY R-GRADE
                   MOVE R-LABEL TO WS-LABEL(WS-I, WS-J)
                   MOVE R-COEF TO WS-COEF(WS-I, WS-J)
                   MOVE R-GRADE TO WS-NOTE(WS-I, WS-J)
                   DISPLAY WS-NOTE(WS-I, WS-J)

                   ADD 1 TO WS-J
                   ADD 1 TO WS-I
                   END-IF
               END-READ
           END-PERFORM.
       
           OPEN OUTPUT F-OUTPUT.
           
           MOVE WS-I TO WS-TABLE-LENGTH
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I >= WS-TABLE-LENGTH

               PERFORM START-CAL-MOY-G THRU END-CAL-MOY-G
           
               STRING WS-ELEVE(WS-I) SPACE "|" SPACE
                      WS-MOYENNE-G(WS-I) SPACE "|" SPACE 
                      WS-NOTE(WS-I, 1) SPACE "|" SPACE 
                      WS-NOTE(WS-I, 2) SPACE "|" SPACE 
                      WS-NOTE(WS-I, 3) SPACE "|" SPACE
                      WS-NOTE(WS-I, 4) SPACE "|" SPACE
                      WS-NOTE(WS-I, 5) SPACE "|" SPACE
                      WS-NOTE(WS-I, 6)
               DELIMITED BY SIZE
               INTO WS-STRING

               WRITE REC-F-OUTPUT FROM WS-STRING
           END-PERFORM.

           CLOSE F-OUTPUT.

           STOP RUN.

       START-CAL-MOY-G.
      *    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 6
      *        ADD WS-NOTE(WS-I, WS-J) TO WS-MOYENNE-G(WS-I)
      *    END-PERFORM.
           DISPLAY "index j " WS-J.
      *    DIVIDE WS-MOYENNE-G(WS-I) BY WS-J GIVING WS-MOYENNE-G(WS-I).
       END-CAL-MOY-G.
       