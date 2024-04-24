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
           03 R-COEF        PIC 9(01)B9(01).
           03 R-GRADE       PIC 9(02)B9(02).

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
               05 WS-MOYENNE-G  PIC Z9,99.
               05  WS-COURS OCCURS 6 TIMES.
                   07 WS-LABEL  PIC X(21).
                   07 WS-COEF   PIC 9,9.
                   07 WS-NOTE   PIC 99,99.

       01  WS-I            PIC 99 VALUE 1.
       01  WS-J            PIC 99 VALUE 1.
       01  WS-TABLE-LENGTH PIC 99.
       01  WS-REC-STRING   PIC X(250).
       01  WS-NOTE-TEMP    PIC 99V99.
       01  WS-MOY-TEMP     PIC 99V99.
       01  WS-NB-ELEV      PIC Z9.
       01  WS-CLASS-MOY-G  PIC Z9V99.
       
       PROCEDURE DIVISION.
           OPEN INPUT F-INPUT.

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

                   MOVE R-LABEL TO WS-LABEL(WS-I, WS-J)
                   MOVE R-COEF TO WS-COEF(WS-I, WS-J)
                   MOVE R-GRADE TO WS-NOTE(WS-I, WS-J)

                   ADD 1 TO WS-J
                   ADD 1 TO WS-I
                   END-IF
               END-READ
           END-PERFORM.

           CLOSE F-INPUT.
       
           OPEN OUTPUT F-OUTPUT.
           
           COMPUTE WS-TABLE-LENGTH = WS-I - 1
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > WS-TABLE-LENGTH

               PERFORM START-STUD-MOY-G THRU END-STUD-MOY-G
           
               STRING WS-ELEVE(WS-I) SPACE "|" SPACE
                      WS-MOYENNE-G(WS-I) SPACE "|" SPACE 
                      WS-NOTE(WS-I, 1) SPACE "|" SPACE 
                      WS-NOTE(WS-I, 2) SPACE "|" SPACE 
                      WS-NOTE(WS-I, 3) SPACE "|" SPACE
                      WS-NOTE(WS-I, 4) SPACE "|" SPACE
                      WS-NOTE(WS-I, 5) SPACE "|" SPACE
                      WS-NOTE(WS-I, 6)
               DELIMITED BY SIZE
               INTO WS-REC-STRING

               WRITE REC-F-OUTPUT FROM WS-REC-STRING

           END-PERFORM.

           PERFORM START-NB-ELEV THRU END-NB-ELEV.
           PERFORM START-CLASS-MOY-G THRU END-CLASS-MOY-G.

           CLOSE F-OUTPUT.

           STOP RUN.

       START-STUD-MOY-G.
           INITIALIZE WS-MOY-TEMP.

           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 6
               INITIALIZE WS-NOTE-TEMP
               MOVE WS-NOTE(WS-I, WS-J) TO WS-NOTE-TEMP
               ADD WS-NOTE-TEMP TO WS-MOY-TEMP
           END-PERFORM.

           SUBTRACT 1 FROM WS-J GIVING WS-J
           DIVIDE WS-MOY-TEMP BY WS-J GIVING WS-MOY-TEMP.
           MOVE WS-MOY-TEMP TO WS-MOYENNE-G(WS-I).
       END-STUD-MOY-G.

       START-NB-ELEV.
           INITIALIZE WS-REC-STRING.

           MOVE WS-TABLE-LENGTH TO WS-NB-ELEV.
           STRING "Nombre d'eleve :" SPACE WS-NB-ELEV
           DELIMITED BY SIZE
           INTO WS-REC-STRING.
           
           WRITE REC-F-OUTPUT FROM SPACE.
           WRITE REC-F-OUTPUT FROM WS-REC-STRING.
       END-NB-ELEV.

       START-CLASS-MOY-G.
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > WS-TABLE-LENGTH
           ADD WS-MOYENNE-G(WS-I) TO WS-MOY-TEMP
           END-PERFORM.

           DIVIDE WS-MOY-TEMP BY WS-TABLE-LENGTH 
           GIVING WS-CLASS-MOY-G.

           WRITE REC-F-OUTPUT FROM WS-CLASS-MOY-G.
       END-CLASS-MOY-G.
       