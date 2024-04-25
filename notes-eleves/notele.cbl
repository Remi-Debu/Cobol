       IDENTIFICATION DIVISION.
       PROGRAM-ID. notele.
       AUTHOR.     Rémi.

      ******************************************************************
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
      
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS
           RECORDING MODE IS V.
       01  REC-F-INPUT-2    PIC X(02).

       01  REC-STUDENT.
           03 R-S-KEY       PIC 9(02).
           03 R-S-LASTNAME  PIC X(07).
           03 R-S-FIRSTNAME PIC X(06).   
           03 R-S-AGE       PIC 9(02).

       01  REC-COURSE.
           03 R-C-KEY       PIC 9(02).
           03 R-C-LABEL     PIC X(21).
           03 R-C-COEF      PIC X(03).
           03 R-C-GRADE     PIC X(05).

       FD  F-OUTPUT
           RECORD CONTAINS 2000 CHARACTERS
           RECORDING MODE IS F.
       01  REC-F-OUTPUT     PIC X(2000).

       WORKING-STORAGE SECTION.
       01  F-INPUT-STATUS  PIC X(02).
           88 F-INPUT-STATUS-OK  VALUE "0".
           88 F-INPUT-STATUS-EOF VALUE "10".
           
       01  F-OUTPUT-STATUS PIC X(02).
           88 F-OUTPUT-STATUS-OK  VALUE "0".
           88 F-OUTPUT-STATUS-EOF VALUE "10".
       
       01  WS-TABLE-STUDENT.
           03  WS-S-CNT  PIC 9(03) VALUE 1.
           03  WS-STUDENT OCCURS 1 TO 200 TIMES
                          DEPENDING ON WS-S-CNT
                          INDEXED BY S-IDX.
               05 WS-S-FIRSTNAME      PIC X(20).
               05 WS-S-LASTNAME       PIC X(20).
               05 WS-S-AGE            PIC 9(02).
               05 WS-S-SUM-GRADE-COEF PIC 9(05)V9(02).
               05 WS-S-AV-GRADE       PIC 99V99.
       
       01  WS-TABLE-COURSE.
           03 WS-C-CNT  PIC 9(03) VALUE 1.
           03 WS-COURSE OCCURS 1 TO 200 TIMES
                        DEPENDING ON WS-C-CNT
                        INDEXED BY C-IDX.
               05 WS-C-LABEL PIC X(21).
               05 WS-C-COEF  PIC 9V9.

       01  WS-TABLE-GRADE.
           03 WS-G-CNT PIC 9(03) VALUE 1.
           03 WS-GRADE OCCURS 1 TO 200 TIMES
                       DEPENDING ON WS-G-CNT
                       INDEXED BY G-IDX.
               05 WS-G-S-FULLNAME PIC X(30).
               05 WS-G-C-LABEL    PIC X(25).
               05 WS-G-GRADE      PIC 99V99.
               05 WS-G-COEF       PIC 9V9.

       01  WS-IS-EXIST PIC X.
           88 WS-IS-EXIST-Y VALUE "Y".
           88 WS-IS-EXIST-N VALUE "N".

       01  WS-PNT.
           03 WS-PNT-NBR      PIC ZZZZ9.
           03 WS-PNT-GRADE    PIC Z9,99.
           03 WS-PNT-COEF     PIC 9,9.

       01  WS-I               PIC 9(03) VALUE 1.
       01  WS-J               PIC 9(03) VALUE 1.
              
       01  WS-STRING-POS      PIC 9(03) VALUE 1.
       01  WS-NUM-TEMP        PIC 9(03)V9(02).
       01  WS-FULLNAME-TEMP   PIC X(30).
       01  WS-SUM-COEF        PIC 9(10)V9.
       01  WS-SUM-GRADE       PIC 9(10)V99.

       01  WS-DISPLAY.
           03 WS-DASH    PIC X(235).
           03 WS-BLANK   PIC X(100).
           03 WS-EMPTY   PIC X.
           03 WS-STRING1 PIC X(235).
           03 WS-STRING2 PIC X(235).

      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM START-R-IP THRU END-R-IP.
           PERFORM START-W-OP THRU END-W-OP.
           STOP RUN.

      ******************************************************************
       START-R-IP.
           OPEN INPUT F-INPUT.

           SET F-INPUT-STATUS-OK TO TRUE.
           PERFORM UNTIL F-INPUT-STATUS-EOF
               READ F-INPUT 
               AT END SET F-INPUT-STATUS-EOF TO TRUE
               NOT AT END 
                   IF REC-F-INPUT-2 EQUAL "01"
                   PERFORM START-HANDLE-STUDENT THRU END-HANDLE-STUDENT
                   END-IF

                   IF REC-F-INPUT-2 EQUAL "02"
                   PERFORM START-HANDLE-COURSE THRU END-HANDLE-COURSE
                   PERFORM START-HANDLE-GRADE THRU END-HANDLE-GRADE
                   END-IF
               END-READ
           END-PERFORM.

           CLOSE F-INPUT.
       END-R-IP.

      ******************************************************************
       START-HANDLE-STUDENT.
           MOVE R-S-FIRSTNAME TO WS-S-FIRSTNAME(WS-S-CNT).
           MOVE R-S-LASTNAME  TO WS-S-LASTNAME(WS-S-CNT).
           MOVE R-S-AGE       TO WS-S-AGE(WS-S-CNT).

           ADD 1 TO WS-S-CNT.
       END-HANDLE-STUDENT.

      ******************************************************************
       START-HANDLE-COURSE.
           INITIALIZE WS-IS-EXIST.
           SET C-IDX TO 1.

           SEARCH WS-COURSE VARYING C-IDX
               AT END
                   SET WS-IS-EXIST-N TO TRUE
               WHEN WS-C-LABEL(C-IDX) EQUAL R-C-LABEL
                   GO TO END-HANDLE-COURSE
           END-SEARCH.
           
           IF WS-IS-EXIST-N
               MOVE R-C-LABEL TO WS-C-LABEL(WS-C-CNT)
               MOVE R-C-COEF  TO WS-C-COEF(WS-C-CNT)

               ADD 1 TO WS-C-CNT
           END-IF.
       END-HANDLE-COURSE.

      ******************************************************************
       START-HANDLE-GRADE.
           STRING FUNCTION TRIM(WS-S-FIRSTNAME(WS-S-CNT - 1))
           SPACE FUNCTION TRIM(WS-S-LASTNAME(WS-S-CNT - 1))
           DELIMITED BY SIZE
           INTO WS-G-S-FULLNAME(WS-G-CNT).

           MOVE R-C-LABEL TO WS-G-C-LABEL(WS-G-CNT).
           MOVE R-C-GRADE TO WS-G-GRADE(WS-G-CNT).
           MOVE R-C-COEF  TO WS-G-COEF(WS-G-CNT).

           ADD 1 TO WS-G-CNT.
       END-HANDLE-GRADE.

      ******************************************************************
       START-W-OP.
           OPEN OUTPUT F-OUTPUT.

           PERFORM START-HEADER THRU END-HEADER.
           PERFORM START-TABLE-HEADER THRU END-TABLE-HEADER.
           PERFORM START-TABLE-DETAILS THRU END-TABLE-DETAILS.
           PERFORM START-TABLE-FOOTER THRU END-TABLE-FOOTER.
           PERFORM START-FOOTER THRU END-FOOTER.

      *    PERFORM START-NB-S THRU END-NB-S.
      *    PERFORM START-CLASS-MOY-G THRU END-CLASS-MOY-G.

           CLOSE F-OUTPUT.
       END-W-OP.

      ******************************************************************
       START-HEADER.
           INITIALIZE WS-STRING1.
           MOVE ALL "-" TO WS-DASH.

           WRITE REC-F-OUTPUT FROM WS-DASH.

           STRING "|" WS-BLANK "BULLETIN D'ELEVES"
           DELIMITED BY SIZE
           INTO WS-STRING1.

           MOVE "|" TO WS-STRING1(235:1).

           WRITE REC-F-OUTPUT FROM WS-STRING1.
           WRITE REC-F-OUTPUT FROM WS-DASH.
           WRITE REC-F-OUTPUT FROM WS-EMPTY.
       END-HEADER.

      ******************************************************************
       START-TABLE-HEADER.
           INITIALIZE WS-STRING1.
           INITIALIZE WS-STRING2.
           MOVE ALL "-" TO WS-DASH.
           
           WRITE REC-F-OUTPUT FROM WS-DASH.

           STRING "| ELEVE"
           DELIMITED BY SIZE
           INTO WS-STRING1.

           STRING "| MOYENNE GENERALE"
           DELIMITED BY SIZE
           INTO WS-STRING1(41:20).

           STRING "|"
           DELIMITED BY SIZE
           INTO WS-STRING2.

           STRING "|"
           DELIMITED BY SIZE
           INTO WS-STRING2(41:20).

           SET WS-I TO 1.
           SET WS-STRING-POS TO 62.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >= WS-C-CNT
              STRING "|" SPACE WS-C-LABEL(WS-I)
              DELIMITED BY SIZE
              INTO WS-STRING1(WS-STRING-POS:29)
              
              INITIALIZE WS-PNT-COEF
              MOVE WS-C-COEF(WS-I) TO WS-PNT-COEF
              
              STRING "|" SPACE "Coef" SPACE WS-PNT-COEF
              DELIMITED BY SIZE
              INTO WS-STRING2(WS-STRING-POS:29)

              ADD WS-C-COEF(WS-I) TO WS-SUM-COEF
              ADD 29 TO WS-STRING-POS
           END-PERFORM.

           MOVE "|" TO WS-STRING1(235:1).
           MOVE "|" TO WS-STRING2(235:1).
           
           WRITE REC-F-OUTPUT FROM WS-STRING1.
           WRITE REC-F-OUTPUT FROM WS-STRING2.
           WRITE REC-F-OUTPUT FROM WS-DASH.
       END-TABLE-HEADER.

      ******************************************************************
       START-TABLE-DETAILS.
           SET WS-I TO 1.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >= WS-S-CNT
              INITIALIZE WS-FULLNAME-TEMP
              INITIALIZE WS-STRING1

              STRING FUNCTION TRIM(WS-S-FIRSTNAME(WS-I)) 
              SPACE FUNCTION TRIM(WS-S-LASTNAME(WS-I))
              DELIMITED BY SIZE
              INTO WS-FULLNAME-TEMP 

              STRING "|" SPACE WS-FULLNAME-TEMP
              DELIMITED BY SIZE
              INTO WS-STRING1(1:40)
              
              PERFORM START-TABLE-DETAILS-C THRU END-TABLE-DETAILS-C
              
              COMPUTE WS-S-AV-GRADE(WS-I) = 
              WS-S-SUM-GRADE-COEF(WS-I) / WS-SUM-COEF
              
              INITIALIZE WS-PNT-GRADE
              MOVE WS-S-AV-GRADE(WS-I) TO WS-PNT-GRADE
              ADD WS-S-AV-GRADE(WS-I)  TO WS-SUM-GRADE

              STRING "|" SPACE WS-PNT-GRADE
              DELIMITED BY SIZE
              INTO WS-STRING1(41:21)

              MOVE "|" TO WS-STRING1(235:1)
              WRITE REC-F-OUTPUT FROM WS-STRING1
              WRITE REC-F-OUTPUT FROM WS-DASH
           END-PERFORM.
       END-TABLE-DETAILS.

      ******************************************************************
       START-TABLE-DETAILS-C.
           SET WS-STRING-POS TO 62
           SET WS-J TO 1
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J >= WS-G-CNT
              IF WS-G-S-FULLNAME(WS-J) EQUAL WS-FULLNAME-TEMP
              INITIALIZE WS-PNT-GRADE
              MOVE WS-G-GRADE(WS-J) TO WS-PNT-GRADE

              STRING "|" SPACE WS-PNT-GRADE
              DELIMITED BY SIZE
              INTO WS-STRING1(WS-STRING-POS:20)

      *       Ajoute les notes de l'eleve en fonction du coefficient
      *       de la matière
              MULTIPLY WS-G-GRADE(WS-J) BY WS-G-COEF(WS-J) 
              GIVING WS-NUM-TEMP
              ADD WS-NUM-TEMP TO WS-S-SUM-GRADE-COEF(WS-I)

              ADD 29 TO WS-STRING-POS
              END-IF
           END-PERFORM.
       END-TABLE-DETAILS-C.

      ******************************************************************
       START-TABLE-FOOTER.
           INITIALIZE WS-STRING1.
           DISPLAY WS-SUM-GRADE
           
           COMPUTE WS-NUM-TEMP = WS-SUM-GRADE / (WS-S-CNT - 1)

           MOVE WS-NUM-TEMP TO WS-PNT-GRADE.

           STRING "| CLASSE"
           DELIMITED BY SIZE
           INTO WS-STRING1(1:40).

           STRING "|" SPACE WS-PNT-GRADE
           DELIMITED BY SIZE
           INTO WS-STRING1(41:20).

           MOVE "|" TO WS-STRING1(235:1)

           WRITE REC-F-OUTPUT FROM WS-STRING1.
           WRITE REC-F-OUTPUT FROM WS-DASH.
       END-TABLE-FOOTER.

      ******************************************************************
       START-FOOTER.
           MOVE ALL "-" TO WS-DASH.
           
           WRITE REC-F-OUTPUT FROM WS-EMPTY.
           WRITE REC-F-OUTPUT FROM WS-DASH.

           INITIALIZE WS-STRING1.
           INITIALIZE WS-NUM-TEMP.
           INITIALIZE WS-PNT-NBR.
           STRING "| Nombre d'eleves" SPACE
           DELIMITED BY SIZE INTO WS-STRING1.
           MOVE WS-S-CNT TO WS-NUM-TEMP.
           SUBTRACT 1 FROM WS-NUM-TEMP.
           MOVE WS-NUM-TEMP TO WS-PNT-NBR.  
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO WS-STRING1(25:5).
           MOVE "|" TO WS-STRING1(235:1).
           WRITE REC-F-OUTPUT FROM WS-STRING1.
           
           INITIALIZE WS-STRING1.
           INITIALIZE WS-NUM-TEMP.
           INITIALIZE WS-PNT-NBR.
           STRING "| Nombre de matieres" SPACE
           DELIMITED BY SIZE INTO WS-STRING1.
           MOVE WS-C-CNT TO WS-NUM-TEMP.
           SUBTRACT 1 FROM WS-NUM-TEMP.
           MOVE WS-NUM-TEMP TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO WS-STRING1(25:5).
           MOVE "|" TO WS-STRING1(235:1).
           WRITE REC-F-OUTPUT FROM WS-STRING1.

           INITIALIZE WS-STRING1.
           INITIALIZE WS-NUM-TEMP.
           INITIALIZE WS-PNT-NBR.
           STRING "| Nombre total de notes" SPACE
           DELIMITED BY SIZE INTO WS-STRING1.
           MOVE WS-G-CNT TO WS-NUM-TEMP.
           SUBTRACT 1 FROM WS-NUM-TEMP.
           MOVE WS-NUM-TEMP TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO WS-STRING1(25:5).
           MOVE "|" TO WS-STRING1(235:1).
           WRITE REC-F-OUTPUT FROM WS-STRING1.

           WRITE REC-F-OUTPUT FROM WS-DASH.
       END-FOOTER.
       