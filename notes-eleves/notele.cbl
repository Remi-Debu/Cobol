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
               05 WS-S-ID PIC 9(03).
               05 WS-S-FIRSTNAME PIC X(20).
               05 WS-S-LASTNAME  PIC X(20).
               05 WS-S-AGE       PIC 9(02).
       
       01  WS-TABLE-COURSE.
           03 WS-C-CNT  PIC 9(03) VALUE 1.
           03 WS-COURSE OCCURS 1 TO 200 TIMES
                        DEPENDING ON WS-C-CNT
                        INDEXED BY C-IDX.
               05 WS-C-ID    PIC 9(03).
               05 WS-C-LABEL PIC X(21).
               05 WS-C-COEF  PIC 9V9.

       01  WS-TABLE-GRADE.
           03 WS-G-CNT PIC 9(03) VALUE 1.
           03 WS-GRADE OCCURS 1 TO 200 TIMES
                       DEPENDING ON WS-G-CNT
                       INDEXED BY G-IDX.
               05 WS-G-ID         PIC 9(03).
               05 WS-G-S-FULLNAME PIC X(40).
               05 WS-G-C-LABEL    PIC X(25).
               05 WS-G-GRADE      PIC 99V99.

       01  WS-IS-EXIST PIC X.
           88 WS-IS-EXIST-Y VALUE "Y".
           88 WS-IS-EXIST-N VALUE "N".

       01  WS-PNT.
           03 WS-PNT-NBR      PIC ZZZZ9.
           03 WS-PNT-GRADE    PIC Z9,99.
           03 WS-PNT-COEF     PIC 9,9.

       01  WS-I               PIC 9(03) VALUE 1.
       01  WS-STRING-POS      PIC 9(03) VALUE 1.
       01  WS-NUM-TEMP        PIC 9(03).

       01  WS-DISPLAY.
           03 WS-DASH   PIC X(180).
           03 WS-BLANK  PIC X(80).
           03 WS-EMPTY  PIC X.
           03 WS-STRING PIC X(180).
      *01  WS-J                  PIC 9(03) VALUE 1.
      *01  WS-STRING-LENGTH-INCR PIC 9(03).
      *01  WS-NB-S               PIC ZZ9.
      *01  WS-GRADE-COMMA        PIC Z9,99.
      *01  WS-AV-COMMA           PIC Z9,99.
      *
      *01  WS-COEF-NUM           PIC 9V9.
      *01  WS-GRADE-NUM          PIC 9(02)V9(02).
      *
      *01  WS-REC-STRING   PIC X(250).
      *01  WS-NOTE-TEMP    PIC 99V99.
      *01  WS-NB-ELEV      PIC Z9.
      *01  WS-CLASS-MOY-G  PIC Z9V99.

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
           MOVE WS-S-CNT      TO WS-S-ID(WS-S-CNT).
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
               MOVE WS-C-CNT  TO WS-C-ID(WS-C-CNT)
               MOVE R-C-LABEL TO WS-C-LABEL(WS-C-CNT)
               MOVE R-C-COEF  TO WS-C-COEF(WS-C-CNT)

               ADD 1 TO WS-C-CNT
           END-IF.
       END-HANDLE-COURSE.

      ******************************************************************
       START-HANDLE-GRADE.
           STRING WS-S-FIRSTNAME(WS-S-CNT - 1) 
           SPACE WS-S-LASTNAME(WS-S-CNT - 1)
           DELIMITED BY SIZE
           INTO WS-G-S-FULLNAME(WS-G-CNT).

           MOVE WS-G-CNT  TO WS-G-ID(WS-G-CNT).
           MOVE R-C-LABEL TO WS-G-C-LABEL(WS-G-CNT).
           MOVE R-C-GRADE TO WS-G-GRADE(WS-G-CNT).

           ADD 1 TO WS-G-CNT.
       END-HANDLE-GRADE.

      ******************************************************************
       START-W-OP.
           OPEN OUTPUT F-OUTPUT.

           PERFORM START-HEADER THRU END-HEADER.
           PERFORM START-TABLE-HEADER THRU END-TABLE-HEADER.
           
           SET WS-I TO 1.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >= WS-G-CNT
               INITIALIZE REC-F-OUTPUT
               INITIALIZE WS-PNT-GRADE
               MOVE WS-G-GRADE(WS-I) TO WS-PNT-GRADE

               STRING WS-G-ID(WS-I) SPACE WS-G-S-FULLNAME(WS-I) SPACE
               WS-G-C-LABEL(WS-I) SPACE WS-PNT-GRADE
               DELIMITED BY SIZE INTO REC-F-OUTPUT

      *        STRING WS-G-S-FULLNAME(WS-I) 
      *        DELIMITED BY SIZE,
      *        WS-GRADE(WS-J) 
      *        DELIMITED BY SIZE
      *        INTO WS-REC-STRING
      *        
      *        ADD 6 TO WS-STRING-LENGTH-INCR

               WRITE REC-F-OUTPUT
           END-PERFORM.
           
           PERFORM START-FOOTER THRU END-FOOTER.

      *    PERFORM START-NB-S THRU END-NB-S.
      *    PERFORM START-CLASS-MOY-G THRU END-CLASS-MOY-G.

           CLOSE F-OUTPUT.
       END-W-OP.

      ******************************************************************
       START-HEADER.
           INITIALIZE WS-STRING.
           MOVE ALL "-" TO WS-DASH.

           WRITE REC-F-OUTPUT FROM WS-DASH.

           STRING "|" WS-BLANK "BULLETIN D'ELEVES"
           DELIMITED BY SIZE
           INTO WS-STRING.

           MOVE "|" TO WS-STRING(180:1).

           WRITE REC-F-OUTPUT FROM WS-STRING.
           WRITE REC-F-OUTPUT FROM WS-DASH.
           WRITE REC-F-OUTPUT FROM WS-EMPTY.
       END-HEADER.

      ******************************************************************
       START-TABLE-HEADER.
           INITIALIZE WS-STRING.
           MOVE ALL "-" TO WS-DASH.
           
           WRITE REC-F-OUTPUT FROM WS-DASH.

           SET WS-I TO 1.
           SET WS-STRING-POS TO 3.
           PERFORM VARYING WS-I FROM 1 BY 1
            UNTIL WS-I > WS-C-CNT
            
               STRING WS-C-LABEL(WS-I)
               DELIMITED BY SIZE
               INTO WS-STRING(WS-STRING-POS:29)

               ADD 29 TO WS-STRING-POS
           END-PERFORM.

           MOVE "|" TO WS-STRING(1:1).
           MOVE "|" TO WS-STRING(180:1).
           
           WRITE REC-F-OUTPUT FROM WS-STRING.
           WRITE REC-F-OUTPUT FROM WS-DASH.
       END-TABLE-HEADER.

       START-FOOTER.
           MOVE ALL "-" TO WS-DASH.
           
           WRITE REC-F-OUTPUT FROM WS-EMPTY.
           WRITE REC-F-OUTPUT FROM WS-DASH.

           INITIALIZE WS-STRING.
           INITIALIZE WS-NUM-TEMP.
           INITIALIZE WS-PNT-NBR.
           STRING "| Nombre d'eleves" SPACE
           DELIMITED BY SIZE INTO WS-STRING.
           MOVE WS-S-CNT TO WS-NUM-TEMP.
           SUBTRACT 1 FROM WS-NUM-TEMP.
           MOVE WS-NUM-TEMP TO WS-PNT-NBR.  
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO WS-STRING(25:5).
           MOVE "|" TO WS-STRING(180:1).
           WRITE REC-F-OUTPUT FROM WS-STRING.
           
           INITIALIZE WS-STRING.
           INITIALIZE WS-NUM-TEMP.
           INITIALIZE WS-PNT-NBR.
           STRING "| Nombre de matieres" SPACE
           DELIMITED BY SIZE INTO WS-STRING.
           MOVE WS-C-CNT TO WS-NUM-TEMP.
           SUBTRACT 1 FROM WS-NUM-TEMP.
           MOVE WS-NUM-TEMP TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO WS-STRING(25:5).
           MOVE "|" TO WS-STRING(180:1).
           WRITE REC-F-OUTPUT FROM WS-STRING.

           INITIALIZE WS-STRING.
           INITIALIZE WS-NUM-TEMP.
           INITIALIZE WS-PNT-NBR.
           STRING "| Nombre total de notes" SPACE
           DELIMITED BY SIZE INTO WS-STRING.
           MOVE WS-G-CNT TO WS-NUM-TEMP.
           SUBTRACT 1 FROM WS-NUM-TEMP.
           MOVE WS-NUM-TEMP TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO WS-STRING(25:5).
           MOVE "|" TO WS-STRING(180:1).
           WRITE REC-F-OUTPUT FROM WS-STRING.

           WRITE REC-F-OUTPUT FROM WS-DASH.
       END-FOOTER.

      ******************************************************************
      *START-STUD-AV.
      *    INITIALIZE WS-AV-COMMA.
      *
      *    DISPLAY WS-SUM-GRADE(WS-I).
      *    DISPLAY WS-SUM-COEF(WS-I).

      *    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 6
      *        INITIALIZE WS-NOTE-TEMP
      *        MOVE WS-GRADE(WS-I, WS-J) TO WS-NOTE-TEMP
      *        ADD WS-NOTE-TEMP TO WS-AV-TEMP
      *    END-PERFORM.

      *    SUBTRACT 1 FROM WS-J GIVING WS-J
      *    DIVIDE WS-AV-TEMP BY WS-J GIVING WS-AV-TEMP.
      *    MOVE WS-AV-TEMP TO WS-S-AV(WS-I).
      *END-STUD-AV.

      ******************************************************************
      *START-NB-S.
      *    INITIALIZE WS-REC-STRING.
      *    
      *    MOVE WS-S-CNT TO WS-NB-S.
      *    STRING "Nombre d'eleve :" SPACE FUNCTION TRIM(WS-NB-S)
      *    DELIMITED BY SIZE
      *    INTO WS-REC-STRING.
      *    
      *    WRITE REC-F-OUTPUT FROM SPACE.
      *    WRITE REC-F-OUTPUT FROM WS-REC-STRING.
      *END-NB-S.

      *START-CLASS-MOY-G.
      *    PERFORM VARYING WS-I FROM 1 BY 1 
      *            UNTIL WS-I > WS-TABLE-LENGTH
      *    ADD WS-S-AV(WS-I) TO WS-AV-TEMP
      *    END-PERFORM.

      *    DIVIDE WS-AV-TEMP BY WS-TABLE-LENGTH 
      *    GIVING WS-CLASS-MOY-G.

      *    WRITE REC-F-OUTPUT FROM WS-CLASS-MOY-G.
      *END-CLASS-MOY-G.
       