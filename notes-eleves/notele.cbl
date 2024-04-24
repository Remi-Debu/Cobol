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
           03 R-COEF        PIC 9,9.
           03 R-GRADE       PIC 99,99.

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
       
       01  WS-TABLE-STUDENT.
           03  WS-STUDENT OCCURS 1 TO 200 TIMES
                          DEPENDING ON WS-S-CNT.
               05 WS-ID        PIC 9(03).
               05 WS-NAME      PIC X(14).
               05 WS-S-AV      PIC 9(02)V9(02).
       
       01  WS-TABLE-COURSE.
           03 WS-COURSE OCCURS 1 TO 200 TIMES
                        DEPENDING ON WS-C-CNT.
               05 WS-S-ID      PIC 9(03).
               05 WS-LABEL     PIC X(21).
               05 WS-COEF      PIC 9V9.
               05 WS-SUM-COEF  PIC 9(05)V9(02).
               05 WS-GRADE     PIC 9(02)V9(02).
               05 WS-SUM-GRADE PIC 9(05)V9(02).

       01  WS-S-CNT  PIC 9(03).
       01  WS-C-CNT  PIC 9(03).

       01  WS-I                  PIC 9(03) VALUE 1.
       01  WS-J                  PIC 9(03) VALUE 1.
       01  WS-STRING-LENGTH-INCR PIC 9(03).
       01  WS-NB-S               PIC ZZ9.
       01  WS-GRADE-COMMA        PIC Z9,99.
       01  WS-AV-COMMA           PIC Z9,99.

       01  WS-COEF-NUM           PIC 9V9.
       01  WS-GRADE-NUM          PIC 9(02)V9(02).

       01  WS-REC-STRING   PIC X(250).
       01  WS-NOTE-TEMP    PIC 99V99.
       01  WS-NB-ELEV      PIC Z9.
       01  WS-CLASS-MOY-G  PIC Z9V99.

       PROCEDURE DIVISION.
           PERFORM START-R-IP THRU END-R-IP.
           PERFORM START-W-S-OP THRU END-W-S-OP.
           STOP RUN.

       START-R-IP.
           OPEN INPUT F-INPUT.

           SET F-INPUT-STATUS-OK TO TRUE.
           PERFORM UNTIL F-INPUT-STATUS-EOF
               READ F-INPUT 
               AT END SET F-INPUT-STATUS-EOF TO TRUE
               NOT AT END 
                   IF REC-F-INPUT-2 EQUAL 01
                   ADD 1 TO WS-S-CNT 
                    
                   MOVE WS-S-CNT TO WS-ID(WS-S-CNT)

                   STRING FUNCTION TRIM(R-FIRSTNAME) SPACE 
                   FUNCTION TRIM(R-LASTNAME)
                   DELIMITED BY SIZE INTO WS-NAME(WS-S-CNT)
                   END-IF

                   IF REC-F-INPUT-2 EQUAL 02
                   ADD 1 TO WS-C-CNT

                   MOVE WS-ID(WS-S-CNT) TO WS-S-ID(WS-C-CNT)
                   MOVE R-LABEL TO WS-LABEL(WS-C-CNT)
                   MOVE R-COEF TO WS-COEF(WS-C-CNT)
                   MOVE R-GRADE TO WS-GRADE(WS-C-CNT)

                   MOVE R-COEF TO WS-COEF-NUM
                   ADD WS-COEF-NUM TO WS-SUM-COEF(WS-C-CNT)
                   MOVE R-GRADE TO WS-GRADE-NUM
                   ADD WS-GRADE-NUM TO WS-SUM-GRADE(WS-C-CNT)

                   END-IF
               END-READ
           END-PERFORM.

           CLOSE F-INPUT.
       END-R-IP.

       START-W-S-OP.
           OPEN OUTPUT F-OUTPUT.
           
           PERFORM VARYING WS-I FROM 1 BY 1 
                   UNTIL WS-I > WS-S-CNT

           MOVE WS-NAME(WS-I) TO WS-REC-STRING
           
           SET WS-STRING-LENGTH-INCR TO 15
              PERFORM VARYING WS-J
              FROM 1 BY 1
              UNTIL WS-J > WS-C-CNT
              
                 IF WS-S-ID(WS-J) EQUAL WS-I
                 STRING WS-REC-STRING(1:WS-STRING-LENGTH-INCR) 
                 DELIMITED BY SIZE,
                 WS-GRADE(WS-J) 
                 DELIMITED BY SIZE
                 INTO WS-REC-STRING
                 
                 ADD 6 TO WS-STRING-LENGTH-INCR
                 END-IF
              END-PERFORM

              PERFORM START-STUD-AV THRU END-STUD-AV

              WRITE REC-F-OUTPUT FROM WS-REC-STRING
           END-PERFORM.

           PERFORM START-NB-S THRU END-NB-S.
      *    PERFORM START-CLASS-MOY-G THRU END-CLASS-MOY-G.

           CLOSE F-OUTPUT.
       END-W-S-OP.

       START-STUD-AV.
           INITIALIZE WS-AV-COMMA.

           DISPLAY WS-SUM-GRADE(WS-I).
           DISPLAY WS-SUM-COEF(WS-I).

      *    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 6
      *        INITIALIZE WS-NOTE-TEMP
      *        MOVE WS-GRADE(WS-I, WS-J) TO WS-NOTE-TEMP
      *        ADD WS-NOTE-TEMP TO WS-AV-TEMP
      *    END-PERFORM.

      *    SUBTRACT 1 FROM WS-J GIVING WS-J
      *    DIVIDE WS-AV-TEMP BY WS-J GIVING WS-AV-TEMP.
      *    MOVE WS-AV-TEMP TO WS-S-AV(WS-I).
       END-STUD-AV.

       START-NB-S.
           INITIALIZE WS-REC-STRING.
           
           MOVE WS-S-CNT TO WS-NB-S.
           STRING "Nombre d'eleve :" SPACE FUNCTION TRIM(WS-NB-S)
           DELIMITED BY SIZE
           INTO WS-REC-STRING.
           
           WRITE REC-F-OUTPUT FROM SPACE.
           WRITE REC-F-OUTPUT FROM WS-REC-STRING.
       END-NB-S.

      *START-CLASS-MOY-G.
      *    PERFORM VARYING WS-I FROM 1 BY 1 
      *            UNTIL WS-I > WS-TABLE-LENGTH
      *    ADD WS-S-AV(WS-I) TO WS-AV-TEMP
      *    END-PERFORM.

      *    DIVIDE WS-AV-TEMP BY WS-TABLE-LENGTH 
      *    GIVING WS-CLASS-MOY-G.

      *    WRITE REC-F-OUTPUT FROM WS-CLASS-MOY-G.
      *END-CLASS-MOY-G.
       