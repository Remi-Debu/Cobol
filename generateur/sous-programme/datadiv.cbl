      ******************************************************************
      *    Sous programme de "genprog.cbl" qui permet d'écrire         *
      *    la DATA DIVISION du fichier "myprog.cbl".                   *
      ****************************************************************** 

       IDENTIFICATION DIVISION.
       PROGRAM-ID. datadiv.
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

       01  PRINT.
           03 PNT-BLANK-6   PIC X(06) VALUE ALL SPACES.
           03 PNT-BLANK-7   PIC X(07) VALUE ALL SPACES.
           03 PNT-BLANK-11  PIC X(11) VALUE ALL SPACES.
           03 PNT-AST       PIC X(66) VALUE ALL "*".
           03 PNT-BLANK-AST PIC X(72).

       01  WS-FILE-NAME PIC X(20).

       LINKAGE SECTION.
       01  LK-OPTIONS-NUM.
           03 LK-OPTION-1  PIC 9(02).
           03 LK-OPTION-2  PIC 9(02).
           03 LK-OPTION-3  PIC 9(02).
           03 LK-OPTION-4  PIC 9(02).
           03 LK-OPTION-5  PIC 9(02).
           03 LK-OPTION-6  PIC 9(02).
           03 LK-OPTION-7  PIC 9(02).
           03 LK-OPTION-8  PIC 9(02).
           03 LK-OPTION-9  PIC 9(02).
           03 LK-OPTION-10 PIC 9(02).
           03 LK-OPTION-11 PIC 9(02).
           03 LK-OPTION-12 PIC 9(02).
           03 LK-OPTION-13 PIC 9(02).
           03 LK-OPTION-14 PIC 9(02).

      ****************************************************************** 

       PROCEDURE DIVISION USING LK-OPTIONS-NUM.
       0000-START-MAIN.
           OPEN EXTEND F-OUTPUT.
           PERFORM 1000-START-DATA THRU END-1000-DATA.
           CLOSE F-OUTPUT.
       END-0000-MAIN.
           GOBACK.

      ******************************************************************
      *    Ecris :                                                     *
      *        DATA DIVISION.                                          * 
      *        FILE SECTION.                                           *
      *    Puis appel le paragraphe "FILE SECTION"                     *
      *                                                                *
      *        WORKING-STORAGE SECTION.                                *
      *    Puis appel les paragraphes "FS-INPUT" et "FS-OUTPUT"        *
      ******************************************************************
       1000-START-DATA.
           STRING 
               PNT-BLANK-6 PNT-AST 
               DELIMITED BY SIZE
               INTO PNT-BLANK-AST
           END-STRING.

           IF LK-OPTION-1 EQUAL 1 OR LK-OPTION-2 EQUAL 2 
           OR LK-OPTION-3 EQUAL 4 OR LK-OPTION-4 EQUAL 4
           OR LK-OPTION-3 EQUAL 5 OR LK-OPTION-4 EQUAL 6
           OR LK-OPTION-3 EQUAL 7 OR LK-OPTION-4 EQUAL 8

           WRITE R-OUTPUT FROM PNT-BLANK-AST
           WRITE R-OUTPUT FROM PNT-BLANK-6

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-7 "DATA DIVISION."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT
           END-IF.

           IF LK-OPTION-1 EQUAL 1 OR LK-OPTION-2 EQUAL 2 
           OR LK-OPTION-3 EQUAL 4 OR LK-OPTION-4 EQUAL 4
               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-7 "FILE SECTION."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT
           END-IF

           IF LK-OPTION-1 EQUAL 1
               MOVE "INPUT" TO WS-FILE-NAME
               PERFORM 1100-START-FILE-SECTION
                  THRU END-1100-FILE-SECTION
           END-IF.

           IF LK-OPTION-2 EQUAL 2
               MOVE "OUTPUT" TO WS-FILE-NAME
               PERFORM 1100-START-FILE-SECTION
                  THRU END-1100-FILE-SECTION
           END-IF.

           IF LK-OPTION-3 EQUAL 3
               MOVE "SORT" TO WS-FILE-NAME
               PERFORM 1100-START-FILE-SECTION
                  THRU END-1100-FILE-SECTION
           END-IF.

           IF LK-OPTION-4 EQUAL 4
               MOVE "MERGE" TO WS-FILE-NAME
               PERFORM 1100-START-FILE-SECTION
                  THRU END-1100-FILE-SECTION
           END-IF.

           IF LK-OPTION-1 EQUAL 1 OR LK-OPTION-2 EQUAL 2 
           OR LK-OPTION-3 EQUAL 4 OR LK-OPTION-4 EQUAL 4

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-7 "WORKING-STORAGE SECTION."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT
           END-IF.

           IF LK-OPTION-1 EQUAL 1
               MOVE "INPUT" TO WS-FILE-NAME
               PERFORM 1200-START-FS-INPUT
                  THRU END-1200-FS-INPUT
           END-IF.

           IF LK-OPTION-2 EQUAL 2
               MOVE "OUTPUT" TO WS-FILE-NAME
               PERFORM 1300-START-FS-OUTPUT
                  THRU END-1300-FS-OUTPUT
           END-IF.

           IF LK-OPTION-3 EQUAL 3
               MOVE "SORT" TO WS-FILE-NAME
               PERFORM 1300-START-FS-OUTPUT
                  THRU END-1300-FS-OUTPUT
           END-IF.

           IF LK-OPTION-4 EQUAL 4
               MOVE "MERGE" TO WS-FILE-NAME
               PERFORM 1200-START-FS-INPUT
                  THRU END-1200-FS-INPUT
           END-IF.

           IF LK-OPTION-5 EQUAL 5
               PERFORM 1400-START-SQL-SECTION 
                  THRU END-1400-SQL-SECTION
           END-IF.
       END-1000-DATA.
           EXIT.

      ******************************************************************
      *    Ecris :                                                     *
      *        FD  F-`VARIABLE`                                        * 
      *            RECORD CONTAINS 1000 CHARACTERS                     *
      *            RECORDING MODE IS F.                                *
      *        01  R-`VARIABLE` PIC X(1000).                           * 
      ******************************************************************
       1100-START-FILE-SECTION.
           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 "FD  F-" FUNCTION TRIM(WS-FILE-NAME)
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-11 "RECORD CONTAINS 1000 CHARACTERS"
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-11 "RECORDING MODE IS F."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 "01  R-" FUNCTION TRIM(WS-FILE-NAME) 
               SPACE "PIC X(1000)."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           WRITE R-OUTPUT FROM PNT-BLANK-6.
       END-1100-FILE-SECTION.
           EXIT.

      ******************************************************************
      *    Ecris :                                                     *
      *        01  FS-`VARIABLE` PIC X(02).                            *
      *            88 FS-`VARIABLE`-OK  VALUE "00".                    *
      *            88 FS-`VARIABLE`-EOF VALUE "10".                    *
      ******************************************************************
       1200-START-FS-INPUT.
           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 "01  FS-" FUNCTION TRIM(WS-FILE-NAME) 
               SPACE "PIC X(02)."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-11 "88 FS-" FUNCTION TRIM(WS-FILE-NAME) 
               '-OK  VALUE "00".'
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-11 "88 FS-" FUNCTION TRIM(WS-FILE-NAME) 
               '-EOF VALUE "10".'
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           WRITE R-OUTPUT FROM PNT-BLANK-6.
       END-1200-FS-INPUT.  
           EXIT.

      ******************************************************************
      *    Ecris :                                                     *
      *        01  FS-`VARIABLE` PIC X(02).                            *
      *            88 FS-`VARIABLE`-OK VALUE "00".                     *
      ******************************************************************
       1300-START-FS-OUTPUT.
           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 "01  FS-" FUNCTION TRIM(WS-FILE-NAME) 
               SPACE "PIC X(02)."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-11 "88 FS-" FUNCTION TRIM(WS-FILE-NAME) 
               '-OK VALUE "00".'
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           WRITE R-OUTPUT FROM PNT-BLANK-6.
       END-1300-FS-OUTPUT.
           EXIT.
      
      ******************************************************************
      *    Ecris :                                                     *
      *        EXEC SQL BEGIN DECLARE SECTION END-EXEC.                *
      *        01  DBNAME   PIC  X(30) VALUE 'dbname'.                 *
      *        01  USERNAME PIC  X(30) VALUE 'username'.               *
      *        01  PASSWD   PIC  X(10) VALUE 'password'.               *
      *        EXEC SQL END DECLARE SECTION END-EXEC.                  *
      *                                                                *
      *        EXEC SQL INCLUDE SQLCA END-EXEC.                        *
      ******************************************************************
       1400-START-SQL-SECTION.
           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 
               "EXEC SQL BEGIN DECLARE SECTION END-EXEC."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 
               '01  DBNAME   PIC  X(30) VALUE "dbname".'
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 
               '01  USERNAME PIC  X(30) VALUE "username".'
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 
               '01  PASSWD   PIC  X(10) VALUE "password".'
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 
               "EXEC SQL END DECLARE SECTION END-EXEC."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           WRITE R-OUTPUT FROM PNT-BLANK-6.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 
               "EXEC SQL INCLUDE SQLCA END-EXEC."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           WRITE R-OUTPUT FROM PNT-BLANK-6.
       END-1400-SQL-SECTION.
           EXIT.
