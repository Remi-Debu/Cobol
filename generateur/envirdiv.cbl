      ******************************************************************
      *    Sous programme de "genprog.cbl" qui permet d'écrire         *
      *    l'IDENTIFICATION DIVISION du fichier "myprog.cbl".          *
      ****************************************************************** 

       IDENTIFICATION DIVISION.
       PROGRAM-ID. envirdiv.
       AUTHOR          Rémi.

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
           PERFORM 1000-START-ENVIRONMENT THRU END-1000-ENVIRONMENT.
           CLOSE F-OUTPUT.
       END-0000-MAIN.
           GOBACK.

      ******************************************************************
      *    Ecris :                                                     *
      *       ENVIRONMENT DIVISION.                                    *
      *       INPUT-OUTPUT SECTION.                                    *
      *       FILE-CONTROL.                                            *
      *    Puis appel le paragraphe "FILE ORGA"                        *
      ******************************************************************
       1000-START-ENVIRONMENT.
           STRING PNT-BLANK-6 PNT-AST 
               DELIMITED BY SIZE
               INTO PNT-BLANK-AST
           END-STRING.

           IF LK-OPTION-1 EQUAL 1 OR LK-OPTION-2 EQUAL 2 
           OR LK-OPTION-3 EQUAL 4 OR LK-OPTION-4 EQUAL 4
      
               WRITE R-OUTPUT FROM PNT-BLANK-AST
               WRITE R-OUTPUT FROM PNT-BLANK-6
      
               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-7 "ENVIRONMENT DIVISION."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT
      
               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-7 "INPUT-OUTPUT SECTION."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT
      
               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-7 "FILE-CONTROL."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT
               
               IF LK-OPTION-1 EQUAL 1
                   MOVE "INPUT" TO WS-FILE-NAME
                   PERFORM 1100-START-FILE-ORGA THRU END-1100-FILE-ORGA
               END-IF
      
               IF LK-OPTION-2 EQUAL 2
                   MOVE "OUTPUT" TO WS-FILE-NAME
                   PERFORM 1100-START-FILE-ORGA THRU END-1100-FILE-ORGA
               END-IF
      
               IF LK-OPTION-3 EQUAL 3
                   MOVE "INPUT-SORT"  TO WS-FILE-NAME
                   PERFORM 1100-START-FILE-ORGA THRU END-1100-FILE-ORGA
                   MOVE "OUTPUT-SORT" TO WS-FILE-NAME
                   PERFORM 1100-START-FILE-ORGA THRU END-1100-FILE-ORGA
                   MOVE "WORK-SORT"   TO WS-FILE-NAME
                   PERFORM 1100-START-FILE-ORGA THRU END-1100-FILE-ORGA
               END-IF
      
               IF LK-OPTION-4 EQUAL 4
                   PERFORM 1100-START-FILE-ORGA THRU END-1100-FILE-ORGA
               END-IF
      
               INITIALIZE R-OUTPUT
               WRITE R-OUTPUT FROM PNT-BLANK-6
           END-IF.
       END-1000-ENVIRONMENT.
           EXIT.

      ******************************************************************
      *    Ecris :                                                     *
      *       SELECT F-`VARIABLE` ASSIGN TO "yourfilename.txt"         *
      *       ORGANIZATION IS LINE SEQUENTIAL                          *
      *       ACCESS MODE IS SEQUENTIAL                                *
      *       FILE STATUS IS FS-`VARIABLE`.                            *
      ******************************************************************
       1100-START-FILE-ORGA.
           INITIALIZE R-OUTPUT.
           WRITE R-OUTPUT FROM PNT-BLANK-6.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-11 "SELECT F-" FUNCTION TRIM(WS-FILE-NAME) 
               SPACE 'ASSIGN TO "yourfilename.txt"'
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-11 "ORGANIZATION IS LINE SEQUENTIAL" 
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-11 "ACCESS MODE IS SEQUENTIAL"
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-11 "FILE STATUS IS FS-" 
               FUNCTION TRIM(WS-FILE-NAME) "."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.
       END-1100-FILE-ORGA.
           EXIT.
