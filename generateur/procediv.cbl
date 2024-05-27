      ******************************************************************
      *    Sous programme de "genprog.cbl" qui permet d'écrire         *
      *    la PROCEDURE DIVISION du fichier "myprog.cbl".             *
      ****************************************************************** 

       IDENTIFICATION DIVISION.
       PROGRAM-ID. procediv.
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
           03 PNT-BLANK-14  PIC X(14) VALUE ALL SPACES.
           03 PNT-BLANK-17  PIC X(17) VALUE ALL SPACES.
           03 PNT-BLANK-20  PIC X(20) VALUE ALL SPACES.
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
           PERFORM 1000-START-PROCEDURE 
              THRU END-1000-PROCEDURE.
           CLOSE F-OUTPUT.
       END-0000-MAIN.
           GOBACK.

      ******************************************************************
      ******************************************************************
       1000-START-PROCEDURE.
           STRING PNT-BLANK-6 PNT-AST 
               DELIMITED BY SIZE
               INTO PNT-BLANK-AST
           END-STRING.

           WRITE R-OUTPUT FROM PNT-BLANK-6.
           WRITE R-OUTPUT FROM PNT-BLANK-AST.
           WRITE R-OUTPUT FROM PNT-BLANK-6.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 "PROCEDURE DIVISION."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 "0000-START-MAIN."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           IF LK-OPTION-13 EQUAL 13
               STRING 
                   PNT-BLANK-11 'DISPLAY "HELLO COBOL".'
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT
           ELSE
               STRING 
                   PNT-BLANK-11 "Main paragraphe..."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT
           END-IF.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 "END-0000-MAIN."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-11 "STOP RUN."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           IF LK-OPTION-1 EQUAL 1
               INITIALIZE R-OUTPUT
               WRITE R-OUTPUT FROM PNT-BLANK-6
               WRITE R-OUTPUT FROM PNT-BLANK-AST

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-7 "START-READ-INPUT."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-11 "OPEN INPUT F-INPUT."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-11 'IF FS-INPUT EQUAL "00"'
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-14 "SET FS-INPUT-OK TO TRUE"
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               WRITE R-OUTPUT FROM PNT-BLANK-6

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-14 "PERFORM UNTIL FS-INPUT-EOF"
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-17 "READ F-INPUT"
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-17 "AT END"
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-20 "SET FS-INPUT-EOF TO TRUE"
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-17 "NOT AT END"
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-6 "*" PNT-BLANK-11 SPACE
                   SPACE "Traitement..."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-17 "END-READ"
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-14 "END-PERFORM"
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-11 "ELSE"
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-14 
                   'DISPLAY "ERROR READ FILE :" SPACE FS-INPUT'
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-11 "END-IF."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-11 "CLOSE F-INPUT."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-7 "END-READ-INPUT."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-11 "EXIT."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT
           END-IF.

           IF LK-OPTION-14 EQUAL 14
               INITIALIZE R-OUTPUT
               WRITE R-OUTPUT FROM PNT-BLANK-6
               WRITE R-OUTPUT FROM PNT-BLANK-AST

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-7 "1000-START-NAME."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-6 "*    Paragraphe..." 
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-7 "END-1000-NAME."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING 
                   PNT-BLANK-11 "EXIT."
                   DELIMITED BY SIZE 
                   INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT
           END-IF.
       END-1000-PROCEDURE.
           EXIT.
