      ******************************************************************
      *    Sous programme de "genprog.cbl" qui permet d'écrire         *
      *    l'ENVIRONMENT DIVISION du fichier "myprog.cbl".             *
      ****************************************************************** 

       IDENTIFICATION DIVISION.
       PROGRAM-ID. identdiv.
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
           03 PNT-AST       PIC X(66) VALUE ALL "*".
           03 PNT-BLANK-AST PIC X(72).

      ****************************************************************** 

       PROCEDURE DIVISION.
       0000-START-MAIN.
           OPEN OUTPUT F-OUTPUT.
           PERFORM 1000-START-IDENTIFICATION 
              THRU END-1000-IDENTIFICATION.
           CLOSE F-OUTPUT.
       END-0000-MAIN.
           GOBACK.

      ******************************************************************
      *    Ecris :                                                     *
      *       IDENTIFICATION DIVISION.                                 *
      *       PROGRAM-ID. myprog.                                      *
      *       AUTHOR.   YourName.                                      *
      ******************************************************************
       1000-START-IDENTIFICATION.
           STRING 
               PNT-BLANK-6 PNT-AST 
               DELIMITED BY SIZE
               INTO PNT-BLANK-AST
           END-STRING.

           STRING PNT-BLANK-6 PNT-AST
           DELIMITED BY SIZE INTO PNT-BLANK-AST.

           WRITE R-OUTPUT FROM PNT-BLANK-AST.
           WRITE R-OUTPUT FROM PNT-BLANK-AST.
           WRITE R-OUTPUT FROM PNT-BLANK-6.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 "IDENTIFICATION DIVISION."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 "PROGRAM-ID. myprog."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.
       
           INITIALIZE R-OUTPUT.
           STRING 
               PNT-BLANK-7 "AUTHOR.   YourName."
               DELIMITED BY SIZE 
               INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           WRITE R-OUTPUT FROM PNT-BLANK-6.
       END-1000-IDENTIFICATION.
           EXIT.
