      ******************************************************************
      *    Le programme génére un programme COBOL qui contient divers  *
      *    éléments en fonction des options que l'utilisateur a choisi *
      *    (interface utilisateur SCREEN SECTION).                     *
      ****************************************************************** 

       IDENTIFICATION DIVISION.
       PROGRAM-ID. genprog.
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
           88 FS-OUTPUT-EOF VALUE "10".

       01  PRINT.
           03 PNT-BLANK-6   PIC X(06) VALUE ALL SPACES.
           03 PNT-BLANK-7   PIC X(07) VALUE ALL SPACES.
           03 PNT-BLANK-11  PIC X(11) VALUE ALL SPACES.
           03 PNT-BLANK-14  PIC X(14) VALUE ALL SPACES.
           03 PNT-BLANK-17  PIC X(17) VALUE ALL SPACES.
           03 PNT-BLANK-20  PIC X(20) VALUE ALL SPACES.
           03 PNT-AST       PIC X(66) VALUE ALL "*".
           03 PNT-BLANK-AST PIC X(72).

       01  OPTIONS-NUM.
           03 OPTION-1  PIC 9(02).
           03 OPTION-2  PIC 9(02).
           03 OPTION-3  PIC 9(02).
           03 OPTION-4  PIC 9(02).
           03 OPTION-5  PIC 9(02).
           03 OPTION-6  PIC 9(02).
           03 OPTION-7  PIC 9(02).
           03 OPTION-8  PIC 9(02).
           03 OPTION-9  PIC 9(02).
           03 OPTION-10 PIC 9(02).
           03 OPTION-11 PIC 9(02).
           03 OPTION-12 PIC 9(02).
           03 OPTION-13 PIC 9(02).
           03 OPTION-14 PIC 9(02).

       SCREEN SECTION.
       01  SCREEN-OPTIONS FOREGROUND-COLOR 2.
           03 LINE 02 COL 02 
              VALUE "PROGRAM GENERATOR" FOREGROUND-COLOR 3.
           03 LINE 04 COL 02 VALUE "PARTIE FICHIER :".
           03 LINE 05 COL 05 VALUE "01. Lire un fichier.".
           03 LINE 05 COL 02 PIC X(02) TO OPTION-1 FOREGROUND-COLOR 7.
           03 LINE 06 COL 05 VALUE "02. Ecrire un fichier.".
           03 LINE 06 COL 02 PIC X(02) TO OPTION-2 FOREGROUND-COLOR 7.
           03 LINE 07 COL 05 VALUE "03. Trier un fichier.".
           03 LINE 07 COL 02 PIC X(02) TO OPTION-3 FOREGROUND-COLOR 7.
           03 LINE 08 COL 05 VALUE "04. Fusionner plusieurs fichiers.".
           03 LINE 08 COL 02 PIC X(02) TO OPTION-4 FOREGROUND-COLOR 7.
           03 LINE 10 COL 02 VALUE "PARTIE SQL :".
           03 LINE 11 COL 05 VALUE "05. Acceder a une BDD.".
           03 LINE 11 COL 02 PIC X(02) TO OPTION-5 FOREGROUND-COLOR 7.
           03 LINE 12 COL 05 VALUE "06. Generer une requete COUNT(*).".
           03 LINE 12 COL 02 PIC X(02) TO OPTION-6 FOREGROUND-COLOR 7.
           03 LINE 13 COL 05 VALUE "07. Generer un curseur.".
           03 LINE 13 COL 02 PIC X(02) TO OPTION-7 FOREGROUND-COLOR 7.
           03 LINE 14 COL 05 VALUE "08. Generer une requete UPDATE.".
           03 LINE 14 COL 02 PIC X(02) TO OPTION-8 FOREGROUND-COLOR 7.
           03 LINE 16 COL 02 VALUE "PARTIE SOUS-ROUTINES :".
           03 LINE 17 COL 05 VALUE "09. Creer une sous-routine.".
           03 LINE 17 COL 02 PIC X(02) TO OPTION-9 FOREGROUND-COLOR 7.
           03 LINE 18 COL 05 VALUE "10. Integrer un appel type.".
           03 LINE 18 COL 02 PIC X(02) TO OPTION-10 FOREGROUND-COLOR 7.
           03 LINE 20 COL 02 VALUE "PARTIE WORKING-STORAGE :".
           03 LINE 21 COL 05 VALUE "11. Preparer un Copybook.".
           03 LINE 21 COL 02 PIC X(02) TO OPTION-11 FOREGROUND-COLOR 7.
           03 LINE 22 COL 05 VALUE "12. Integrer un Copybook.".
           03 LINE 22 COL 02 PIC X(02) TO OPTION-12 FOREGROUND-COLOR 7.
           03 LINE 24 COL 02 VALUE "PARTIE PROCEDURE :".
           03 LINE 25 COL 05 VALUE "13. Ajouter un Hello Cobol.".
           03 LINE 25 COL 02 PIC X(02) TO OPTION-13 FOREGROUND-COLOR 7.
           03 LINE 26 COL 05 VALUE "14. Integrer des paragraphes.".
           03 LINE 26 COL 02 PIC X(02) TO OPTION-14 FOREGROUND-COLOR 7.

       01  SCREEN-ENVIRONMENT FOREGROUND-COLOR 2.
           03 LINE 04 COL 02 VALUE "ENVIRONMENT DIVISION.". 
           03 LINE 05 COL 02 VALUE "INPUT-OUTPUT SECTION.".
           03 LINE 06 COL 02 VALUE "FILE-CONTROL.".
           03 LINE 07 COL 05 VALUE "LOGICAL FILE NAME : F-".
           03 LINE 08 COL 05 VALUE "READ FILE NAME    :".
           03 LINE 09 COL 05 VALUE "ORGANIZATION      :".
           03 LINE 10 COL 05 VALUE "ACCESS MODE       :".
           03 LINE 07 COL 27 PIC X(10) 
                             TO LOGICAL-FILE-NAME FOREGROUND-COLOR 7.
           03 LINE 08 COL 25 PIC X(20) 
                             TO READ-FILE-NAME FOREGROUND-COLOR 7.
           03 LINE 09 COL 25 PIC X(15) 
                             TO ENV-ORGANIZATION FOREGROUND-COLOR 7.
           03 LINE 10 COL 25 PIC X(10) 
                             TO ENV-ACCESS-MODE FOREGROUND-COLOR 7.
       
       01  SCREEN-ENVIRONMENT-READ.
           03 BLANK SCREEN.
           03 LINE 02 COL 02 
              VALUE "READ FILE GENERATOR" FOREGROUND-COLOR 3.

       01  SCREEN-ENVIRONMENT-WRITE.
           03 BLANK SCREEN.
           03 LINE 02 COL 02 
              VALUE "WRITE FILE GENERATOR" FOREGROUND-COLOR 3.

       01  SCREEN-ENVIRONMENT-SORT.
           03 BLANK SCREEN.
           03 LINE 02 COL 02 
              VALUE "SORT FILE GENERATOR" FOREGROUND-COLOR 3.

       01  SCREEN-FILE-SECTION FOREGROUND-COLOR 2.
           03 BLANK SCREEN.
           03 LINE 02 COL 02 
              VALUE "FILE SECTION GENERATOR" FOREGROUND-COLOR 3.
           03 LINE 04 COL 02 VALUE "FILE SECTION :".
           03 LINE 05 COL 05 VALUE "RECORD LENGTH  :".
           03 LINE 06 COL 05 VALUE "RECORDING MODE :".
           03 LINE 05 COL 22 PIC X(05) 
                             TO RECORD-LENGTH FOREGROUND-COLOR 7.
           03 LINE 06 COL 22 PIC X(01) 
                             TO RECORDING-MODE FOREGROUND-COLOR 7.

      ****************************************************************** 

       PROCEDURE DIVISION.
       0000-START-MAIN.
           PERFORM 1000-START-OPTIONS 
              THRU END-1000-OPTIONS.
           PERFORM 2000-START-IDENTIFICATION 
              THRU END-2000-IDENTIFICATION.

           IF OPTION-1 EQUAL 1 OR OPTION-2 EQUAL 2 
           OR OPTION-3 EQUAL 4 OR OPTION-4 EQUAL 4
               PERFORM 3000-START-ENVIRONMENT 
                  THRU END-3000-ENVIRONMENT
           END-IF.

           PERFORM 4000-START-FILE-SECTION
              THRU END-4000-FILE-SECTION.

           PERFORM 4000-START-WS 
              THRU END-4000-WS.

           PERFORM 5000-START-PROCEDURE
              THRU END-5000-PROCEDURE.
       END-0000-MAIN.
           STOP RUN.

      ******************************************************************
       1000-START-OPTIONS.
           ACCEPT SCREEN-OPTIONS.
       END-1000-OPTIONS.
           EXIT.

      ******************************************************************
       2000-START-IDENTIFICATION.
           OPEN OUTPUT F-OUTPUT.

           STRING PNT-BLANK-6 PNT-AST
           DELIMITED BY SIZE INTO PNT-BLANK-AST.

           WRITE R-OUTPUT FROM PNT-BLANK-AST.
           WRITE R-OUTPUT FROM PNT-BLANK-AST.
           WRITE R-OUTPUT FROM PNT-BLANK-6.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "IDENTIFICATION DIVISION."
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "PROGRAM-ID. myprog.cbl."
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.
       
           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "AUTHOR. YourName."
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           WRITE R-OUTPUT FROM PNT-BLANK-6.

           CLOSE F-OUTPUT.
       END-2000-IDENTIFICATION.
           EXIT.

      ******************************************************************
       3000-START-ENVIRONMENT.
           OPEN EXTEND F-OUTPUT.

           WRITE R-OUTPUT FROM PNT-BLANK-AST.
           WRITE R-OUTPUT FROM PNT-BLANK-6.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "ENVIRONMENT DIVISION."
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "INPUT-OUTPUT SECTION."
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "FILE-CONTROL."
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.

           IF OPTION-1 EQUAL 1
               DISPLAY SCREEN-ENVIRONMENT-READ
               PERFORM 3100-START-FILE-CONTROL 
               THRU END-3100-FILE-CONTROL
           END-IF.

           IF OPTION-2 EQUAL 2
               DISPLAY SCREEN-ENVIRONMENT-WRITE
               PERFORM 3100-START-FILE-CONTROL 
               THRU END-3100-FILE-CONTROL
           END-IF.

           IF OPTION-3 EQUAL 3
               DISPLAY SCREEN-ENVIRONMENT-SORT
               PERFORM 3100-START-FILE-CONTROL 
               THRU END-3100-FILE-CONTROL
           END-IF.

           IF OPTION-4 EQUAL 4
               PERFORM 3100-START-FILE-CONTROL 
               THRU END-3100-FILE-CONTROL
           END-IF.

           INITIALIZE R-OUTPUT.
           WRITE R-OUTPUT FROM PNT-BLANK-6.

           CLOSE F-OUTPUT.
       END-3000-ENVIRONMENT.
           EXIT.

      ******************************************************************
       3100-START-FILE-CONTROL.
           ACCEPT SCREEN-ENVIRONMENT.

           MOVE FUNCTION UPPER-CASE(LOGICAL-FILE-NAME) 
           TO LOGICAL-FILE-NAME.
           MOVE FUNCTION UPPER-CASE(ENV-ORGANIZATION) 
           TO ENV-ORGANIZATION.
           MOVE FUNCTION UPPER-CASE(ENV-ACCESS-MODE) 
           TO ENV-ACCESS-MODE.

           INITIALIZE R-OUTPUT.
           WRITE R-OUTPUT FROM PNT-BLANK-6.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-11 "SELECT F-" 
           FUNCTION TRIM(LOGICAL-FILE-NAME)
           SPACE 'ASSIGN TO "'
           FUNCTION TRIM(READ-FILE-NAME) '"'
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-11 "ORGANIZATION IS" SPACE 
           FUNCTION TRIM(ENV-ORGANIZATION)
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-11 "ACCESS MODE IS" SPACE 
           FUNCTION TRIM(ENV-ACCESS-MODE)
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-11 "FILE STATUS IS FS-" 
           FUNCTION TRIM(LOGICAL-FILE-NAME) "."
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.
       END-3100-FILE-CONTROL.
           EXIT.

      ******************************************************************
       4000-START-FILE-SECTION.
           OPEN EXTEND F-OUTPUT.
           
           WRITE R-OUTPUT FROM PNT-BLANK-AST.
           WRITE R-OUTPUT FROM PNT-BLANK-6.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "DATA DIVISION."
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.

           IF OPTION-1 EQUAL 1
               ACCEPT SCREEN-FILE-SECTION
           
               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-7 "FILE SECTION."
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-7 "FD F-" 
               FUNCTION TRIM(LOGICAL-FILE-NAME)
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-11 "RECORD CONTAINS" SPACE 
               FUNCTION TRIM(RECORD-LENGTH) SPACE "CHARACTERS"
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-11 "RECORDING MODE IS" SPACE 
               RECORDING-MODE "."
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-7 "01  R-"
               FUNCTION TRIM(LOGICAL-FILE-NAME)
               SPACE "PIC X(" 
               FUNCTION TRIM(RECORD-LENGTH) ")."
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT
           END-IF.

           CLOSE F-OUTPUT.    
       END-4000-FILE-SECTION.

      ******************************************************************
       4000-START-WS.
           OPEN EXTEND F-OUTPUT.

           WRITE R-OUTPUT FROM PNT-BLANK-6.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "WORKING-STORAGE SECTION."
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.

           IF OPTION-1 EQUAL 1
               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-7 "01  FS-"
               FUNCTION TRIM(LOGICAL-FILE-NAME) SPACE
               "PIC X(02)."
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-11 "88 FS-"
               FUNCTION TRIM(LOGICAL-FILE-NAME)
               '-OK VALUE "00".'
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-11 "88 FS-"
               FUNCTION TRIM(LOGICAL-FILE-NAME)
               '-EOF VALUE "10".'
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT
           END-IF.

           CLOSE F-OUTPUT.
       END-4000-WS.

      ******************************************************************
       5000-START-PROCEDURE.
           OPEN EXTEND F-OUTPUT.
           
           WRITE R-OUTPUT FROM PNT-BLANK-6.
           WRITE R-OUTPUT FROM PNT-BLANK-AST.
           WRITE R-OUTPUT FROM PNT-BLANK-6.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "PROCEDURE DIVISION."
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "0000-START-MAIN."
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.

           IF OPTION-13 EQUAL 13
               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-11 'DISPLAY "HELLO COBOL".'
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT
           END-IF.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "END-0000-MAIN."
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-11 "STOP RUN."
           DELIMITED BY SIZE INTO R-OUTPUT.
           WRITE R-OUTPUT.

           IF OPTION-1 EQUAL 1
               INITIALIZE R-OUTPUT
               WRITE R-OUTPUT FROM PNT-BLANK-6
               WRITE R-OUTPUT FROM PNT-BLANK-AST

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-7 "START-READ-INPUT."
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-11 "OPEN INPUT F-INPUT."
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-11 'IF FS-INPUT EQUAL "00"'
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-14 "SET FS-INPUT-OK TO TRUE"
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               WRITE R-OUTPUT FROM PNT-BLANK-6

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-14 "PERFORM UNTIL FS-INPUT-EOF"
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-17 "READ F-INPUT"
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-17 "AT END"
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-20 "SET FS-INPUT-EOF TO TRUE"
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-17 "NOT AT END"
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-6 "*" PNT-BLANK-11 SPACE
               SPACE "Traitement..."
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-17 "END-READ"
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-14 "END-PERFORM"
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-11 "ELSE"
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-14 'DISPLAY "ERROR :" SPACE FS-INPUT'
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-11 "END-IF."
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-11 "CLOSE F-INPUT."
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-7 "END-READ-INPUT."
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-11 "EXIT."
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT
           END-IF.

           IF OPTION-14 EQUAL 14
               INITIALIZE R-OUTPUT
               WRITE R-OUTPUT FROM PNT-BLANK-6
               WRITE R-OUTPUT FROM PNT-BLANK-AST

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-7 "1000-START-NAME."
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-6 "*    Paragraphe..." 
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-7 "END-1000-NAME."
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-11 "EXIT."
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT
           END-IF.

           CLOSE F-OUTPUT.
       END-5000-PROCEDURE.
