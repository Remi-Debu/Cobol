      ******************************************************************
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

       01  NAME-IDENTIFICATION.
           03 PROGRAM-ID-NAME PIC X(08).
           03 AUTHOR-NAME     PIC X(20).

       01  NAME-ENVIRONMENT.
           03 READ-FILE-NAME  PIC X(20).

       SCREEN SECTION.
       01  SCREEN-OPTIONS.
           03 LINE 02 COL 02 VALUE "GENERATEUR DE PROGRAMME".
           03 LINE 04 COL 02 VALUE "PARTIE FICHIER :".
           03 LINE 05 COL 05 VALUE "01. Lire un fichier.".
           03 LINE 06 COL 05 VALUE "02. Ecrire un fichier.".
           03 LINE 07 COL 05 VALUE "03. Trier un fichier.".
           03 LINE 08 COL 05 VALUE "04. Fusionner plusieurs fichiers.".
           03 LINE 10 COL 02 VALUE "PARTIE SQL :".
           03 LINE 11 COL 05 VALUE "05. Acceder a une BDD.".
           03 LINE 12 COL 05 VALUE "06. Generer une requete COUNT(*).".
           03 LINE 13 COL 05 VALUE "07. Generer un curseur.".
           03 LINE 14 COL 05 VALUE "08. Generer une requete UPDATE.".
           03 LINE 16 COL 02 VALUE "PARTIE SOUS-ROUTINES :".
           03 LINE 17 COL 05 VALUE "09. Creer une sous-routine.".
           03 LINE 18 COL 05 VALUE "10. Integrer un appel type.".
           03 LINE 20 COL 02 VALUE "PARTIE WORKING-STORAGE :".
           03 LINE 21 COL 05 VALUE "11. Preparer un Copybook.".
           03 LINE 22 COL 05 VALUE "12. Integrer un Copybook.".
           03 LINE 24 COL 02 VALUE "PARTIE PROCEDURE :".
           03 LINE 25 COL 05 VALUE "13. Ajouter un Hello Cobol.".
           03 LINE 26 COL 05 VALUE "14. Integrer des paragraphes.".
           03 LINE 05 COL 02 PIC X(02) TO OPTION-1.
           03 LINE 06 COL 02 PIC X(02) TO OPTION-2.
           03 LINE 07 COL 02 PIC X(02) TO OPTION-3.
           03 LINE 08 COL 02 PIC X(02) TO OPTION-4.
           03 LINE 11 COL 02 PIC X(02) TO OPTION-5.
           03 LINE 12 COL 02 PIC X(02) TO OPTION-6.
           03 LINE 13 COL 02 PIC X(02) TO OPTION-7.
           03 LINE 14 COL 02 PIC X(02) TO OPTION-8.
           03 LINE 17 COL 02 PIC X(02) TO OPTION-9.
           03 LINE 18 COL 02 PIC X(02) TO OPTION-10.
           03 LINE 21 COL 02 PIC X(02) TO OPTION-11.
           03 LINE 22 COL 02 PIC X(02) TO OPTION-12.
           03 LINE 25 COL 02 PIC X(02) TO OPTION-13.
           03 LINE 26 COL 02 PIC X(02) TO OPTION-14.

       01  SCREEN-IDENTIFICATION.
           03 BLANK SCREEN.
           03 LINE 02 COL 02 VALUE "GENERATEUR DE PROGRAMME".
           03 LINE 04 COL 02 VALUE "IDENTIFICATION DIVISION :".
           03 LINE 05 COL 05 VALUE "PROGRAM-ID.".
           03 LINE 06 COL 05 VALUE "AUTHOR.".
           03 LINE 05 COL 17 PIC X(08) TO PROGRAM-ID-NAME.
           03 LINE 06 COL 13 PIC X(20) TO AUTHOR-NAME.

       01  SCREEN-ENVIRONMENT.
           03 BLANK SCREEN.
           03 LINE 02 COL 02 VALUE "GENERATEUR DE PROGRAMME".
           03 LINE 04 COL 02 VALUE "ENVIRONMENT DIVISION :".
           03 LINE 05 COL 05 VALUE "READ-FILE-NAME :".
           03 LINE 05 COL 17 PIC X(22) TO READ-FILE-NAME.

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
       END-0000-MAIN.
           STOP RUN.

      ******************************************************************
       1000-START-OPTIONS.
           DISPLAY SCREEN-OPTIONS.
           ACCEPT SCREEN-OPTIONS.
           DISPLAY OPTIONS-NUM.
       END-1000-OPTIONS.
           EXIT.

      ******************************************************************
       2000-START-IDENTIFICATION.
           DISPLAY SCREEN-IDENTIFICATION.
           ACCEPT SCREEN-IDENTIFICATION.
           
           OPEN OUTPUT F-OUTPUT.

           STRING PNT-BLANK-6 PNT-AST
           DELIMITED BY SIZE
           INTO PNT-BLANK-AST.

           WRITE R-OUTPUT FROM PNT-BLANK-AST.
           WRITE R-OUTPUT FROM PNT-BLANK-AST.
           WRITE R-OUTPUT FROM PNT-BLANK-6.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "IDENTIFICATION DIVISION."
           DELIMITED BY SIZE
           INTO R-OUTPUT.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "PROGRAM-ID." SPACE 
           FUNCTION TRIM(PROGRAM-ID-NAME) "."
           DELIMITED BY SIZE
           INTO R-OUTPUT.
           WRITE R-OUTPUT.
       
           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "AUTHOR." SPACE 
           FUNCTION TRIM(AUTHOR-NAME) "."
           DELIMITED BY SIZE
           INTO R-OUTPUT.
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
           DELIMITED BY SIZE
           INTO R-OUTPUT.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "INPUT-OUTPUT SECTION."
           DELIMITED BY SIZE
           INTO R-OUTPUT.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING PNT-BLANK-7 "FILE-CONTROL."
           DELIMITED BY SIZE
           INTO R-OUTPUT.
           WRITE R-OUTPUT.

           IF OPTION-1 EQUAL 1
               DISPLAY SCREEN-ENVIRONMENT
               ACCEPT SCREEN-ENVIRONMENT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-7 'SELECT F-OUTPUT ASSIGN TO "'
               FUNCTION TRIM(READ-FILE-NAME) '"'
               DELIMITED BY SIZE
               INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-7 "ORGANIZATION IS LINE SEQUENTIAL"
               DELIMITED BY SIZE
               INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-7 "ACCESS MODE IS SEQUENTIAL"
               DELIMITED BY SIZE
               INTO R-OUTPUT
               WRITE R-OUTPUT

               INITIALIZE R-OUTPUT
               STRING PNT-BLANK-7 "FILE STATUS IS FS-OUTPUT."
               DELIMITED BY SIZE
               INTO R-OUTPUT
               WRITE R-OUTPUT
           END-IF.

           INITIALIZE R-OUTPUT.
           WRITE R-OUTPUT FROM PNT-BLANK-6.

           CLOSE F-OUTPUT.
       END-3000-ENVIRONMENT.
           EXIT.


           