      ******************************************************************
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. gencpy.
       AUTHOR        Rémi.

      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT ASSIGN 
           TO "COBOL_FICHIER_MODELE_POUR_CLAUSE_COPY_Exercice.txt"
      *    TO "test.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-INPUT.

           SELECT F-OUTPUT ASSIGN TO "output.cpy"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-OUTPUT.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 5 TO 1000 CHARACTERS
           RECORDING MODE IS V.
       01  R-INPUT PIC X(230).

       FD  F-OUTPUT
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.
       01  R-OUTPUT PIC X(80).

       WORKING-STORAGE SECTION.
       01  FS-INPUT PIC X(02).
           88 FS-INPUT-OK VALUE "00".
           88 FS-INPUT-EOF VALUE "10".

       01  FS-OUTPUT PIC X(02).
           88 FS-OUTPUT-OK VALUE "00".

       01  PHRASE.
           03 M-CNT PIC 9(04) VALUE 1.
           03 TABLE-MOT OCCURS 1 TO 1000 TIMES
                     DEPENDING ON M-CNT
                     INDEXED BY M-IDX.
               05 MOT PIC X(40).
               05 FILLER-LENGTH PIC 9(02).
               05 MOT-LENGTH PIC 9(02) VALUE 1.

       01  R-IDX              PIC 9(04) VALUE 1.
       01  M-POS              PIC 9(04) VALUE 1.
       01  R-LENGTH           PIC 9(05).
      *01  R-SPACES           PIC 9(05).
      *01  N-MOT              PIC 9(03).

       01  PRINT.
           03 START-FILLER PIC X(27) 
           VALUE "           03 FILLER PIC X(".

      ****************************************************************** 
       PROCEDURE DIVISION.
       START-MAIN.
           PERFORM START-R-IP THRU END-R-IP.
       END-MAIN.
           STOP RUN.

      ******************************************************************
       START-R-IP.
           OPEN INPUT F-INPUT
                OUTPUT F-OUTPUT.
           IF FS-INPUT EQUAL "00"
              SET FS-INPUT-OK TO TRUE
               
              PERFORM UNTIL FS-INPUT-EOF
                 READ F-INPUT 
                 AT END 
                    SET FS-INPUT-EOF TO TRUE
                 NOT AT END 
                    PERFORM START-SPLIT-PHRASE-LOOP 
                       THRU END-SPLIT-PHRASE-LOOP

                    PERFORM START-W-OP THRU END-W-OP
                  END-READ
              END-PERFORM
           ELSE
              DISPLAY "ERREUR :" SPACE FS-INPUT
           END-IF.    
           CLOSE F-INPUT.
       END-R-IP.
           EXIT.

      ******************************************************************
       START-SPLIT-PHRASE-LOOP.
           INSPECT R-INPUT TALLYING R-LENGTH FOR CHARACTERS.
      *    INSPECT R-INPUT TALLYING R-SPACES FOR ALL SPACES.

           PERFORM VARYING R-IDX FROM 1 BY 1 UNTIL R-IDX > R-LENGTH
               IF R-INPUT(R-IDX:1) NOT EQUAL SPACE
                  MOVE R-INPUT(R-IDX:1) 
                  TO MOT(M-CNT)(MOT-LENGTH(M-CNT):1)
           
                  ADD 1 TO MOT-LENGTH(M-CNT)
               ELSE 
                   ADD 1 TO FILLER-LENGTH(M-CNT)
                   SET M-POS TO 1
                   ADD 1 TO M-CNT
               END-IF
           END-PERFORM.
       END-SPLIT-PHRASE-LOOP.
           EXIT.

      ******************************************************************
       START-W-OP.
           OPEN OUTPUT F-OUTPUT.
           MOVE "       01 GROUPE." TO R-OUTPUT.
           WRITE R-OUTPUT.

           PERFORM VARYING M-IDX FROM 1 BY 1 UNTIL M-IDX > M-CNT
               IF MOT(M-IDX) NOT EQUAL SPACE
                   INITIALIZE R-OUTPUT
                   SUBTRACT 1 FROM MOT-LENGTH(M-IDX)

                   STRING START-FILLER 
                   MOT-LENGTH(M-IDX) ") VALUE" SPACE 
                   "'"FUNCTION TRIM(MOT(M-IDX)) "'."
                   DELIMITED BY SIZE
                   INTO R-OUTPUT
               
                   WRITE R-OUTPUT 

                   INITIALIZE R-OUTPUT
                   STRING START-FILLER 
                   FILLER-LENGTH(M-IDX) ") VALUE SPACES."
                   DELIMITED BY SIZE
                   INTO R-OUTPUT
               
                   WRITE R-OUTPUT
                    
      *            ADD 1 TO N-MOT
               END-IF
           END-PERFORM.

           CLOSE F-OUTPUT.
       END-W-OP.
           EXIT.
           