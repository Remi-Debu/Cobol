      ******************************************************************
      *    Le programme permet de lire un fichier et à partir de ce    *
      *    fichier générer un copybook composer uniquement de filler   *
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
           RECORD CONTAINS 1 TO 1000 CHARACTERS
           RECORDING MODE IS V.
       01  R-INPUT PIC X(219).

       FD  F-OUTPUT
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.
       01  R-OUTPUT PIC X(80).

       WORKING-STORAGE SECTION.
       01  FS-INPUT PIC X(02).
           88 FS-INPUT-OK  VALUE "00".
           88 FS-INPUT-EOF VALUE "10".

       01  FS-OUTPUT PIC X(02).
           88 FS-OUTPUT-OK VALUE "00".

       01  INPUT-LINE.
           03 W-CNT PIC 9(04) VALUE 1.
           03 WORD-ARRAY OCCURS 1 TO 1000 TIMES
                     DEPENDING ON W-CNT
                     INDEXED BY W-IDX.
               05 WORD        PIC X(40).
               05 WORD-LENGTH PIC 9(02).
               05 SEP-LENGTH  PIC 9(02).

       01  R-IDX              PIC 9(04) VALUE 1.

       01  CB-FORMAT.
           03 CB-GROUP PIC X(16) 
           VALUE "       01 GROUP.".
           03 CB-START-FILLER  PIC X(27) 
           VALUE "           03 FILLER PIC X(".
           03 CB-VALUE-FILLER  PIC X(08) 
           VALUE ") VALUE ".
           03 CB-SPACES-FILLER PIC X(07) 
           VALUE "SPACES.".

      ****************************************************************** 
       PROCEDURE DIVISION.
       START-MAIN.
           PERFORM START-R-IP THRU END-R-IP.
           PERFORM START-W-OP THRU END-W-OP.
       END-MAIN.
           STOP RUN.

      ******************************************************************
      *    Lis le fichier input et appel le paragraphe
      ******************************************************************
       START-R-IP.
           OPEN INPUT F-INPUT.
           IF FS-INPUT EQUAL "00"
              SET FS-INPUT-OK TO TRUE
               
              PERFORM UNTIL FS-INPUT-EOF
                 READ F-INPUT 
                 AT END 
                    SET FS-INPUT-EOF TO TRUE
                 NOT AT END 
                    PERFORM START-SPLIT-INPUT-LINE 
                       THRU END-SPLIT-INPUT-LINE
                  END-READ
              END-PERFORM
           ELSE
              DISPLAY "ERREUR :" SPACE FS-INPUT
           END-IF.    
           CLOSE F-INPUT.
       END-R-IP.
           EXIT.

      ******************************************************************
      *    Parcours chaque caractères du string R-INPUT pour stocker   *
      *    chaque mot dans le tableau "WORD-ARRAY" ainsi que le nombre *
      *    d'espaces qui sépare ces mots.                              *        
      ******************************************************************
       START-SPLIT-INPUT-LINE.
           PERFORM VARYING R-IDX FROM 1 BY 1 
                   UNTIL R-IDX > FUNCTION LENGTH(R-INPUT)
       
      *       SI le caractères est un espace
      *       ajoute 1 au nombre d'espaces
      *       ET SI le prochain caractères n'est pas un espace
      *       ajoute 1 au compteur de mot
              IF R-INPUT(R-IDX:1) EQUAL SPACE 
                 ADD 1 TO SEP-LENGTH(W-CNT)

                 IF R-INPUT(R-IDX + 1:1) NOT EQUAL SPACE
                    ADD 1 TO W-CNT
                 END-IF

      *       SINON le caractère n'est pas un espace
      *       ajoute 1 à la longueur du mot et
      *       ajoute le caractère au mot dans le tableau
              ELSE 
                 ADD 1 TO WORD-LENGTH(W-CNT)

      *          MOVE "le caractère"
                 MOVE R-INPUT(R-IDX:1) 
      *          TO "le mot du tableau à la position qui dépend de la
      *          longueur du mot actuelle"
                 TO WORD(W-CNT)(WORD-LENGTH(W-CNT):1)
              END-IF
           END-PERFORM.
       END-SPLIT-INPUT-LINE.
           EXIT.

      ******************************************************************
      *    Ecris le copybook en fonction du contenu du tableau de mots *
      ******************************************************************
       START-W-OP.
           OPEN OUTPUT F-OUTPUT.

      *    Ecris le 01 GROUP
           WRITE R-OUTPUT FROM CB-GROUP.

           PERFORM VARYING W-IDX FROM 1 BY 1 UNTIL W-IDX >= W-CNT
              IF WORD(W-IDX) NOT EQUAL SPACE
                 INITIALIZE R-OUTPUT
       
      *          Ecris le filler contenant le mot
                 STRING CB-START-FILLER 
                        WORD-LENGTH(W-IDX) 
                        CB-VALUE-FILLER 
                    "'" FUNCTION TRIM(WORD(W-IDX)) "'."
                 DELIMITED BY SIZE
                 INTO R-OUTPUT
                 WRITE R-OUTPUT 
       
      *          Ecris le filler de sépartion contenant que des SPACES    
                 INITIALIZE R-OUTPUT
                 STRING CB-START-FILLER 
                        SEP-LENGTH(W-IDX) 
                        CB-VALUE-FILLER
                        CB-SPACES-FILLER
                 DELIMITED BY SIZE
                 INTO R-OUTPUT
                 WRITE R-OUTPUT
              END-IF
           END-PERFORM.
           CLOSE F-OUTPUT.
       END-W-OP.
           EXIT.
           