      ******************************************************************
      *    Le programme lis un fichier puis écris un rapport de        *
      *    synthèse avec les données lues                              *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. testech.
      
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           SELECT F-INPUT ASSIGN TO 'datassur.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-INPUT.

           SELECT F-OUTPUT ASSIGN TO 'rapport_de_synthese.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-OUTPUT.

       DATA DIVISION.
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 122 CHARACTERS
           RECORDING MODE IS F.
       01  R-INPUT.
           03 R-ID         PIC X(08).
           03 R-SEPARATOR  PIC X(01).
           03 R-GROUP      PIC X(14).
           03 FILLER       PIC X(01).
           03 R-TYPE       PIC X(14).
           03 FILLER       PIC X(01).
           03 R-LABEL      PIC X(41).
           03 FILLER       PIC X(01).
           03 R-STATUS     PIC X(08).
           03 FILLER       PIC X(01).
           03 R-DATE-START.
               05 R-D-S-YYYY PIC X(04).
               05 R-D-S-MM   PIC X(02).
               05 R-D-S-DD   PIC X(02).
           03 FILLER       PIC X(01).
           03 R-DATE-END.
               05 R-D-E-YYYY PIC X(04).
               05 R-D-E-MM   PIC X(02).
               05 R-D-E-DD   PIC X(02).
           03 FILLER       PIC X(01).
           03 R-AMOUNT     PIC X(09).
           03 FILLER       PIC X(01).
           03 R-SYMBOL     PIC X(03).

       FD  F-OUTPUT.
       01  R-OUTPUT PIC X(200).

       WORKING-STORAGE SECTION.
       01  FS-INPUT PIC X(02).
           88 FS-INPUT-OK  VALUE '00'.
           88 FS-INPUT-EOF VALUE '88'.

       01  FS-OUTPUT PIC X(02).
           88 FS-OUTPUT-OK VALUE '00'.

       01  PRINT.
           03 PNT-AST         PIC X(131) VALUE ALL '*'.
           03 PNT-BLANK-30    PIC X(30) VALUE ALL SPACES.
           03 PNT-SEPARATOR   PIC X(01).
           03 PNT-NUM         PIC Z(03)9.

       01  WS-CURRENT-DATE PIC X(08).

       01  INSURENCE-TABLE.
           03 I-CNT PIC 9(04) VALUE 1.
           03 INSERENCE OCCURS 1 TO 1000 TIMES 
                        DEPENDING ON I-CNT
                        INDEXED BY I-IDX.
               05 INS-ID         PIC X(08).
               05 INS-GROUP      PIC X(14).
               05 INS-TYPE       PIC X(14).
               05 INS-LABEL      PIC X(41).
               05 INS-STATUS     PIC X(08).
               05 INS-DATE-START.
                   07 INS-D-S-DD   PIC X(02).
                   07 INS-D-S-MM   PIC X(02).
                   07 INS-D-S-YYYY PIC X(04).
               05 INS-DATE-END.
                   07 INS-D-E-DD   PIC X(02).
                   07 INS-D-E-MM   PIC X(02).
                   07 INS-D-E-YYYY PIC X(04).
               05 INS-AMOUNT     PIC X(09).
               05 INS-SYMBOL     PIC X(03).
       
      ******************************************************************
       PROCEDURE DIVISION.
       0000-START-MAIN.
           PERFORM 1000-START-READ THRU END-1000-READ.
           PERFORM 2000-START-WRITE THRU END-2000-WRITE.
       END-0000-MAIN.
           STOP RUN.

      ******************************************************************
      *    Lis le fichier et appel le paragraphe 'HANDLE'.             *
      ******************************************************************
       1000-START-READ. 
           OPEN INPUT F-INPUT.
           IF FS-INPUT EQUAL "00"
              SET FS-INPUT-OK TO TRUE

              PERFORM UNTIL FS-INPUT-EOF
                 READ F-INPUT
                 AT END
                    SUBTRACT 1 FROM I-CNT
                    SET FS-INPUT-EOF TO TRUE
                 NOT AT END
                       PERFORM 1100-START-HANDLE THRU END-1100-HANDLE
                 END-READ
              END-PERFORM
           ELSE
              DISPLAY "ERROR :" SPACE FS-INPUT
           END-IF.
           CLOSE F-INPUT.
       END-1000-READ.
           EXIT.

      ******************************************************************
      *    Transfert les données lues dans la table 'INSURENCE'.       *
      ******************************************************************
       1100-START-HANDLE.
           MOVE R-ID         TO INS-ID(I-CNT).
           MOVE R-SEPARATOR  TO PNT-SEPARATOR.
           MOVE R-GROUP      TO INS-GROUP(I-CNT).
           MOVE R-TYPE       TO INS-TYPE(I-CNT).
           MOVE R-LABEL      TO INS-LABEL(I-CNT).
           MOVE R-STATUS     TO INS-STATUS(I-CNT).
           MOVE R-D-S-YYYY   TO INS-D-S-YYYY(I-CNT).
           MOVE R-D-S-MM     TO INS-D-S-MM(I-CNT).
           MOVE R-D-S-DD     TO INS-D-S-DD(I-CNT).
           MOVE R-D-E-YYYY   TO INS-D-E-YYYY(I-CNT).
           MOVE R-D-E-MM     TO INS-D-E-MM(I-CNT).
           MOVE R-D-E-DD     TO INS-D-E-DD(I-CNT).
           MOVE R-AMOUNT     TO INS-AMOUNT(I-CNT).
           MOVE R-SYMBOL     TO INS-SYMBOL(I-CNT).

           ADD 1 TO I-CNT.
       END-1100-HANDLE.
           EXIT.

      ******************************************************************
      *    Appel des différents paragraphes d'écriture du rapport de   *
      *    synthèse.                                                   *
      ******************************************************************
       2000-START-WRITE.
           OPEN OUTPUT F-OUTPUT.
           PERFORM 2100-START-WRITE-HEADER THRU END-2100-WRITE-HEADER.
           PERFORM 2200-START-WRITE-BODY   THRU END-2200-WRITE-BODY.
           PERFORM 2300-START-WRITE-FOOTER THRU END-2300-WRITE-FOOTER.
           CLOSE F-OUTPUT.
       END-2000-WRITE.
           EXIT.

      ******************************************************************
      *    Ecrite de l'en-tête du rapport.                             *
      ******************************************************************
       2100-START-WRITE-HEADER.
           INITIALIZE R-OUTPUT.
           STRING 
                 PNT-BLANK-30 "RAPPORT DE SYNTHESE"
                 DELIMITED BY SIZE
                 INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
                 PNT-BLANK-30 "Rémi DEBUSSCHERE"
                 DELIMITED BY SIZE
                 INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
      *    MOVE FUNCTION DATE-COMPILED(WS-CURRENT-DATE).
           STRING 
                 PNT-BLANK-30 WS-CURRENT-DATE
                 DELIMITED BY SIZE
                 INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.
       END-2100-WRITE-HEADER.
           EXIT.

      ******************************************************************
      *    Ecriture du corps du rapport.                               *
      ******************************************************************
       2200-START-WRITE-BODY.
           INITIALIZE R-OUTPUT.
           WRITE R-OUTPUT FROM PNT-BLANK-30.

           INITIALIZE R-OUTPUT.
           WRITE R-OUTPUT FROM PNT-AST.

           INITIALIZE R-OUTPUT.
           MOVE 'ID' TO R-OUTPUT.
           MOVE '||GROUPE' TO R-OUTPUT(09:8).
           MOVE '||TYPE' TO R-OUTPUT(25:6).
           MOVE '||LABEL' TO R-OUTPUT(41:7).
           MOVE '||STATUS' TO R-OUTPUT(84:8).
           MOVE '||DATE DEBUT' TO R-OUTPUT(94:12).
           MOVE '||DATE FIN' TO R-OUTPUT(106:10).
           MOVE '||MONTANT' TO R-OUTPUT(118:9).
           MOVE '||' TO R-OUTPUT(130:2).
           WRITE R-OUTPUT.
       
           PERFORM VARYING I-IDX FROM 1 BY 1 UNTIL I-IDX > I-CNT
               INITIALIZE R-OUTPUT
               STRING 
                     INS-ID(I-IDX) '||'
                     INS-GROUP(I-IDX) '||'      
                     INS-TYPE(I-IDX) '||'      
                     INS-LABEL(I-IDX) '||'     
                     INS-STATUS(I-IDX) '||'    
                     INS-D-S-DD(I-IDX) '-' INS-D-S-MM(I-IDX) '-' 
                     INS-D-S-YYYY(I-IDX) '||'
                     INS-D-E-DD(I-IDX) '-' INS-D-E-MM(I-IDX) '-' 
                     INS-D-E-YYYY(I-IDX) '||'
                     INS-AMOUNT(I-IDX) INS-SYMBOL(I-IDX) '||'    
                     DELIMITED BY SIZE
                     INTO R-OUTPUT
               END-STRING
               WRITE R-OUTPUT
           END-PERFORM.
       END-2200-WRITE-BODY.
           EXIT.

      ******************************************************************
      *    Ecrite du pied de page du rapport.                          *
      ******************************************************************
       2300-START-WRITE-FOOTER.
           INITIALIZE R-OUTPUT.
           WRITE R-OUTPUT FROM PNT-BLANK-30.

           INITIALIZE R-OUTPUT.
           WRITE R-OUTPUT FROM PNT-AST.

           INITIALIZE R-OUTPUT.
           MOVE I-CNT TO PNT-NUM
           STRING 
                 "Nombre d'enregistrements :" SPACE PNT-NUM
                 DELIMITED BY SIZE
                 INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           INITIALIZE R-OUTPUT.
           STRING 
                 "Délimiteur :" SPACE FUNCTION TRIM(PNT-SEPARATOR)
                 DELIMITED BY SIZE
                 INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.
       END-2300-WRITE-FOOTER.
           EXIT.
