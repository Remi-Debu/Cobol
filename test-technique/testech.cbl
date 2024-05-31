      ******************************************************************
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
       FD  F-INPUT.
       01  R-INPUT.
           03 R-ID PIC X(08).
           03 FILLER PIC X(01).
           03 R-GROUP PIC X(14).
           03 FILLER PIC X(01).
           03 R-TYPE PIC X(14).
           03 FILLER PIC X(01).
           03 R-LABEL PIC X(41).
           03 FILLER PIC X(01).
           03 R-STATUS PIC X(08).
           03 FILLER PIC X(01).
           03 R-DATE-START PIC X(08).
           03 FILLER PIC X(01).
           03 R-DATE-END PIC X(08).
           03 FILLER PIC X(01).
           03 R-AMOUNT PIC X(09).
           03 FILLER PIC X(01).
           03 R-SYMBOL PIC X(03).

       FD  F-OUTPUT.
       01  R-OUTPUT PIC X(120).

       WORKING-STORAGE SECTION.
       01  FS-INPUT PIC X(02).
           88 FS-INPUT-OK  VALUE '00'.
           88 FS-INPUT-EOF VALUE '88'.

       01  FS-OUTPUT PIC X(02).
           88 FS-OUTPUT-OK VALUE '00'.

       01  PRINT.
           03 PNT-AST PIC X(120) VALUE ALL '*'.
           03 PNT-BLANK-30 PIC X(30) VALUE ALL SPACES.

       01  WS-CURRENT-DATE PIC X(08).

       01  INSURENCE.
           03 INS-ID PIC X(8).

       
      ******************************************************************
       PROCEDURE DIVISION.
       0000-START-MAIN.
           PERFORM 2000-START-WRITE THRU END-2000-WRITE.
       END-0000-MAIN.
           STOP RUN.

      ******************************************************************
       1000-START-READ. 
           OPEN INPUT F-INPUT.
           IF FS-INPUT EQUAL "00"
              SET FS-INPUT-OK TO TRUE

              PERFORM UNTIL FS-INPUT-EOF
                 READ F-INPUT
                 AT END
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
       1100-START-HANDLE.
           MOVE R
       END-1100-HANDLE.
           EXIT.

      ******************************************************************
       2000-START-WRITE.
           PERFORM 2100-START-WRITE-HEADER THRU END-2100-WRITE-HEADER.
           PERFORM 2200-START-WRITE-BODY   THRU END-2200-WRITE-BODY.
           PERFORM 2300-START-WRITE-FOOTER THRU END-2300-WRITE-FOOTER.
       END-2000-WRITE.
           EXIT.

      ******************************************************************
       2100-START-WRITE-HEADER.
           OPEN OUTPUT F-OUTPUT.
           
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

           MOVE FUNCTION CURRENT-DATE('DDMMYYYY') TO WS-CURRENT-DATE.
           STRING 
                 PNT-BLANK-30 WS-CURRENT-DATE
                 DELIMITED BY SIZE
                 INTO R-OUTPUT
           END-STRING.
           WRITE R-OUTPUT.

           CLOSE F-OUTPUT.
       END-2100-WRITE-HEADER.
           EXIT.

      ******************************************************************
       2200-START-WRITE-BODY.
           
       END-2200-WRITE-BODY.
           EXIT.

      ******************************************************************
       2300-START-WRITE-FOOTER.
           
       END-2300-WRITE-FOOTER.
           EXIT.
