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

           INITIALIZE R-OUTPUT.
           IF OPTION-13 EQUAL 13
               STRING PNT-BLANK-11 'DISPLAY "HELLO COBOL".'
               DELIMITED BY SIZE INTO R-OUTPUT
               WRITE R-OUTPUT
           ELSE
               STRING PNT-BLANK-11 "Main paragraphe..."
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
               STRING PNT-BLANK-14 
               'DISPLAY "ERROR READ FILE :" SPACE FS-INPUT'
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
