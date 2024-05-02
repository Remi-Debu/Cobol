      ******************************************************************
      *    Le programme demande à l'utilisateur de saisir 2 nombres et *
      *    additionne, soustrait, divise et multiplie ces 2 nombres    *
      *    pour ensuite afficher les calculs et les resultats.         *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. operatn.
       AUTHOR.     Rémi.

      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUM-GROUP.
           03 WS-NUM-A            PIC 9(10).
           03 WS-NUM-B            PIC 9(10).
           03 WS-ADD-RESULT       PIC 9(11).
           03 WS-SUBTRACT-RESULT  PIC 9(11).
           03 WS-DIVIDE-RESULT    PIC 9(11).
           03 WS-MULTIPLY-RESULT  PIC 9(11).
           03 WS-TEMP             PIC 9(10).

      ******************************************************************
       PROCEDURE DIVISION.
      *    Affichages et saisis
           DISPLAY "Saisir un premier nombre :" 
           SPACE WITH NO ADVANCING.
           ACCEPT WS-NUM-A.

           DISPLAY "Saisir un deuxieme nombre :" 
           SPACE WITH NO ADVANCING.
           ACCEPT WS-NUM-B.

      *    Si WS-NUM-A est plus petit que WS-NUM-B
      *    inverse les valeurs WS-NUM-A et WS-NUM-B
           IF WS-NUM-A < WS-NUM-B
               MOVE WS-NUM-A TO WS-TEMP
               MOVE WS-NUM-B TO WS-NUM-A
               MOVE WS-TEMP  TO WS-NUM-B
           END-IF.
      
      *    Opérations
           ADD      WS-NUM-A TO   WS-NUM-B GIVING WS-ADD-RESULT.
           SUBTRACT WS-NUM-B FROM WS-NUM-A GIVING WS-SUBTRACT-RESULT.
           DIVIDE   WS-NUM-A BY   WS-NUM-B GIVING WS-DIVIDE-RESULT.
           MULTIPLY WS-NUM-A BY   WS-NUM-B GIVING WS-MULTIPLY-RESULT.


      *    Affichages des calculs
           DISPLAY WS-NUM-A SPACE "+" SPACE WS-NUM-B SPACE 
           "=" SPACE WS-ADD-RESULT.

           DISPLAY WS-NUM-A SPACE "-" SPACE WS-NUM-B SPACE
           "=" SPACE WS-SUBTRACT-RESULT.

           DISPLAY WS-NUM-A SPACE "/" SPACE WS-NUM-B SPACE
           "=" SPACE WS-DIVIDE-RESULT.
           
           DISPLAY WS-NUM-A SPACE "x" SPACE WS-NUM-B SPACE
           "=" SPACE WS-MULTIPLY-RESULT.

           STOP RUN.
