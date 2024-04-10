       IDENTIFICATION DIVISION.
       PROGRAM-ID. tble.
       AUTHOR. Remi.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TEST-DATA.
           03 FILLER PIC X(33) VALUE "0001HOKKAI       TARO       0400".
           03 FILLER PIC X(33) VALUE "0002AOMORI       JIRO       0350".
           03 FILLER PIC X(33) VALUE "0003AKITA        SABURO     0300".
           03 FILLER PIC X(33) VALUE "0004IWATE        SHIRO      0900".
           03 FILLER PIC X(33) VALUE "0005MIYAGI       GORO       0200".
           03 FILLER PIC X(33) VALUE "0006FUKUSHIMA    RIKURO     0150".
           03 FILLER PIC X(33) VALUE "0007TOCHIGI      SHICHIRO   0100".
           03 FILLER PIC X(33) VALUE "0008IBARAKI      HACHIRO    1050".
           03 FILLER PIC X(33) VALUE "0009GUMMA        KURO       0200".
           03 FILLER PIC X(33) VALUE "0010SAITAMA      JURO       0350".

       01  WS-TABLE.
           05 WS-PERSON-RANDOM OCCURS 10 TIMES.
               10 WS-ID        PIC 9(04).
               10 WS-FIRSTNAME PIC X(13).
               10 WS-LASTNAME  PIC X(11).
               10 WS-NUM       PIC 9(04).

       01  WS-INDEX  PIC 9(02) VALUE 0.
       01  WS-INCREMENT PIC 9(03) VALUE 1.

       PROCEDURE DIVISION.
      *    Séquence répétée 10 fois
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 10
      *        Transfert de TEST-DATA
      *        à partir de l'emplacement WS-INCREMENT de longueur 33
      *        pour la personne de ma table à l'index WS-INDEX
               MOVE TEST-DATA(WS-INCREMENT:33)
               TO WS-PERSON-RANDOM(WS-INDEX)

      *        Affichage d'une personne de ma table
               DISPLAY WS-ID(WS-INDEX) SPACE WS-FIRSTNAME(WS-INDEX)
               SPACE WS-LASTNAME(WS-INDEX) SPACE WS-NUM(WS-INDEX)

      *        Incrementation de 33 de WS-INCREMENT
               COMPUTE WS-INCREMENT = WS-INCREMENT + 33
           END-PERFORM.

      *    Affichage des 3 personnes souhaitées
           DISPLAY "--------------------------------------------------".
           DISPLAY WS-ID(2) SPACE FUNCTION TRIM(WS-FIRSTNAME(2))
           SPACE FUNCTION TRIM(WS-LASTNAME(2)) SPACE WS-NUM(2)

           DISPLAY WS-ID(5) SPACE FUNCTION TRIM(WS-FIRSTNAME(5))
           SPACE FUNCTION TRIM(WS-LASTNAME(5)) SPACE WS-NUM(5)

           DISPLAY WS-ID(10) SPACE FUNCTION TRIM(WS-FIRSTNAME(10))
           SPACE FUNCTION TRIM(WS-LASTNAME(10)) SPACE WS-NUM(10)
           DISPLAY "--------------------------------------------------".

           STOP RUN.

