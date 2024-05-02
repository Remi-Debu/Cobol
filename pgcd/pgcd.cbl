      ******************************************************************
      *    Le programme demande à l'utilisateur de saisir 2 nombres    *
      *    puis calcul et affiche le PGCD (Plus Grand Commun Diviseur) *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. pgcd.
       AUTHOR.     Rémi.

      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUM-GROUPE.        
           03 WS-NUM1 PIC 9(10).
           03 WS-NUM2 PIC 9(10).
           03 WS-QUOT PIC 9(10).
           03 WS-REST PIC 9(10).
           03 WS-TEMP PIC 9(10).
           03 WS-PGCD PIC Z(09)9.

       01  WS-CONTINUER PIC X(03) VALUE "OUI".
           88 WS-OUI VALUE "OUI".
           88 WS-NON VALUE "NON".

       01  WS-SAISIE.
           03 WS-SAISIE-NUM1     PIC X(10).
           03 WS-SAISIE-NUM2     PIC X(10).
           03 WS-SAISIE-CONTINUE PIC X(25).
              
       01  WS-AFFICHAGE.
           03 WS-AST         PIC X(43).
           03 WS-BLANK1      PIC X(12).
           03 WS-BLANK2      PIC X(19).
           03 WS-MESSAGE-NUM PIC X(33).

      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM 0000-MAIN THRU 0000-MAIN-END.
           STOP RUN.

      ******************************************************************
      *    Appel 
      ******************************************************************
       0000-MAIN.
           PERFORM 1000-AFFICHAGE-BIENVENUE 
              THRU 1000-AFFICHAGE-BIENVENUE-END.

           PERFORM 1000-BOUCLE 
              THRU 1000-BOUCLE-END.

           PERFORM 1000-AFFICHER-FIN 
              THRU 1000-AFFICHER-FIN-END.
       0000-MAIN-END.
           EXIT.

       1000-BOUCLE.
           PERFORM UNTIL WS-NON
           
           PERFORM 1000-AFFICHAGE-SAISI 
              THRU 1000-AFFICHAGE-SAISI-END
           PERFORM 2000-SAISIE-CONTROLE 
              THRU 2000-SAISIE-CONTROLE-END
           PERFORM 2000-INVERSER-NUM 
              THRU 2000-INVERSER-NUM-END
           PERFORM 2000-DIVISION 
              THRU 2000-DIVISION-END
           PERFORM 2000-CALCUL-PGCD 
              THRU 2000-CALCUL-PGCD-END
           PERFORM 1000-AFFICHAGE-PGCD 
              THRU 1000-AFFICHAGE-PGCD-END
           PERFORM 1000-CONTINUER 
              THRU 1000-CONTINUER-END

           END-PERFORM.
       1000-BOUCLE-END.
           EXIT.

       1000-AFFICHAGE-BIENVENUE.
           MOVE ALL "*" TO WS-AST.

           DISPLAY WS-AST.
           DISPLAY "*" WS-BLANK1 "CALCULATRICE PGCD" WS-BLANK1 "*".
           DISPLAY WS-AST.
           DISPLAY SPACE.
       1000-AFFICHAGE-BIENVENUE-END.
           EXIT.

       1000-AFFICHAGE-SAISI.
           MOVE "Saisir un nombre entier positif :" TO WS-MESSAGE-NUM.

           DISPLAY "1." SPACE WS-MESSAGE-NUM SPACE NO ADVANCING
           ACCEPT WS-SAISIE-NUM1.

           DISPLAY "2." SPACE WS-MESSAGE-NUM SPACE NO ADVANCING.
           ACCEPT WS-SAISIE-NUM2.
       1000-AFFICHAGE-SAISI-END.
           EXIT.

      *    Contrôle si les 2 saisis sont des numériques.
      *    sinon affiche un message d'erreur et redirige l'utilisateur
      *    vers la demande de saisi d'un nombre.
      *    Les conditions seront fausses s'il y a dans la saisie 
      *    des symboles tels que (-, +, ., etc) ou des lettres
       2000-SAISIE-CONTROLE.
      *    Contrôle de la 1ere saisi
           IF FUNCTION TRIM(WS-SAISIE-NUM1) IS NUMERIC
              MOVE WS-SAISIE-NUM1 TO WS-NUM1
           ELSE 
              DISPLAY "Saisi incorrect !"
              DISPLAY "Veuillez entrer un nombre ENTIER et POSITIF,"
              SPACE "d'une longueur maximale de 10."
              DISPLAY SPACE 

              GO TO 1000-AFFICHAGE-SAISI
           END-IF.

      *    Contrôle de la 2ème saisi
           IF FUNCTION TRIM(WS-SAISIE-NUM2) IS NUMERIC
              MOVE WS-SAISIE-NUM2 TO WS-NUM2
           ELSE 
              DISPLAY "Saisi incorrect !"
              DISPLAY "Veuillez entrer un nombre ENTIER et POSITIF,"
              SPACE "d'une longueur maximale de 10."
              DISPLAY SPACE 
              
              GO TO 1000-AFFICHAGE-SAISI
           END-IF.
       2000-SAISIE-CONTROLE-END.
           EXIT.

      *    Inverse les valeurs WS-NUM1 et WS-NUM2,
      *    si WS-NUM1 est plus petit que WS-NUM2
       2000-INVERSER-NUM.
           IF WS-NUM1 < WS-NUM2
               MOVE WS-NUM1 TO WS-TEMP
               MOVE WS-NUM2 TO WS-NUM1
               MOVE WS-TEMP  TO WS-NUM2
           END-IF.
       2000-INVERSER-NUM-END.
           EXIT.

       2000-DIVISION.
           DIVIDE    WS-NUM1
           BY        WS-NUM2
           GIVING    WS-QUOT
           REMAINDER WS-REST.
       2000-DIVISION-END.
           EXIT.

      *    Boucle jusqu'à ce que le reste soit inférieur ou égal à 0,
      *    remplace les valeurs WS-NUM1 et WS-NUM2
      *    par les valeurs WS-NUM2 et WS-REST
      *    pour ensuite refaire la division
       2000-CALCUL-PGCD.
           PERFORM UNTIL WS-REST <= 0
               MOVE WS-NUM2 TO WS-NUM1
               MOVE WS-REST  TO WS-NUM2

               PERFORM 2000-DIVISION THRU 2000-DIVISION-END
           END-PERFORM.
       2000-CALCUL-PGCD-END.
           EXIT.

       1000-AFFICHAGE-PGCD.    
           IF WS-REST <= 0
              MOVE WS-NUM2 TO WS-PGCD
              DISPLAY SPACE
              DISPLAY "Le PGCD est" SPACE FUNCTION TRIM(WS-PGCD) "!"
           END-IF.
       1000-AFFICHAGE-PGCD-END.
           EXIT.

      *    Demande à l'utilisateur s'il souhaite continuer en fonction 
      *    de sa saisi (OUI / NON).
      *    Saisi non sensible à la casse
      *    et affiche un message d'erreur pour une saisie incorrecte
       1000-CONTINUER.
           DISPLAY SPACE.
           DISPLAY "Souhaitez-vous continuer ? (OUI / NON)" SPACE 
           WITH NO ADVANCING.
           ACCEPT WS-SAISIE-CONTINUE.

           MOVE FUNCTION UPPER-CASE(WS-SAISIE-CONTINUE) 
           TO WS-SAISIE-CONTINUE.

           EVALUATE WS-SAISIE-CONTINUE
               WHEN "OUI"
                 SET WS-OUI TO TRUE
               WHEN "NON"
                 SET WS-NON TO TRUE
               WHEN OTHER
                  DISPLAY "La commande saisie est incorrecte." 
                  DISPLAY "Veuillez entrer 'oui' ou 'non'"

                  GO TO 1000-CONTINUER
           END-EVALUATE.
       1000-CONTINUER-END.
           EXIT.

       1000-AFFICHER-FIN.
           DISPLAY SPACE.
           DISPLAY WS-AST.
           DISPLAY "*" WS-BLANK2 "FIN" WS-BLANK2 "*".
           DISPLAY WS-AST.
       1000-AFFICHER-FIN-END.
           EXIT.
