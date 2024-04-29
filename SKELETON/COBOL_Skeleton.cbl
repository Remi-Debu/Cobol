       IDENTIFICATION DIVISION.
       PROGRAM-ID. SKELETON.
	   AUTHOR. SIMPLON.
      *******************************************************
      *    Cobol programme structure exemple
      ******************************************************************
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           SELECT FILE-A ASSIGN TO 'fichier.filea.txt'
		     FILE-STATUS IS WS-FILE-A
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.		   
           
		   SELECT FILE-B ASSIGN TO 'fichier.fileb.txt'
		     FILE-STATUS IS WS-FILE-A
             ORGANIZATION IS LINE SEQUENTIAL
             ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILE-A
	       RECORD CONTAINS nnn CHARACTERS
           BLOCK CONTAINS  nnn CHARACTERS 
           DATA RECORD IS RFILE-A
		   FILE STATUS IS WS-FILE-A
           RECORDING MODE IS F.   

       01 RFILE-A.
           03 [champ de donnée]				PIC X(nn).
		   03 [suite des données]           PIC 9(nn).

       FD  FILE-B
	       RECORD CONTAINS nnn CHARACTERS
           BLOCK CONTAINS  nnn CHARACTERS 
           DATA RECORD IS RFILE-B
		   FILE STATUS IS WS-FILE-B
           RECORDING MODE IS F.   

       01 RFILE-B.
           03 [champ de donnée]				PIC X(nn).
		   03 [suite des données]           PIC 9(nn).

       WORKING-STORAGE SECTION.
	   01  WS-FILE-A                     	PIC 9(02).
	   01  WS-FILE-B                     	PIC 9(02).
      *
      * zone de communication
      *
	  [LINKAGE SECTION.
         data-description-entry...]
       PROCEDURE DIVISION.
      *
      ******************************************************** 
       D1-00-FILE-A-PROBLEM SECTION. 
          USE AFTER STANDARD ERROR PROCEDURE ON FILE-A. 
      * 
      * Si une instruction d'accès au fichier pour FILE-A aboutit à une
      * erreur, D1-00-FILE-A-PROBLEM s'exécute.
      * 
      * 
       D1-01-FILE-A-PROBLEM. 
          PERFORM D9-00-REPORT-FILE-STATUS. 
          . 
          . 
      ******************************************************** 
       D2-00-FILE-INPUT-PROBLEM SECTION. 
          USE AFTER STANDARD EXCEPTION PROCEDURE ON INPUT. 
      * 
      * If an error occurs for any file open 
      * in the INPUT mode except FILE-A, 
      * D2-00-FILE-INPUT-PROBLEM executes. 
      * 

       D2-01-FILE-INPUT-PROBLEM. 
          PERFORM D9-00-REPORT-FILE-STATUS. 
      
      ******************************************************** 
       D3-00-FILE-OUTPUT-PROBLEM SECTION. 
          USE AFTER STANDARD EXCEPTION PROCEDURE ON OUTPUT. 
      * 
      * Si une erreur se produit pour un fichier ouvert
      * en mode SORTIE FILE-B sauf FILE-A, puis
      * D3-00-FILE-OUTPUT-PROBLEM exécuté. 
      * 
       D3-01-FILE-OUTPUT-PROBLEM. 
          PERFORM D9-00-REPORT-FILE-STATUS. 
          . 
      ******************************************************** 
       D4-00-FILE-I-O-PROBLEM SECTION. 
          USE AFTER STANDARD EXCEPTION PROCEDURE ON I-O. 
      * 
      * Si une erreur se produit pour un fichier ouvert
      * en mode ENTREE-SORTIE sauf FILE-A,
      * D4-00-FILE-I-O-PROBLEM s'exécute.
      * 
       D4-01-FILE-I-O-PROBLEM. 
          PERFORM D9-00-REPORT-FILE-STATUS. 
          . 
          . 
      ******************************************************** 
       D5-00-FILE-EXTEND-PROBLEM SECTION. 
          USE AFTER STANDARD EXCEPTION PROCEDURE ON EXTEND. 
      * 
      * Si une erreur se produit pour un fichier ouvert
      * en mode EXTEND sauf FILE-A,
      * D5-00-FILE-EXTEND-PROBLEM s'exécute.
      * 
      * 
       D5-01-FILE-EXTEND-PROBLEM. 
           PERFORM D9-00-REPORT-FILE-STATUS. 
           . 
           . 
           D9-00-REPORT-FILE-STATUS. 
           . 
           . 
       END DECLARATIVES. 
      ******************************************************************
      *    Créé le fichier vide. Spécifique windows*unix
      ******************************************************************
       S01-PRINCIPAL SECTION.
       P01-OUVRIR-FICHIER.

	       OPEN INPUT FILE-A.
	       OPEN OUTPUT FILE-B.
           CLOSE FILE-B.		   
           OPEN EXTEND FILE-B.
       
	       PERFORM Pnn-LECTURE-FICHIERS UNTIL WS-FILE-A NOT = 00.
	   
			Pnn-TRAITEMENT
		   END-PERFORM.
	   
	       PERFORM Pnn-FERMETURE-FICHIER.
	   
	       GOBACK.
      ******************************************************************
      * LECTURE DES FICHIERS
      ******************************************************************
       Pnn-LECTURE-FICHIERS.           
		   READ FILE-A
		     AT END PERFORM Pnn-REPORT-FILE-STATUS
			 NOT 	AT END
		   
		   END-READ.

      ******************************************************************
      *    Ouvre le fichier en mode Ajout
      ******************************************************************
       Pnn-TRAITEMENT.    

           .
		   .

      ******************************************************************
      * FERMETURE DES FICHIERS  
      ******************************************************************
       Pnn-FERMETURE-FICHIER.           
           
           CLOSE FILE-A
           CLOSE FILE-B.
		   
	   Pnn-REPORT-FILE-STATUS.
	       EVALUATE TRUE
		   WHEN WS-FILE-A = 00
		     DISPLAY "Fin de fichier"
		   WHEN WS-FILE-A = 10 
		     DISPLAY "Fin de fichier"
		   WHEN WS-FILE-A = 01 
		     DISPLAY "Clé File status n'est pas valide"
		   WHEN WS-FILE-A = 02
		     DISPLAY "Enregistrement en double."					  
		   WHEN WS-FILE-A = 03 
		     DISPLAY "Code File status n'est pas valide"					
		   WHEN WS-FILE-A = 04
		     DISPLAY "Longueur enregistrement n'est pas valide"		
		   WHEN WS-FILE-A = 10 
		     DISPLAY "Fin de fichier."		
 		   WHEN WS-FILE-A = 14 
		     DISPLAY "Le numéro d'enregistrement relatif indiqué "
		             "dépasse le maximum pour ce fichier."		
 		   WHEN WS-FILE-A = 21 
		     DISPLAY "Lors des ÉCRITES séquentielles, la clé est "
                     "inférieure à la clé précédemment écrite. "
			 DISPLAY "Ou encore, lors de la RÉÉCRITURE d'un "
			         "enregistrement existant, le programme "
					 "d'application a modifié la clé de "
					 "l'enregistrement.".
 		   WHEN WS-FILE-A = 22 
		     DISPLAY "Une tentative d'ÉCRITURE d'un nouvel "
			         "enregistrement a été tentée pour une "
					 "clé qui correspond à un enregistrement"
                     " actuellement dans le fichier.".
 		   WHEN WS-FILE-A = 23
		     DISPLAY "L'enregistrement (clé) demandé "
			         "n'a pas été trouvé dans le fichier.".		
 		   WHEN WS-FILE-A = 24 
		     DISPLAY "De l'espace DASD supplémentaire était "
			         "requis pour cette demande, mais il n'a"
					 " pas pu être obtenu. Pour les fichiers"
                     " au format compatible, cela indique que"
                     " la zone Débordement indépendant a été remplie."
					 " Pour les fichiers au format amélioré, "
					 "soit l'espace était insuffisant pour développer"
					 " le fichier, soit le fichier avait atteint "
					 "l'extension maximale autorisée, soit "
					 "16 extensions par volume DASD. "
					 "Généralement, cette erreur nécessite "
					 "que le fichier soit réorganisé, nécessitant "
					 " éventuellement un DELETE et un DEFINE pour "
					 "allouer plus d'espace DASD."
             DISPLAY "Pour les fichiers au format amélioré, si la "
			         "cause est un espace DASD insuffisant et si "
					 "certains ensembles de données existants peuvent "
                     "être supprimés du volume DASD, il peut être "
					 "possible de réessayer ultérieurement la demande.".
"		
 		   WHEN WS-FILE-A = 34 
		     DISPLAY "Le dossier est plein.".		
 		   WHEN WS-FILE-A = 35 
		     DISPLAY "OUVERTURE incorrecte d'un fichier vide "
                     "(déchargé), par exemple ouvert en "
					 "entrée uniquement.".
		   WHEN WS-FILE-A = 37 
		     DISPLAY "Mode invalide pour OUVERT.".				   
		   WHEN WS-FILE-A = 39 
		     DISPLAY "Cet état de fichier implique généralement une "
                     "inadéquation entre les attributs du fichier "
					 "définis et le cliché d'enregistrement dans "
					 "le programme COBOL.". 
			 DISPLAY "Par exemple, les causes possibles sont que la "
			         "longueur de clé ou le décalage par rapport à "
                     "la définition de fichier ne correspondent pas "
                     "à la présentation d'enregistrement COBOL.".
             DISPLAY "De plus, la longueur maximale d'enregistrement "
			         "définie pour le fichier est plus courte que la "
					 "longueur maximale possible pour les "
					 "enregistrements de longueur variable. ".
			 DISPLAY "COBOL exige que la longueur d'enregistrement "
					 "définie soit au moins aussi longue que la "
					 "taille d'enregistrement théorique maximale, à "
					 "partir de la mise en page.".	
		WHEN WS-FILE-A = 41 
		     DISPLAY "Un OPEN a été demandé p/un fichier déjà ouvert."
		   WHEN WS-FILE-A = 42
		     DISPLAY "Un CLOSE a été émis pour un dossier déjà fermé.".							   
		WHEN WS-FILE-A = 43 
		     DISPLAY "Une demande de mise à jour de fichier"
                     "(c'est-à-dire REWRITE ou DELETE) a été émise "
                     "sans READ préalable pour UPDATE.".					
		WHEN WS-FILE-A = 44
		     DISPLAY "Une longueur d'enregistrement incorrecte a été "
                     "spécifiée sur WRITE. "
	         DISPLAY "Soit la longueur était plus courte que le "
			         "minimum, c'est-à-dire la longueur de clé plus "
					 "le décalage de clé, soit l'enregistrement était"
                     " plus long que la taille d'enregistrement"
					 " maximale définie pour le fichier.".
        WHEN WS-FILE-A = 46 
		     DISPLAY "Une LECTURE a échoué parce que l'application "
			         "n'a pas réussi à établir une position dans le"
					 " fichier ou parce qu'une LECTURE a été tentée "
					 "après que la fin du fichier ait été atteinte.".		
 		   WHEN WS-FILE-A = 47 
		     DISPLAY "Une requête READ a été émise pour un fichier "
			         "qui n'a pas été ouvert en entrée ou en E/S.".
 		   WHEN WS-FILE-A = 48 
		     DISPLAY "Une requête WRITE a été émise pour un fichier"
                     " non ouvert pour la sortie ou "
					 "les E/S (mise à jour).".		
 		   WHEN WS-FILE-A = 49 
		     DISPLAY "Une requête DELETE ou REWRITE a été émise "
			         "pour un fichier qui n'a pas été ouvert "
					 "pour les E/S (mise à jour)."		
 		   WHEN WS-FILE-A = 90
		     DISPLAY "Erreur logique. Code d'erreur logique"
                     " possible x(E1).".		
 		   WHEN WS-FILE-A = 92 
		     DISPLAY "Erreur de logique."
             DISPLAY "Les attributs de fichier du programme COBOL,"
             DISPLAY " tels que la longueur de clé, la position"
                     " relative de la clé ou la longueur de l'enre"
					 "gistrement, ne correspondent pas au fichier "
                     "auquel vous accédez.".
 		   WHEN WS-FILE-A = 93 
		     DISPLAY 
			 "Enregistrer les conflits de verrouillage ou les erreurs "
			 " de contrôle exclusif.".
             DISPLAY 
			 "Cela peut également être dû à un stockage virtuel insuff"
			 "isant pour traiter la demande.". 
             DISPLAY
			 "Très probablement, le paramètre REGION doit être augmenté"
             " pour le travail.".		
 		   WHEN WS-FILE-A = 94 
		     DISPLAY "Non positionné pour requête READ séquentielle".
		   WHEN WS-FILE-A = 95 
		     DISPLAY "Informations fichier invalides ou incomplètes.".
		   WHEN WS-FILE-A = 96 
		     DISPLAY "La carte DD est manquante pour ce fichier.".							
          END-EVALUATE.					
					
					
					