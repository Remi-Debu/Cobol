      ******************************************************************
      *    Le programe lis le fichier "train1.dat" puis écris ses      *
      *    données triées par station dans l'ordre alphabétique dans   *
      *    le fichier "train3.dat".                                    *
      *    Ensuite réécris le fichier "train3.dat" sous forme de       *
      *    rapport pour le rendre plus lisible et ajoute les données   *
      *    heure d'arrivée et nombre d'arrêt aux données déjà          *
      *    présentes dans le fichier.                                  *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. train.
       AUTHOR.     Rémi.
      
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-TRAIN1 ASSIGN TO "train1.dat"
           ACCESS MODE IS SEQUENTIAL
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-TRAIN1.

           SELECT SORT-TRAIN1 ASSIGN TO "train1.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

           SELECT F-TRAIN3 ASSIGN TO "train3.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-TRAIN3.


      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  F-TRAIN1
           RECORD CONTAINS 27 TO 100 CHARACTERS 
           RECORDING MODE IS V.
       01  R-TRAIN1 PIC X(100).

       SD  SORT-TRAIN1 
           RECORD CONTAINS 21 TO 100 CHARACTERS 
           RECORDING MODE IS V.
       01  TRAIN1-REC.
           03 FILLER PIC X(03).
           03 TRAIN1-STATION PIC X(18).

       FD  F-TRAIN3
           RECORD CONTAINS 200 CHARACTERS 
           RECORDING MODE IS F.   
       COPY train1fd.
       01  R-TRAIN3 PIC X(200).
       

       WORKING-STORAGE SECTION.
       COPY train3fd.

       01  FS-TRAIN1 PIC X(02).

       01  FS-TRAIN3 PIC X(02).
           88 FS-TRAIN3-OK  VALUE "0".
           88 FS-TRAIN3-EOF VALUE "10".

       01  WS-PRINT.
           03 WS-PNT-AST    PIC X(66).
           03 WS-PNT-BLANK  PIC X(27).
           03 WS-PNT-STRING PIC X(100).
           03 WS-PNT-NBR    PIC ZZZZ9.

       01  HALT-IDX         PIC 9(05).
       01  WS-NUM-TEMP      PIC 9(02).

      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM START-MAIN THRU END-MAIN.
           STOP RUN.

      ******************************************************************
      *    MAIN qui appel les différents paragraphes qui vont trier,   *
      *    lire et écrire le fichier "train3.dat".                     *
      ******************************************************************
       START-MAIN.
           MOVE ALL "*" TO WS-PNT-AST.
           
           PERFORM START-SORT-TRAIN THRU END-SORT-TRAIN.
           PERFORM START-R-TRAIN3 THRU END-R-TRAIN3.
           PERFORM START-W-HEADER THRU END-W-HEADER.
           PERFORM START-W-TRAIN3 THRU END-W-TRAIN3.
           PERFORM START-W-FOOTER THRU END-W-FOOTER.
       END-MAIN.
      
      ******************************************************************
      *    Trie les données du fichier "train1.dat" dans l'ordre       *
      *    alphabétique des stations puis les écris dans le fichier    *
      *    "train3.dat".                                               * 
      ******************************************************************
       START-SORT-TRAIN.
           SORT SORT-TRAIN1
           ON ASCENDING KEY TRAIN1-STATION 
           USING F-TRAIN1
           GIVING F-TRAIN3.
       END-SORT-TRAIN.

      ******************************************************************
      *    Lis le fichier "train3.dat".                                *
      ******************************************************************
       START-R-TRAIN3.
           OPEN INPUT F-TRAIN3.
      
           IF FS-TRAIN3 EQUAL "00"
              SET FS-TRAIN3-OK TO TRUE
              PERFORM UNTIL FS-TRAIN3-EOF 
                 READ F-TRAIN3
                    AT END SET FS-TRAIN3-EOF TO TRUE
                 NOT AT END
                    PERFORM START-HANDLE-TRAIN THRU END-HANDLE-TRAIN
              END-PERFORM
           ELSE
              DISPLAY "ERREUR :" SPACE FS-TRAIN3
           END-IF.
           CLOSE F-TRAIN3.
       END-R-TRAIN3.

      ******************************************************************
      *    Stock les données lus dans le fichier "train3.dat" dans la  *
      *    Working Storage.                                            *
      ******************************************************************
       START-HANDLE-TRAIN.
           MOVE RECORD-TYPE      TO T-TYPE(T-CNT).
           MOVE STATION-DEPART   TO T-STATION-DEPART(T-CNT).
           MOVE TRAIN-TIME       TO TRAIN-TIME-START(T-CNT).
           MOVE TRAIN-NBR-HEURES TO T-NBR-HOURS(T-CNT).

           PERFORM VARYING HALT-IDX FROM 1 BY 1 UNTIL HALT-IDX > 10
              MOVE TRAIN-HALT-FLAG(HALT-IDX) 
              TO T-HALT-FLAG(T-CNT)(HALT-IDX:1)
           END-PERFORM.

           ADD 1 TO T-CNT.
       END-HANDLE-TRAIN.

      ******************************************************************
      *    Écris l'en-tête du rapport.                                 *
      ******************************************************************
       START-W-HEADER.
           OPEN OUTPUT F-TRAIN3.
           INITIALIZE WS-PNT-STRING.
           
           STRING WS-PNT-BLANK "TRAIN PLANNING"
           DELIMITED BY SIZE 
           INTO WS-PNT-STRING.

           WRITE R-TRAIN3 FROM WS-PNT-AST.
           WRITE R-TRAIN3 FROM WS-PNT-STRING.
           WRITE R-TRAIN3 FROM WS-PNT-AST.

           INITIALIZE WS-PNT-STRING.

           STRING "TYPE"
           DELIMITED BY SIZE 
           INTO WS-PNT-STRING. 

           STRING "|| STATION"
           DELIMITED BY SIZE 
           INTO WS-PNT-STRING(6:22).

           STRING "|| DÉPART"
           DELIMITED BY SIZE 
           INTO WS-PNT-STRING(28:10).

           STRING "|| ARRIVÉE"
           DELIMITED BY SIZE 
           INTO WS-PNT-STRING(39:11).

           STRING "|| DURÉE"
           DELIMITED BY SIZE 
           INTO WS-PNT-STRING(51:9).

           STRING "|| ARRÊT"
           DELIMITED BY SIZE 
           INTO WS-PNT-STRING(61:9).
           
           WRITE R-TRAIN3 FROM WS-PNT-STRING.
           WRITE R-TRAIN3 FROM WS-PNT-BLANK . 
           CLOSE F-TRAIN3. 
       END-W-HEADER.

      ******************************************************************
      *    Écris les données qui ont été stocké dans la WS dans le     *
      *    rapport.                                                    *
      ******************************************************************
       START-W-TRAIN3.
           OPEN EXTEND F-TRAIN3.
           PERFORM VARYING T-IDX FROM 1 BY 1 UNTIL T-IDX >= T-CNT
              INITIALIZE WS-PNT-STRING
              INITIALIZE WS-PNT-NBR 

      *       Compte le nombre d'arrêt
              INSPECT T-HALT-FLAG(T-IDX) 
              TALLYING T-NBR-HALT(T-IDX) 
              FOR ALL "H"

              MOVE T-NBR-HALT(T-IDX) TO WS-PNT-NBR

              PERFORM START-TIME-END THRU END-TIME-END

              STRING T-TYPE(T-IDX)
              DELIMITED BY SIZE 
              INTO WS-PNT-STRING 

              STRING "||" SPACE T-STATION-DEPART(T-IDX)
              DELIMITED BY SIZE 
              INTO WS-PNT-STRING(6:22)

              STRING "||" SPACE T-START-HH(T-IDX) ":" T-START-MM(T-IDX)
              DELIMITED BY SIZE 
              INTO WS-PNT-STRING(28:10)

              STRING "||" SPACE T-END-HH(T-IDX) ":" T-END-MM(T-IDX)
              DELIMITED BY SIZE 
              INTO WS-PNT-STRING(38:11)

              STRING "||" SPACE  T-NBR-HOURS(T-IDX)
              DELIMITED BY SIZE 
              INTO WS-PNT-STRING(49:9)

              STRING "||" SPACE FUNCTION TRIM(WS-PNT-NBR)
              DELIMITED BY SIZE 
              INTO WS-PNT-STRING(58:9)
              
              WRITE R-TRAIN3 FROM WS-PNT-STRING
           END-PERFORM.
           CLOSE F-TRAIN3.
       END-W-TRAIN3.

      ******************************************************************
      *    Calcul l'heure d'arrivée du train en fonction de son heure  *
      *    de départ et de la durée du trajet.                         *
      ******************************************************************
       START-TIME-END.
           MOVE T-START-MM(T-IDX) TO T-END-MM(T-IDX).
      
      *    Ajoute la durée du trajet et l'heure de départ à l'heure
      *    d'arrivée
           ADD T-NBR-HOURS(T-IDX) TO T-START-HH(T-IDX) 
           GIVING T-END-HH(T-IDX).

      *    Si l'heure d'arrivée est strictement supérieure à 24
      *    soustrait 24 à l'heure d'arrivée tant que celle-ci est 
      *    strictement supérieure à 24.
           IF T-END-HH(T-IDX) > 24
              PERFORM UNTIL T-END-HH(T-IDX) < 25
                 SUBTRACT 24 FROM T-END-HH(T-IDX) 
                 GIVING T-END-HH(T-IDX)
              END-PERFORM
           END-IF.
       END-TIME-END. 

      ******************************************************************
      *    Écris le pied de page du rapport qui notamment le nombre    *
      *    d'enregistrements que contient le rapport.                  *
      ******************************************************************
       START-W-FOOTER.
           OPEN EXTEND F-TRAIN3.

           WRITE R-TRAIN3 FROM WS-PNT-AST.
      
      *    Nombre d'enregistrement
           INITIALIZE WS-PNT-STRING.
           INITIALIZE WS-PNT-NBR.

           MOVE T-CNT TO WS-NUM-TEMP.

           SUBTRACT 1 FROM WS-NUM-TEMP.

           MOVE WS-NUM-TEMP TO WS-PNT-NBR.

           STRING "NOMBRE D'ENREGISTREMENTS :" SPACE 
           FUNCTION TRIM(WS-PNT-NBR)
           DELIMITED BY SIZE 
           INTO WS-PNT-STRING.
           
           WRITE R-TRAIN3 FROM WS-PNT-STRING.

      *    Fin
           INITIALIZE WS-PNT-STRING.
           STRING WS-PNT-BLANK "FIN"
           DELIMITED BY SIZE 
           INTO WS-PNT-STRING.

           WRITE R-TRAIN3 FROM WS-PNT-AST.
           WRITE R-TRAIN3 FROM WS-PNT-STRING.
           CLOSE F-TRAIN3. 
       END-W-FOOTER.
