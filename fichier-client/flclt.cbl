       IDENTIFICATION DIVISION.
       PROGRAM-ID. flclt.
       AUTHOR.     Remi.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-EMPLOYEE ASSIGN TO "fichierclient.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

           SELECT F-DEPT ASSIGN TO "fr-liste-dept.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

           SELECT F-CLISOR ASSIGN TO "employee-dept.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           COPY FCLIENT REPLACING ==:CLIENT:== BY ==EMPLOYEE==.
           COPY FDEPT.
           COPY FCLISOR.

       WORKING-STORAGE SECTION.
       COPY FRENTETE.

       01  WS-FS-CLIENT PIC X(02).

       01  WS-TABLE.
           03 WS-DEPT OCCURS 1 TO 200 TIMES
                   DEPENDING ON WS-INDEX.
               05 WS-DEPT-ID     PIC X(03).
               05 WS-DEPT-DEP    PIC X(23) VALUE "DEP".
               05 WS-DEPT-REGION PIC X(26) VALUE "REGION".

       01  WS-STRING        PIC X(40).
       01  WS-SALAIRE-TEMP  PIC 9(7).
       01  WS-SALAIRE-TOTAL PIC 9(7).
       01  WS-SALAIRE-CLEAN PIC ZZZZZZ9.
       01  WS-STOP          PIC 9(01) VALUE 0.
       01  WS-INDEX         PIC 9(03) VALUE 1.

       PROCEDURE DIVISION.
      *    ENTETE
           OPEN OUTPUT F-CLISOR.

           WRITE R-CLISOR FROM R-ENTETE.

           CLOSE F-CLISOR.

      *    LECTURE FICHIER DEPARTEMENT
           OPEN INPUT F-DEPT.
       
           PERFORM UNTIL WS-STOP = 1
               READ F-DEPT
               AT END
                   SET WS-STOP TO 1
               NOT AT END
                   MOVE RDEPT-ID     TO WS-DEPT-ID(WS-INDEX)
                   MOVE RDEPT-DEP    TO WS-DEPT-DEP(WS-INDEX)
                   MOVE RDEPT-REGION TO WS-DEPT-REGION(WS-INDEX)

                   ADD 1 TO WS-INDEX
           END-PERFORM.
           
           CLOSE F-DEPT.

      *    LECTURE FICHIER CLIENT + ECRITURE EMPLOYEE DEPARTEMENT
           OPEN INPUT F-EMPLOYEE
                EXTEND F-CLISOR.

           SET WS-STOP TO 0.
           PERFORM UNTIL WS-STOP = 1
               READ F-EMPLOYEE
               AT END
                   SET WS-STOP TO 1
               NOT AT END
                   SET WS-INDEX TO 1
                   MOVE ALL SPACE TO R-CLISOR

                   MOVE REMPLOYEE-SALAIRE TO WS-SALAIRE-TEMP
                   ADD WS-SALAIRE-TEMP TO WS-SALAIRE-TOTAL

                   MOVE REMPLOYEE-ID TO RCLISOR-ID
                   MOVE REMPLOYEE-NOM TO RCLISOR-NOM
                   MOVE REMPLOYEE-PRENOM TO RCLISOR-PRENOM
                   MOVE REMPLOYEE-POSTE TO RCLISOR-POSTE
                   MOVE REMPLOYEE-SALAIRE TO RCLISOR-SALAIRE-V
                   MOVE REMPLOYEE-AGENCE TO RCLISOR-AGENCE

                   PERFORM 101 TIMES
                   IF RCLISOR-AGENCE = WS-DEPT-ID(WS-INDEX)
                      MOVE WS-DEPT-DEP(WS-INDEX) TO RCLISOR-DEPART
                      MOVE WS-DEPT-REGION(WS-INDEX) TO RCLISOR-REGION
                   END-IF
                   ADD 1 TO WS-INDEX
                   END-PERFORM

                   WRITE R-CLISOR
           END-PERFORM.

           CLOSE F-CLISOR.
           CLOSE F-EMPLOYEE.

           OPEN EXTEND F-CLISOR.

           MOVE WS-SALAIRE-TOTAL TO WS-SALAIRE-CLEAN.
           
           STRING "TOTAL DES SALAIRES :" DELIMITED BY SIZE, SPACE,
           WS-SALAIRE-CLEAN DELIMITED BY SIZE, SPACE, "€" 
           DELIMITED BY SIZE INTO WS-STRING.
           
           WRITE R-CLISOR FROM WS-STRING.

           CLOSE F-CLISOR.

           STOP RUN.
