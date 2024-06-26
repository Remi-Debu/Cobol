       IDENTIFICATION DIVISION.
       PROGRAM-ID. tgvfs.
       AUTHOR.     Remi.

       ENVIRONMENT DIVISION.
    
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE1 ASSIGN TO "file1.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS CODE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  file1.
           RECORD VARYING FROM 24 TO 34 CHARACTERS
                  DEPENDING ON DATA-NAME1
           RECORDING MODE IS V.

       01  TGV.
           03 TYPE   PIC X(03).
           03 NOM    PIC X(17).
           03 HEURE  PIC 9(02).
           03 MINUTE PIC 9(02).
           03 ARRET  PIC X(10).

       WORKING-STORAGE SECTION.
       01  CODE-STATUS PIC X(02).
       01  DATA-NAME1  PIC X(02).
       