      ******************************************************************
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. programID.
       AUTHOR         Rémi.

      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-NAME ASSIGN TO "filename.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-INPUT.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  F-NAME
           RECORD CONTAINS 100 CHARACTERS
           RECORDING MODE IS F.
       01  R-NAME PIC X(100).

       WORKING-STORAGE SECTION.
       01  FS-INPUT PIC X(02).
           88 FS-DEPT-OK VALUE "00".
           88 FS-DEPT-EOF VALUE "10".

      ****************************************************************** 
       PROCEDURE DIVISION.
           GOBACK.
           