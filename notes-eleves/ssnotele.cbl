      ******************************************************************
      *    Le programme lis le fichier "input.dat" pour ensuite        *
      *    écrire dans un fichier "output.dat" un rapport sur les      *
      *    notes des élèves.                                           *
      *    Le rapport contient les notes et les moyennes en fonction   *
      *    des coefficients des matières de chaque élève et de la      *
      *    classe, ainsi que les moyennes générales.                   *
      *    À la fin du rapport on retrouve un résumé des données       *
      *    nombre d'élèves, de matières et de notes.                   *
      *                                                                *
      *    SOUS-PROGRAMME :                                            *
      *    - rnotele => Lecture / Trie                                 *
      *    - wnotele => Écriture                                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ssnotele.
       AUTHOR         Rémi.

      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TABLE-STUDENT.
           03  S-CNT  PIC 9(03) VALUE 1.
           03  STUDENT OCCURS 1 TO 200 TIMES
                          DEPENDING ON S-CNT
                          INDEXED BY S-IDX.
               05 S-LASTNAME       PIC X(10).
               05 S-FIRSTNAME      PIC X(10).
               05 S-AGE            PIC 9(02).
               05 S-SUM-GRADE-COEF PIC 9(05)V9(02).
               05 S-AV-GRADE       PIC 9(02)V9(02).
       
       01  TABLE-COURSE.
           03 C-CNT  PIC 9(03) VALUE 1.
           03 COURSE OCCURS 1 TO 200 TIMES
                        DEPENDING ON C-CNT
                        INDEXED BY C-IDX.
               05 C-ID        PIC X(10).
               05 C-ID-NAME   PIC X(04).
               05 C-LABEL     PIC X(21).
               05 C-COEF      PIC 9V9.
               05 C-SUM-GRADE PIC 9(05)V9(02).
               05 C-AV-GRADE  PIC 9(02)V9(02).

       01  TABLE-GRADE.
           03 G-CNT PIC 9(03) VALUE 1.
           03 GRADE OCCURS 1 TO 200 TIMES
                       DEPENDING ON G-CNT
                       INDEXED BY G-IDX.
               05 G-S-FULLNAME PIC X(20).
               05 G-C-LABEL    PIC X(25).
               05 G-COEF       PIC 9V9.
               05 G-GRADE      PIC 9(02)V9(02).

      ****************************************************************** 
       PROCEDURE DIVISION.
           CALL "rnotele"
               USING BY REFERENCE 
               TABLE-STUDENT, TABLE-COURSE, TABLE-GRADE
           END-CALL.

           CALL "wnotele"
               USING BY REFERENCE 
               TABLE-STUDENT, TABLE-COURSE, TABLE-GRADE
           END-CALL.

           STOP RUN.
           