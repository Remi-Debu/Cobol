       IDENTIFICATION DIVISION.
       PROGRAM-ID.             TESTFUNC.
      *AUTHOR.                 ARNOLD J. TREMBLEY.
      *DATE-WRITTEN.           2020-12-23.
      *DATE-REVISED.           2022-01-11.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  800-MODULE-ID                   PIC X(24)   VALUE "UNKNOWN".
       01  800-MODULE-FORMATTED-DATE       PIC X(24)   VALUE "UNKNOWN".
       01  800-MODULE-SOURCE               PIC X(24)   VALUE "UNKNOWN".
       01  800-MODULE-PATH                 PIC X(64)   VALUE "UNKNOWN".
       01  800-WIN-USERNAME                PIC X(24)   VALUE "UNKNOWN".
       01  800-WIN-USERPROFILE             PIC X(24)   VALUE "UNKNOWN".
       01  800-WIN-USERDOMAIN              PIC X(24)   VALUE "UNKNOWN".
       01  800-WHEN-COMPILED.
           05  800-COMPILED-DATE-YYYY      PIC X(04)   VALUE SPACES.
           05  800-COMPILED-DATE-MM        PIC X(02)   VALUE SPACES.
           05  800-COMPILED-DATE-DD        PIC X(02)   VALUE SPACES.
           05  800-COMPILED-TIME-HH        PIC X(02)   VALUE SPACES.
           05  800-COMPILED-TIME-MM        PIC X(02)   VALUE SPACES.
           05  800-COMPILED-TIME-SS        PIC X(02)   VALUE SPACES.
           05  FILLER                      PIC X(07)   VALUE SPACES.
       01  800-CURRENT-DATE.
           05  800-CURRENT-DATE-YYYY       PIC X(04)   VALUE SPACES.
           05  800-CURRENT-DATE-MM         PIC X(02)   VALUE SPACES.
           05  800-CURRENT-DATE-DD         PIC X(02)   VALUE SPACES.
           05  800-CURRENT-TIME-HH         PIC X(02)   VALUE SPACES.
           05  800-CURRENT-TIME-MM         PIC X(02)   VALUE SPACES.
           05  800-CURRENT-TIME-SS         PIC X(02)   VALUE SPACES.
           05  FILLER                      PIC X(07)   VALUE SPACES.

       PROCEDURE DIVISION.
       MAINLINE.
           DISPLAY 'TESTFUNC Start - GnuCOBOL 3.2 26Dec2022'
           MOVE FUNCTION WHEN-COMPILED TO 800-WHEN-COMPILED
           MOVE FUNCTION CURRENT-DATE  TO 800-CURRENT-DATE
           DISPLAY 'TESTFUNC Compiled = '
               800-compiled-date-yyyy '/'
               800-compiled-date-mm   '/'
               800-compiled-date-dd   space
               800-compiled-time-hh   ':'
               800-compiled-time-mm   ':'
               800-compiled-time-ss
           DISPLAY 'TESTFUNC Executed = '
               800-current-date-yyyy '/'
               800-current-date-mm   '/'
               800-current-date-dd   space
               800-current-time-hh   ':'
               800-current-time-mm   ':'
               800-current-time-ss
           DISPLAY SPACE
           MOVE FUNCTION MODULE-ID     TO 800-MODULE-ID
           MOVE FUNCTION MODULE-FORMATTED-DATE
                                       TO 800-MODULE-FORMATTED-DATE
           MOVE FUNCTION MODULE-SOURCE TO 800-MODULE-SOURCE
           MOVE FUNCTION MODULE-PATH   TO 800-MODULE-PATH
           DISPLAY 'Module-ID         = ' 800-MODULE-ID
           DISPLAY 'Module Compiled   = ' 800-MODULE-FORMATTED-DATE
           DISPLAY 'Module-Source     = ' 800-MODULE-SOURCE
           DISPLAY 'Module-Path       = ' 800-MODULE-PATH
           ACCEPT 800-WIN-USERNAME FROM ENVIRONMENT "USERNAME"
           ACCEPT 800-WIN-USERDOMAIN FROM ENVIRONMENT "USERDOMAIN"
           ACCEPT 800-WIN-USERPROFILE FROM ENVIRONMENT "USERPROFILE"
           DISPLAY "Win username      = " 800-WIN-USERNAME
           DISPLAY "Win userprofile   = " 800-WIN-USERPROFILE
           DISPLAY "Win userdomain    = " 800-WIN-USERDOMAIN
           DISPLAY SPACE
           DISPLAY 'TESTFUNC Successfully Completed'
           STOP RUN.
