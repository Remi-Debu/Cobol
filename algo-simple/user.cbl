       IDENTIFICATION DIVISION.
       PROGRAM-ID. user.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-USER.
           05 WS-ID                   PIC X(5).
           05 WS-CITY                 PIC A(20).
           05 WS-STREET               PIC X(20).
           05 WS-GSM-NUMBER           PIC 9(10).
           05 WS-LASTNAME             PIC X(20).
           05 WS-FIRSTNAME            PIC X(20).

       01  WS-PET.
           05 WS-ID.
               10 WS-ID-KEY           PIC A(3).
               10 WS-ID-NUM           PIC 9(5).
           05 WS-GENDER               PIC A(1).
           05 WS-NAME                 PIC X(10).
           05 WS-LOOF                 PIC A(3).
           05 WS-AGE                  PIC 9(3).
           05 WS-MASTER.
               10 WS-MASTER-ID        PIC X(5).
               10 WS-MASTER-LASTNAME  PIC X(20).
               10 WS-MASTER-FIRSTNAME PIC X(20).
