       01  TRAIN-PLANNING-DETAILS.
           03  T-CNT PIC 9(03) VALUE 1.
           03  TRAIN OCCURS 1 TO 200 TIMES
                          DEPENDING ON T-CNT
                          INDEXED BY T-IDX.
               05 T-TYPE           PIC X(03).
               05 T-STATION-DEPART PIC X(18).
      
               05 TRAIN-TIME-START.
                  07 T-START-HH PIC 99.
                  07 T-START-MM PIC 99.
      
               05 TRAIN-TIME-END.
                  07 T-END-HH PIC 99.
                  07 T-END-MM PIC 99.
      
               05 T-NBR-HOURS PIC 99.
               05 T-HALT-FLAG PIC X(10).
               05 T-NBR-HALT  PIC 99.
