       01 TRAIN-PLANNING.
           03 RECORD-TYPE       PIC XXX.
              88 TGV     VALUE 'TGV'.
              88 CORAIL  VALUE 'COR'.
              88 TER     VALUE 'TER'.

           03 STATION-DEPART    PIC X(18).

           03 TRAIN-TIME.
              05 TRAIN-TIME-HH  PIC 99.
              05 TRAIN-TIME-MM  PIC 99.

           03 TRAIN-TIME-END.
              05 TRAIN-TIME-HH  PIC 99.
              05 TRAIN-TIME-MM  PIC 99.

           03 TRAIN-NBRE-HEURES PIC 99.
           03 TRAIN-NBRE-HALT   PIC 99.

           03 TRAIN-HALT-FLAG   PIC X OCCURS 10 TIMES.
              88 TRAIN-STOPS-HERE VALUE 'H'.
              88 TRAIN-SERVICE    VALUE 'S'.
              88 TRAIN-FRETE      VALUE 'F'. 
              