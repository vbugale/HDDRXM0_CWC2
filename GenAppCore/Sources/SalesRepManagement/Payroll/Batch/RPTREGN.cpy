********
********  REGION REPORT RECORD LAYOUTS.
********
       01  REGION-HDR1.
           05  FILLER      PIC X      VALUE SPACES.
           05  FILLER      PIC X(10)  VALUE 'RUN DATE  '.
           05  REG-RUN-MONTH
                           PIC 99.
           05  FILLER      PIC X      VALUE '/'.
           05  REG-RUN-DAY PIC 99.
           05  FILLER      PIC X      VALUE '/'.
           05  REG-RUN-YEAR
                           PIC 99.
           05  FILLER      PIC X(11)  VALUE SPACES.
           05  FILLER      PIC X(21)  VALUE  'REGIONAL SALES REPORT'.
           05  FILLER      PIC X(21)  VALUE SPACES.
           05  FILLER      PIC X(5)   VALUE 'PAGE '.
           05  REG-PAGE    PIC ZZ9.
       01  REGION-HDR2.
           05  FILLER      PIC X      VALUE SPACES.
           05  FILLER      PIC X(7)   VALUE 'MANAGER'.
           05  FILLER      PIC X(9)   VALUE SPACES.
           05  FILLER      PIC X(6)   VALUE 'REGION'.
           05  FILLER      PIC X(3)   VALUE SPACES.
           05  FILLER      PIC X(11)  VALUE 'TOTAL SALES'.
           05  FILLER      PIC X(5)   VALUE SPACES.
           05  FILLER      PIC X(6)   VALUE 'SALARY'.
           05  FILLER      PIC X(5)   VALUE SPACES.
           05  FILLER      PIC X(10)  VALUE 'COMMISSION'.
           05  FILLER      PIC X(3)   VALUE SPACES.
           05  FILLER      PIC X(5)   VALUE 'TOTAL'.
           05  FILLER      PIC X(9)   VALUE SPACES.
       01  REGION-DETAIL.
           05  FILLER             PIC X         VALUE SPACES.
           05  REG-DTL-MANAGER    PIC X(15).
           05  FILLER             PIC X         VALUE SPACES.
           05  REG-DTL-REGION     PIC X(5).
           05  FILLER             PIC X(5)      VALUE SPACES.
           05  REG-DTL-SALES      PIC ZZZZZ9.99.
           05  FILLER             PIC X(5)      VALUE SPACES.
           05  REG-DTL-SALARY     PIC ZZZ9.99.
           05  FILLER             PIC X(5)      VALUE SPACES.
           05  REG-DTL-COMM       PIC ZZZZ9.99.
           05  FILLER             PIC X(3)      VALUE SPACES.
           05  REG-DTL-TOTAL      PIC ZZZZ9.99.
           05  FILLER             PIC X         VALUE SPACES.
           05  REG-DTL-COMMENT    PIC X(5).
       01  MGMT-TOTAL-DTL.
           05  FILLER             PIC X(4)      VALUE SPACES.
           05  FILLER             PIC X(5)      VALUE 'TOTAL'.
           05  FILLER             PIC X(53)     VALUE SPACES.
           05  MGMT-GRAND-TOTAL   PIC ZZZZZZ9.99.
           05  FILLER             PIC X(8)      VALUE SPACES.