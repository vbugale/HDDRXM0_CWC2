********
********  EMPLOYEE REPORT RECORD LAYOUTS.
********
       01  EMPLOYEE-HDR1.
           05  FILLER      PIC X         VALUE SPACES.
           05  FILLER      PIC X(10)
                              VALUE  'RUN DATE  '.
           05  EMP-RUN-MM
                           PIC 99.
           05  FILLER      PIC X         VALUE '/'.
           05  EMP-RUN-DD
                           PIC 99.
           05  FILLER      PIC X         VALUE '/'.
           05  EMP-RUN-YY
                           PIC 99.
           05  FILLER      PIC X(7)      VALUE SPACES.
           05  FILLER      PIC X(28)
                              VALUE  'EMPLOYEE COMPENSATION REPORT'.
           05  FILLER      PIC X(18)     VALUE SPACES.
           05  FILLER      PIC X(5)      VALUE 'PAGE '.
           05  EMP-PAGE    PIC ZZ9.
       01  EMPLOYEE-HDR2.
           05  FILLER      PIC X(31)     VALUE SPACES.
           05  FILLER      PIC X(4)      VALUE 'HIRE'.
           05  FILLER      PIC X(2)      VALUE SPACES.
           05  FILLER      PIC X(6)      VALUE 'YRS OF'.
           05  FILLER      PIC X(37)     VALUE SPACES.
       01  EMPLOYEE-HDR3.
           05  FILLER      PIC X         VALUE SPACES.
           05  FILLER      PIC X(13)     VALUE 'EMPLOYEE NAME'.
           05  FILLER      PIC X(3)      VALUE SPACES.
           05  FILLER      PIC X(6)      VALUE 'REGION'.
           05  FILLER      PIC X         VALUE SPACES.
           05  FILLER      PIC X(4)      VALUE 'TYPE'.
           05  FILLER      PIC X(3)      VALUE SPACES.
           05  FILLER      PIC X(4)      VALUE 'DATE'.
           05  FILLER      PIC X(2)      VALUE SPACES.
           05  FILLER      PIC X(7)      VALUE 'SERVICE'.
           05  FILLER      PIC X(2)      VALUE SPACES.
           05  FILLER      PIC X(6)      VALUE 'SALARY'.
           05  FILLER      PIC X(3)      VALUE SPACES.
           05  FILLER      PIC X(3)      VALUE 'O/T'.
           05  FILLER      PIC X(3)      VALUE SPACES.
           05  FILLER      PIC X(10)     VALUE 'COMMISSION'.
           05  FILLER      PIC X(4)      VALUE SPACES.
           05  FILLER      PIC X(5)      VALUE 'TOTAL'.
       01  EMPLOYEE-DTL.
           05  FILLER                  PIC X         VALUE SPACES.
           05  EMP-DTL-NAME            PIC X(15).
           05  FILLER                  PIC X         VALUE SPACES.
           05  EMP-DTL-REGION          PIC X(5).
           05  FILLER                  PIC X(3)      VALUE SPACES.
           05  EMP-DTL-TYPE            PIC X.
           05  FILLER                  PIC X(3)      VALUE SPACES.
           05  EMP-DTL-HIRE-MM         PIC 9(2).
           05  EMP-DTL-SLASH1          PIC X         VALUE SPACES.
           05  EMP-DTL-HIRE-DD         PIC 9(2).
           05  EMP-DTL-SLASH2          PIC X         VALUE SPACES.
           05  EMP-DTL-HIRE-YY         PIC 9(2).
           05  FILLER                  PIC X(3)      VALUE SPACES.
           05  EMP-DTL-YRS-OF-SERVICE  PIC 9(2).
           05  FILLER                  PIC X(2)      VALUE SPACES.
           05  EMP-DTL-WAGES           PIC ZZZZ9.99.
           05  FILLER                  PIC X         VALUE SPACES.
           05  EMP-DTL-OT              PIC ZZZZ9.99.
           05  FILLER                  PIC X(2)      VALUE SPACES.
           05  EMP-DTL-COMM            PIC ZZZZ9.99.
           05  FILLER                  PIC X         VALUE SPACES.
           05  EMP-DTL-TOTAL           PIC ZZZZ9.99.
       01  EMP-TOTAL-DTL.
           05  FILLER            PIC X(4)      VALUE SPACES.
           05  FILLER            PIC X(5)      VALUE 'TOTAL'.
           05  FILLER            PIC X(61)     VALUE SPACES.
           05  EMP-GRAND-TOTAL   PIC ZZZZZZ9.99.
