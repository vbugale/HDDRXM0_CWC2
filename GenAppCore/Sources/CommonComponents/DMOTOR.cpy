      ******************************************************************
      * DCLGEN TABLE(GENAPPDB.MOTOR)                                   *
      *        LIBRARY(PFHPWB0.GENAPP.V5R1M01.SOURCE(DMOTOR))          *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GENAPPDB_MOTOR)                               *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE GENAPPDB.MOTOR TABLE
           ( POLICYNUMBER                   INTEGER NOT NULL,
             MAKE                           CHAR(15),
             MODEL                          CHAR(15),
             VALUE                          INTEGER,
             REGNUMBER                      CHAR(7),
             COLOUR                         CHAR(8),
             CC                             SMALLINT,
             YEAROFMANUFACTURE              DATE,
             PREMIUM                        INTEGER,
             ACCIDENTS                      INTEGER,
             CARYEAR                        CHAR(4) NOT NULL,
             CARVIN                         CHAR(20) NOT NULL,
             AUTOPAY                        CHAR(1) NOT NULL,
             CHECKACCTNBR                   CHAR(12) NOT NULL,
             BANKROUTECODE                  CHAR(9) NOT NULL,
             CREDITCARDTYPE                 CHAR(8) NOT NULL,
             CREDITCARDNBR                  CHAR(16) NOT NULL,
             CREDITCARDPIN                  CHAR(4) NOT NULL,
             CREDITVALIDTHRU                CHAR(5) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GENAPPDB.MOTOR                     *
      ******************************************************************
       01  GENAPPDB_MOTOR.
           10 POLICYNUMBER         PIC S9(9) USAGE COMP.
           10 MAKE                 PIC X(15).
           10 MODEL                PIC X(15).
           10 VALUE1               PIC S9(9) USAGE COMP.
           10 REGNUMBER            PIC X(7).
           10 COLOUR               PIC X(8).
           10 CC                   PIC S9(4) USAGE COMP.
           10 YEAROFMANUFACTURE    PIC X(10).
           10 PREMIUM              PIC S9(9) USAGE COMP.
           10 ACCIDENTS            PIC S9(9) USAGE COMP.
           10 CARYEAR              PIC X(4).
           10 CARVIN               PIC X(20).
           10 AUTOPAY              PIC X(1).
           10 CHECKACCTNBR         PIC X(12).
           10 BANKROUTECODE        PIC X(9).
           10 CREDITCARDTYPE       PIC X(8).
           10 CREDITCARDNBR        PIC X(16).
           10 CREDITCARDPIN        PIC X(4).
           10 CREDITVALIDTHRU      PIC X(5).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
