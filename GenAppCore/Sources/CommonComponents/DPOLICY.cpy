      ******************************************************************
      * DCLGEN TABLE(GENAPPDB.POLICY)                                  *
      *        LIBRARY(PFHPWB0.GENAPP.V5R1M01.SOURCE(DPOLICY))         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GENAPPDB_POLICY)                              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE GENAPPDB.POLICY TABLE
           ( POLICYNUMBER                   INTEGER NOT NULL,
             CUSTOMERNUMBER                 INTEGER NOT NULL,
             ISSUEDATE                      DATE,
             EXPIRYDATE                     DATE,
             POLICYTYPE                     CHAR(1),
             LASTCHANGED                    TIMESTAMP NOT NULL,
             BROKERID                       INTEGER,
             BROKERSREFERENCE               CHAR(10),
             PAYMENT                        INTEGER,
             COMMISSION                     SMALLINT
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GENAPPDB.POLICY                    *
      ******************************************************************
       01  GENAPPDB_POLICY.
           10 POLICYNUMBER         PIC S9(9) USAGE COMP.
           10 CUSTOMERNUMBER       PIC S9(9) USAGE COMP.
           10 ISSUEDATE            PIC X(10).
           10 EXPIRYDATE           PIC X(10).
           10 POLICYTYPE           PIC X(1).
           10 LASTCHANGED          PIC X(26).
           10 BROKERID             PIC S9(9) USAGE COMP.
           10 BROKERSREFERENCE     PIC X(10).
           10 PAYMENT              PIC S9(9) USAGE COMP.
           10 COMMISSION           PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
