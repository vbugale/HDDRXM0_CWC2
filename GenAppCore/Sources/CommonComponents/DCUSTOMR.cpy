      ******************************************************************
      * DCLGEN TABLE(GENAPPDB.CUSTOMER)                                *
      *        LIBRARY(PFHPWB0.GENAPP.V5R1M01.SOURCE(DCUSTOMR))        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GENAPPDB_CUSTOMER)                            *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE GENAPPDB.CUSTOMER TABLE
           ( CUSTOMERNUMBER                 INTEGER NOT NULL,
             FIRSTNAME                      CHAR(10),
             LASTNAME                       CHAR(20),
             DATEOFBIRTH                    DATE,
             HOUSENAME                      CHAR(20),
             HOUSENUMBER                    CHAR(4),
             POSTCODE                       CHAR(8),
             PHONEHOME                      CHAR(20),
             PHONEMOBILE                    CHAR(20),
             EMAILADDRESS                   CHAR(100),
             NATIONALIDNBR                  CHAR(20) NOT NULL,
             NINTYPE                        CHAR(2) NOT NULL,
             STREETADDRESS                  CHAR(30) NOT NULL,
             CITY                           CHAR(20) NOT NULL,
             STATE                          CHAR(2) NOT NULL,
             COUNTRYCODE                    CHAR(3) NOT NULL,
             SALESTERRITORY                 CHAR(5) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE GENAPPDB.CUSTOMER                  *
      ******************************************************************
       01  GENAPPDB_CUSTOMER.
           10 CUSTOMERNUMBER       PIC S9(9) USAGE COMP.
           10 FIRSTNAME            PIC X(10).
           10 LASTNAME             PIC X(20).
           10 DATEOFBIRTH          PIC X(10).
           10 HOUSENAME            PIC X(20).
           10 HOUSENUMBER          PIC X(4).
           10 POSTCODE             PIC X(8).
           10 PHONEHOME            PIC X(20).
           10 PHONEMOBILE          PIC X(20).
           10 EMAILADDRESS         PIC X(100).
           10 NATIONALIDNBR        PIC X(20).
           10 NINTYPE              PIC X(2).
           10 STREETADDRESS        PIC X(30).
           10 CITY                 PIC X(20).
           10 STATE                PIC X(2).
           10 COUNTRYCODE          PIC X(3).
           10 SALESTERRITORY       PIC X(5).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************
