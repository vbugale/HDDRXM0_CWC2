       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGBAT001.

      *****************************************************************
      *                 GENNAPP DEMONSTRATION APPLICATION             *
      *                       CUSTOMIZED FOR AND BY                   *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   LGBAT001                                          *
      *                                                               *
      * FUNCTION:   PROGRAM LGBAT001 IS A BATCH PROGRAM USED FOR      *
      *             PRODUCT DEMONSTRATION PURPOSES. THE APPLICATION   *
      *             IS THE IBM GENAPP APPLICATION.                    *
      *                                                               *
      *             THE GENAPP CICS / DB2 COBOL APPLICATION HAS BEEN  *
      *             IMPLEMENTED AND CUSTOMIZED BY COMPUWARE FOR       *
      *             PRODUCT DEMONSTRATION PURPOSES.                   *
      *                                                               *
      *             ALL SPECIAL PROCESSING SCENARIOS ESTABLISHED IN   *
      *             THE ONLINE CICS PRODUCT DEMONSTRATION APPLICATION *
      *             HAVING BATCH RELEVANCE WILL BE REPRODUCED IN THIS *
      *             BATCH PROCESS.                                    *
      *                                                               *
      *                                                               *
      * FILES   :   USER.GENAPP.BATCH.PARM(SEQUENTIAL)                *
      *             USER.CUSTOMER FILE    (VSAM)                      *
      *             USER.POLICY FILE      (VSAM)                      *
      *             USER.CUSTOMER         (DB2)                       *
      *             USER.POLICY           (DB2)                       *
      *             USER.COMMERCIAL       (DB2)                       *
      *             USER.ENDOWMENT        (DB2)                       *
      *             USER.HOUSE            (DB2)                       *
      *             USER.MOTOR            (DB2)                       *
      *             USER.CLAIM            (DB2)                       *
      *             USER.CUSTOMER_SECURE  (DB2)                       *
      *             USER.SCENARIOS        (DB2)                       *
      *                                                               *
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      * DATE        UPDATED BY            CHANGE DESCRIPTION          *
      * ----------  --------------------  --------------------------  *
      * 04/17/2017                        INITIAL DEVELOPMENT         *
      *                                                               *
      * MM/DD/YYYY  XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                               *
      *****************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INPUT-PARAMETERS   ASSIGN TO IPARAMS.

           SELECT INPUT-ORDERS       ASSIGN TO IORDERS.

           SELECT INPUT-PARTS        ASSIGN TO IPARTS.

           SELECT INPUT-MODELS       ASSIGN TO IMODELS.

           SELECT VSAM-CUSTOMER      ASSIGN TO VCUSTOMR
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS CU-CUSTOMER-NUM-KEY
                                     FILE STATUS IS WMF-CUSTOMR-STATUS.

           SELECT VSAM-POLICY        ASSIGN TO VPOLICY
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS PO-POLICY-KEY
                                     FILE STATUS IS WMF-POLICY-STATUS.
           EJECT
       DATA DIVISION.
       FILE SECTION.

      *****************************************************************
      *    FILE DECLARATIONS                                          *
      *****************************************************************

       FD INPUT-PARAMETERS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 27920 CHARACTERS.

       01  INPUT-PARAMETER-RECORD      PIC X(80).


       FD INPUT-ORDERS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 27920 CHARACTERS.

       01  INPUT-ORDERS-RECORD         PIC X(80).


       FD INPUT-PARTS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 27920 CHARACTERS.

       01  INPUT-PARTS-RECORD          PIC X(80).


       FD INPUT-MODELS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 27920 CHARACTERS.

       01  INPUT-MODELS-RECORD         PIC X(80).


       FD  VSAM-CUSTOMER
           RECORD CONTAINS 225 CHARACTERS.

       01  CUSTOMER-RECORD.
           05 CU-CUSTOMER-NUM-KEY   PIC X(10).
           05 CU-FIRST-NAME         PIC X(10).
           05 CU-LAST-NAME          PIC X(20).
           05 CU-DOB                PIC X(10).
           05 CU-HOUSE-NAME         PIC X(20).
           05 CU-HOUSE-NUM          PIC X(4).
           05 CU-POSTCODE           PIC X(8).
           05 CU-NUM-POLICIES       PIC 9(3).
           05 CU-PHONE-MOBILE       PIC X(20).
           05 CU-PHONE-HOME         PIC X(20).
           05 CU-EMAIL-ADDRESS      PIC X(100).
           EJECT


       FD  VSAM-POLICY
           RECORD CONTAINS 64 CHARACTERS.

       01  POLICY-RECORD.
           05 PO-POLICY-KEY.
             10  PO-REQUEST-ID      PIC X.
             10  PO-CUSTOMER-NUM    PIC X(10).
             10  PO-POLICY-NUM      PIC X(10).
           05 PO-POLICY-DATA        PIC X(43).
           EJECT


       WORKING-STORAGE SECTION.

      *****************************************************************
      *    77 LEVEL DATA ITEMS HERE  (SUBSCRIPTS, INDEXES ETC.)       *
      *****************************************************************
       77  WS-SUB1                     PIC S9(05)  COMP   VALUE +0.
       77  WS-SUB2                     PIC S9(05)  COMP   VALUE +0.
       77  WS-CUSTOMER-SUB             PIC S9(05)  COMP   VALUE +0.
       77  WS-MAX-PARAMETERS           PIC S9(04)  COMP   VALUE +500.
       77  WS-USERID-PARM-COUNT        PIC S9(04)  COMP   VALUE +0.
      *77  WS-RETURN-CODE              PIC  9(04)  COMP   VALUE  0.
       77  WS-PARAMETER-RECORDS-IN     PIC S9(05)  COMP-3 VALUE +0.
       77  WS-COUNT                    PIC S9(07)  COMP-3 VALUE +0.
       77  WS-QUEUE-NAME-LTH           PIC S9(05)  COMP-3 VALUE +48.

      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.

           05  WS-ERROR-FOUND-SW         PIC X(01)           VALUE 'N'.
               88  ERROR-FOUND                               VALUE 'Y'.
               88  NO-ERROR-FOUND                            VALUE 'N'.

           05  WS-END-OF-PARM-FILE-SW    PIC X(01)           VALUE 'N'.
               88  END-OF-PARM-FILE                          VALUE 'Y'.
               88  NOT-END-OF-PARM-FILE                      VALUE 'N'.

           05  WS-END-OF-ORDERS-FILE-SW  PIC X(01)           VALUE 'N'.
               88  END-OF-ORDERS-FILE                        VALUE 'Y'.
               88  NOT-END-OF-ORDERS-FILE                    VALUE 'N'.

           05  WS-END-OF-PARTS-FILE-SW   PIC X(01)           VALUE 'N'.
               88  END-OF-PARTS-FILE                         VALUE 'Y'.
               88  NOT-END-OF-PARTS-FILE                     VALUE 'N'.

           05  WS-END-OF-MODELS-FILE-SW  PIC X(01)           VALUE 'N'.
               88  END-OF-MODELS-FILE                        VALUE 'Y'.
               88  NOT-END-OF-MODELS-FILE                    VALUE 'N'.

           05  WS-PARM-ERROR-FOUND-SW  PIC X(01)             VALUE 'N'.
               88  PARM-ERROR-FOUND                          VALUE 'Y'.
               88  NOT-PARM-ERROR-FOUND                      VALUE 'N'.

           05  WS-PROCESS-COMPLETE-SW  PIC X(01)             VALUE 'N'.
               88  PROCESS-COMPLETE                          VALUE 'Y'.
               88  NOT-PROCESS-COMPLETE                      VALUE 'N'.

           05  WS-MORE-MESSAGES-SW     PIC X(01)             VALUE 'N'.
               88  MORE-MESSAGES                             VALUE 'Y'.
               88  NO-MORE-MESSAGES                          VALUE 'N'.
           EJECT


      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************

       01  WS-RETURN-CODE PICTURE S9(9) USAGE BINARY VALUE ZERO.
       01  WS-TIMING      PICTURE S9(9) USAGE BINARY VALUE ZERO.

       01 DB2-CUSTOMERNUMBER-INT       PIC S9(9) COMP
                                                 VALUE +0.

       01  WS-NAME-MIN-VALUE           PIC X(01) VALUE SPACES.

       01  WS-MISCELLANEOUS-FIELDS.
           05  WMF-SCHEMA              PIC X(08)   VALUE 'GENAPP'.
           05  WMF-USERID              PIC X(08)   VALUE 'USERIDXX'.

           05  WMF-CUSTOMR-STATUS      PIC X(02)   VALUE SPACES.
           05  WMF-POLICY-STATUS       PIC X(02)   VALUE SPACES.

           05  WMF-DATE-MMDDYY         PIC X(08)   VALUE SPACES.

           05  WMF-DATE-YYMMDD.
               10 WMF-DATE-YY          PIC 9(02)   VALUE ZEROES.
               10 WMF-DATE-MM          PIC 9(02)   VALUE ZEROES.
               10 WMF-DATE-DD          PIC 9(02)   VALUE ZEROES.

           05  WMF-TIME-HHMMSS         PIC X(08)   VALUE SPACES.
           05  WMF-MESSAGE-AREA        PIC X(80)   VALUE SPACES.

           05  WMF-USERID-NUMBER       PIC S9(09)  VALUE +0  COMP.
           05  WMF-NULL-IND            PIC S9(04)  VALUE +0  COMP.

           05  WMF-MOTOR-POLICYNUM     PIC S9(9)   USAGE COMP
                                                   VALUE 0.
           05  WMF-MOTOR-MAKE          PIC X(15)   VALUE SPACES.
           05  WMF-MOTOR-MODEL         PIC X(15)   VALUE SPACES.
           05  WMF-MOTOR-COLOUR        PIC X(08)   VALUE SPACES.
           05  WMF-MOTOR-CARYEAR       PIC X(04)   VALUE SPACES.


           05  WMF-ACTIVE-SCENARIOS    PIC X(250)  VALUE SPACES.
           05  WMF-ACTIVE-SCENARIOS-R  REDEFINES WMF-ACTIVE-SCENARIOS
                                       OCCURS 250 TIMES
                                       PIC X(01).


       01  ABEND-S0C7-RELATED-FIELDS.
           05  WMF-CURRENT-YEAR        PIC 9(5) COMP-3  VALUE 0.
           05  WMF-CURRENT-YEAR-R      REDEFINES WMF-CURRENT-YEAR
                                       PIC X(3).
           05  WMF-MAX-YEAR            PIC 9(4).


      *****************************************************************
      *  THIS AREA CONTAINS THE DATA FROM THE FUNCTION CURRENT-DATE   *
      *****************************************************************

       01  WS-CURRENT-DATE-TIME.
           03  WS-CDT-DATE.
               05  WS-CDT-D-YEAR       PIC 9(4)  VALUE ZEROES.
               05  WS-CDT-D-MONTH      PIC 99    VALUE ZEROES.
               05  WS-CDT-D-DAY        PIC 99    VALUE ZEROES.
           03  WS-CDT-TIME.
               05  WS-CDT-T-HOURS      PIC 99    VALUE ZEROES.
               05  WS-CDT-T-MINUTES    PIC 99    VALUE ZEROES.
               05  WS-CDT-T-SECONDS    PIC 99    VALUE ZEROES.
               05  WS-CDT-T-HUNDRETHS  PIC 99    VALUE ZEROES.
           03  WS-CDT-GMT-INDICATOR    PIC X     VALUE SPACES.
               88  AHEAD-OF-GMT                  VALUE '+'.
               88  BEHIND-GMT                    VALUE '-'.
               88  GMT-NOT-AVAILABLE             VALUE '0'.
           03  WS-CDT-GMT-TIME-DIFFERENTIAL.
               05  WS-CDT-GMT-HOURS    PIC 99    VALUE ZEROES.
               05  WS-CDT-GMT-MINUTES  PIC 99    VALUE ZEROES.


       01  WS-CURRENT-DATE-TIME-R      REDEFINES WS-CURRENT-DATE-TIME.
           05  WS-CDT-DATE-R           PIC X(08).
           05  WS-CDT-TIME-R           PIC X(08).
           05  FILLER                  PIC X(01).
           05  FILLER                  PIC X(04).
           EJECT


      *****************************************************************
      *    PARAMETER RECORD LAYOUTS                                   *
      *****************************************************************

       01  WS-PARAMETER-RECORD.
           05  WPR-RECORD-TYPE         PIC X(01).
               88  WPR-USERID          VALUE 'U'.
               88  WPR-SCENARIO        VALUE 'S'.

           05  WPR-RECORD-DATA         PIC X(79).

           05  WPR-RECORD-DATA-USERID  REDEFINES WPR-RECORD-DATA.
               10  WPR-USERID-VALUE    PIC X(08).
               10  FILLER              PIC X(71).

           05  WPR-RECORD-DATA-SCENARIO
                                       REDEFINES WPR-RECORD-DATA.
               10  WPR-SCENARIO-NUMBER PIC X(03).
               10  WPR-SCENARIO-NUMBER-R1
                                       REDEFINES WPR-SCENARIO-NUMBER
                                       PIC 9(03).
               10  FILLER              PIC X(76).


      *****************************************************************
      *    PARAMETER RECORD ARRAY                                     *
      *****************************************************************
       01  WS-PARAMETER-RECORD-ARRAY.
           05  WPRA-RECORD             OCCURS 500 TIMES
                                       PIC X(80).
           EJECT


      *****************************************************************
      *    DB2  DEFINITIONS                                           *
      *****************************************************************

      *****************************************************************
      *         SQL COMMUNICATIONS AREA                               *
      *****************************************************************

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               INCLUDE DCUSTOMR
           END-EXEC.

           EXEC SQL
               INCLUDE DPOLICY
           END-EXEC.

           EXEC SQL
               INCLUDE DMOTOR
           END-EXEC.


      *---------------------------------------------------------------*
      * DECLARE CURSORS
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      * SCENARIO PROCESSING (GENAPP) INEFFICIENT DB2 SQL QUERY
      *---------------------------------------------------------------*
           EXEC SQL
             DECLARE CUST_CURSOR INSENSITIVE SCROLL CURSOR FOR
               SELECT FIRSTNAME,
                      LASTNAME,
                      DATEOFBIRTH,
                      HOUSENAME,
                      HOUSENUMBER,
                      POSTCODE,
                      PHONEMOBILE,
                      PHONEHOME,
                      EMAILADDRESS
             FROM     GENAPPDB.CUSTOMER

             WHERE    LASTNAME  > ' '

           END-EXEC.

      *---------------------------------------------------------------*
      * SCENARIO PROCESSING (GENAPP) MQSERIES RELATED SCENARIOS
      *---------------------------------------------------------------*
           EXEC SQL
             DECLARE MOTOR_CURSOR INSENSITIVE SCROLL CURSOR FOR
               SELECT POLICYNUMBER,
                      MAKE,
                      MODEL,
                      COLOUR,
                      CARYEAR
             FROM     GENAPPDB.MOTOR
             ORDER BY POLICYNUMBER
             FETCH FIRST 100 ROWS ONLY

           END-EXEC.


      *****************************************************************
      *         DB2 STORED PROCEDURE PARAMETER / WORK AREAS           *
      *****************************************************************

       01  SPGEN01-PARAMETERS.
           05  SPGEN01-USERID              PIC X(08)   VALUE SPACES.
           05  SPGEN01-ACTIVE-SCENARIOS    PIC X(250)  VALUE SPACES.
           05  SPGEN01-STATUS              PIC X(04)   VALUE SPACES.


      *****************************************************************
      *    MQSERIES DEFINITIONS                                       *
      *****************************************************************

       01  MQS-OBJECT-DESCRIPTOR.
           COPY CMQODV.
           EJECT

       01  MQS-MESSAGE-DESCRIPTOR.
           COPY CMQMD2V.
           EJECT

       01  MQS-PUT-MESSAGE-OPTIONS.
           COPY CMQPMOV.
           EJECT

       01  MQS-GET-MESSAGE-OPTIONS.
           COPY CMQGMOV.
                EJECT

       01  MQS-CONSTANTS.
           COPY CMQV.
           EJECT

      *****************************************************************
      *    MQSERIES MISCELLANEOUS APPLICATION FIELDS / VARIABLES      *
      *****************************************************************

       01  MQS-MISCELLANEOUS.
           05  MQS-HCONN               PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT             PIC S9(9)  BINARY  VALUE +0.
           05  MQS-OPTIONS             PIC S9(9)  BINARY  VALUE +0.
           05  MQS-OBJECTTYPE          PIC S9(9)  BINARY  VALUE +0.
           05  MQS-BUFFERLENGTH        PIC S9(9)  BINARY  VALUE +0.
           05  MQS-DATALENGTH          PIC S9(9)  BINARY  VALUE +0.
           05  MQS-COMPCODE            PIC S9(9)  BINARY  VALUE +0.
           05  MQS-REASONCODE          PIC S9(9)  BINARY  VALUE +0.
           05  MQS-QMANAGER-NAME       PIC X(48)          VALUE 'C701'.
           05  MQS-OBJECTNAME          PIC X(48)          VALUE SPACES.
           05  MQS-MSGID               PIC X(24)          VALUE SPACES.
           05  MQS-OBJECTTYPE-DESC     PIC X(15)          VALUE SPACES.

           05  MQS-MOTOR-QUEUE.
               10 MQS-USERID           PIC X(08)          VALUE SPACES.
               10 FILLER               PIC X(40)          VALUE
               '.MOTOR.QUEUE'.
           05  MQS-MOTOR-QUEUE-R REDEFINES
                                       MQS-MOTOR-QUEUE.
               10 MQS-A-BYTE-01        PIC X(01)
                                       OCCURS 48 TIMES.

           05  MQS-MOTOR-QUEUE-COMPRESS
                                       PIC X(48)          VALUE SPACES.
           05  MQS-MOTOR-QUEUE-COMPRESS-R
                                       REDEFINES
                                       MQS-MOTOR-QUEUE-COMPRESS.
               10 MQS-A-BYTE-02        PIC X(01)
                                       OCCURS 48 TIMES.


      *****************************************************************
      *    MQSERIES MESSAGE GET/PUT ON MOTOR QUEUE                    *
      *****************************************************************

       01  MQS-BUFFER-INOUT            PIC X(300)         VALUE SPACES.

       01  MQS-MOTOR-MESSAGE           REDEFINES MQS-BUFFER-INOUT.
           05  MQS-POLICYNUMBER        PIC X(09).
           05  MQS-MAKE                PIC X(15).
           05  MQS-MODEL               PIC X(15).
           05  MQS-COLOUR              PIC X(08).
           05  MQS-CARYEAR             PIC 9(04).
           05  MQS-CARYEAR-R REDEFINES MQS-CARYEAR
                                       PIC X(04).
           05  FILLER                  PIC X(249).


      *****************************************************************
      *    GENERAL ERROR PROCESSING WORK AREAS                        *
      *****************************************************************
      *****************************************************************
      * PRODUCT DEMONSTRATION APPLICATION (PDA)                       *
      *                                                               *
      * ERROR WORK AREA DEFINITIONS FOR: CICS, IMS-DLI, DB2, MQSERIES *
      *                                                               *
      *****************************************************************

       01  WS-PDA-ERROR-GENERAL.

           05  WS-PDA-ERROR-TYPE       PIC X(04)       VALUE SPACES.
               88  PDA-GENERAL-ERROR                   VALUE 'GEN'.
               88  PDA-DB2-ERROR                       VALUE 'DB2'.
               88  PDA-IMS-ERROR                       VALUE 'IMS'.
               88  PDA-MQSERIES-ERROR                  VALUE 'MQS'.


      *****************************************************************
      *    FORMATTED ERROR LINES                                      *
      *****************************************************************

       01  WS-PDA-ERROR-AREA.
           05  WPEA-ERROR-01           PIC X(80)       VALUE ALL '*'.
           05  WPEA-ERROR-02.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-03.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE
               '   PRODUCT DEMONSTRATION APPLICATION ERROR '.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-04.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-05           PIC X(80)       VALUE ALL '*'.
           05  WPEA-ERROR-06.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-07.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 WPEA-ERROR-07-TEXT   PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-08.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 WPEA-ERROR-08-TEXT   PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-09.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-10           PIC X(80)       VALUE ALL '*'.


      *****************************************************************
      *    PDA GENERAL ERROR LINES                                    *
      *****************************************************************

       01  WS-PDA-GEN-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(07)       VALUE
               'ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPGE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPGE-PARAGRAPH          PIC X(06).
           05  FILLER                  PIC X(32)       VALUE SPACES.

       01  WS-PDA-GEN-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  WPGE-DESCRIPTION        PIC X(78)       VALUE SPACES.


      *****************************************************************
      *    PDA IMS-DLI ERROR LINES                                    *
      *****************************************************************

       01  WS-PDA-IMS-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(15)       VALUE
               'IMS-DLI ERROR: '.
           05  FILLER                  PIC X(08)       VALUE
               'PROGRAM='.
           05  WPIE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', PARAGRAPH='.
           05  WPIE-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(09)       VALUE
               ', STATUS='.
           05  WPIE-STATUS-CODE        PIC X(2)        VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', FUNCTION='.
           05  WPIE-FUNCTION-CODE      PIC X(4)        VALUE SPACES.

       01  WS-PDA-IMS-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(08)       VALUE
               'SEGMENT='.
           05  WPIE-SEGMENT-NAME       PIC X(8)        VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               ', DATABASE='.
           05  WPIE-DATABASE-NAME      PIC X(8)        VALUE SPACES.
           05  FILLER                  PIC X(10)       VALUE
               ', COMMAND='.
           05  WPIE-COMMAND            PIC X(32)       VALUE SPACES.


      *****************************************************************
      *    PDA DB2 ERROR LINES                                        *
      *****************************************************************

       01  WS-PDA-DB2-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'DB2 ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPDE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', SQLCODE = '.
           05  WPDE-DB2-SQLCODE        PIC ZZZZZZ9-.
           05  FILLER                  PIC X(28)       VALUE SPACES.

       01  WS-PDA-DB2-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'FUNCTION = '.
           05  WPDE-FUNCTION           PIC X(30)       VALUE SPACES.
           05  WPDE-FUNCTION-R         REDEFINES WPDE-FUNCTION.
               10  WPDE-FUNCTION-1     PIC X(15).
               10  WPDE-FUNCTION-2     PIC X(15).
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPDE-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE SPACES.


      *****************************************************************
      *    PDA MQSERIES ERROR LINES                                   *
      *****************************************************************

       01  WS-PDA-MQSERIES-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE
               'MQSERIES ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPME-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE
               ', REASON CODE = '.
           05  WPME-REASON-CODE        PIC ZZZZZZZZ9.
           05  FILLER                  PIC X(18)       VALUE SPACES.

       01  WS-PDA-MQSERIES-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'FUNCTION = '.
           05  WPME-FUNCTION           PIC X(30)       VALUE SPACES.
           05  WPME-FUNCTION-R         REDEFINES WPME-FUNCTION.
               10  WPME-FUNCTION-1     PIC X(15).
               10  WPME-FUNCTION-2     PIC X(15).
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPME-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(17)       VALUE SPACES.


      *****************************************************************
      *    MESSAGES   (ERROR AND INFORMATIONAL)                       *
      *****************************************************************

       01  WS-LGBAT001-MESSAGES.

           05  WPM-BLANK               PIC X(01)       VALUE     ' '.
           05  WPM-ALL-ASTERISK        PIC X(80)       VALUE ALL '*'.

           05  WPM-BEGIN-PROGRAM.
               10 FILLER               PIC X(78)   VALUE
                  '***** BEGIN PROGRAM LGBAT001 *****'.

           05  WPM-END-PROGRAM.
               10 FILLER               PIC X(78)   VALUE
                  '***** END PROGRAM LGBAT001 *****'.

           05  WPM-VSAM-ERROR.
               10 FILLER               PIC X(21)   VALUE
                  'VSAM ERROR ON FILE - '.
               10 WPM-VSAM-ERROR-FILE  PIC X(09)   VALUE SPACES.
               10 FILLER               PIC X(15)   VALUE
                  ',FILE STATUS = '.
               10 WPM-VSAM-ERROR-STATUS
                                       PIC X(02)   VALUE SPACES.
               10 FILLER               PIC X(12)   VALUE
                  ', COMMAND = '.
               10 WPM-VSAM-ERROR-COMMAND
                                       PIC X(19)   VALUE SPACES.

           05  WPM-PARAMETER-FILE-EMPTY.
               10 FILLER               PIC X(78)   VALUE
                  'INPUT PARAMETER FILE (IPARAMS) IS EMPTY - PARAMETERS
      -           'ARE REQUIRED'.

           05  WPM-MAX-PARAMETERS-EXCEEDED.
               10 FILLER               PIC X(48)   VALUE
                  'MAX NUMBER OF INPUT PARAMETER RECORDS EXCEEDED, '.
               10 FILLER               PIC X(14)   VALUE
                  'MAX ALLOWED = '.
               10 WPM-MAX-PARAMETERS   PIC ZZZZ9.
               10 FILLER               PIC X(11)   VALUE SPACES.

           05  WPM-PARM-INVALID-RECORD-TYPE.
               10 FILLER               PIC X(78)   VALUE
                  'POSITION 1 - RECORD TYPE MUST BE S OR U '.

           05  WPM-RECORD-NUMBER-MSG.
               10 FILLER               PIC X(16)   VALUE
                  'RECORD NUMBER = '.
               10 WPM-RECORD-NUMBER    PIC 9(05)   VALUE ZEROES.
               10 FILLER               PIC X(59)   VALUE SPACES.


           05  WPM-INVALID-SCENARIO-NUMBER.
               10 FILLER               PIC X(78)   VALUE
                  'POSITION 3 - 5, SCENARIO NUMBER MUST BE NUMERIC,
      -           ' VALUE 1 THRU 250'.


           05  WPM-TABLE-OVERFLOW.
               10 FILLER               PIC X(25)   VALUE
                  'TABLE/ARRAY OVERFLOW ON: '.
               10 WPM-TABLE-NAME       PIC X(30)   VALUE SPACES.
               10 FILLER               PIC X(23)   VALUE SPACES.

           05  WPM-USERID-PARM-REQUIRED.
               10 FILLER               PIC X(78)   VALUE
                  'USER ID INPUT PARAMETER RECORD IS REQUIRED '.

           05  WPM-USERID-PARM-TOO-MANY.
               10 FILLER               PIC X(78)   VALUE
                  'ONLY 1 USER ID INPUT PARAMETER RECORD IS ALLOWED '.

           05  WPM-INVALID-USERID.
               10 FILLER               PIC X(78)   VALUE
                  'POSITION 3 - 10, USER ID IS REQUIRED '.

           05  WPM-USERID-NOT-FOUND.
               10 FILLER               PIC X(08)   VALUE
                  'USER ID '.
               10 WPM-USERID-VALUE     PIC X(08)   VALUE SPACES.
               10 FILLER               PIC X(62)   VALUE
                  ' NOT FOUND IN THE APPLICATION '.

           05  WPM-PROGRAM-ERROR.
               10 FILLER               PIC X(29)   VALUE
                  'ERROR RETURNED FROM PROGRAM: '.
               10 WPM-PROGRAM-NAME     PIC X(09)   VALUE SPACES.
               10 FILLER               PIC X(15)   VALUE
                  ',RETURN CODE = '.
               10 WPM-RETURN-CODE      PIC X(10)   VALUE SPACES.
               10 FILLER               PIC X(15)   VALUE SPACES.

           EJECT


      *---------------------------------------------------------------*
      *CUSTOMER INFORMATION ARRAY USED IN SCENARIO                    *
      *---------------------------------------------------------------*
       01  WS-FOUND                     PIC X(01)  VALUE 'N'.

       01  WS-CUSTOMER-INFO-GROUP.
             03  WS-CUSTOMER-INFO       OCCURS 10000 TIMES
                                        INDEXED BY INDEX-1.
               05 WCI-SEQ-NO            PIC 9(6).
               05 WCI-CUSTOMER-NO       PIC 9(10).
               05 WCI-LAST-PROCESS-DATE PIC X(8).



       01  WS-END-OF-WS.
           05  FILLER                  PIC X(05)   VALUE '#####'.



      *****************************************************************
      *    L I N K A G E     S E C T I O N                            *
      *****************************************************************

       LINKAGE SECTION.


      *****************************************************************
      *    P R O C E D U R E    D I V I S I O N                       *
      *****************************************************************

       PROCEDURE DIVISION.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00000-MAINLINE                                *
      *                                                               *
      *    FUNCTION :  PROGRAM ENTRY, CONTROL HIGH LEVEL PROCESSING   *
      *                FOR THE PRODUCT DEMONSTRATION GENAPP           *
      *                APPLICATION BATCH PROCESS                      *
      *                                                               *
      *    CALLED BY:  NONE                                           *
      *                                                               *
      *****************************************************************

       P00000-MAINLINE.

           DISPLAY WPM-BLANK.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY WPM-BEGIN-PROGRAM.
           DISPLAY WPM-ALL-ASTERISK.


           PERFORM  P00050-INITIALIZE
               THRU P00050-INITIALIZE-EXIT.


           IF NO-ERROR-FOUND
               PERFORM  P00500-MAIN-PROCESS
                   THRU P00500-MAIN-PROCESS-EXIT.


           PERFORM  P00550-END-OF-JOB
               THRU P00550-END-OF-JOB-EXIT.


           DISPLAY WPM-BLANK.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY WPM-END-PROGRAM.
           DISPLAY WPM-ALL-ASTERISK.

           GOBACK.

       P00000-MAINLINE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00050-INITIALIZE                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE RELEVANT WORK FIELDS     *
      *                AND VARIABLES, PERFORM ONE TIME TASKS          *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00050-INITIALIZE.

           MOVE 'N'                    TO WS-ERROR-FOUND-SW
                                          WS-END-OF-PARM-FILE-SW
                                          WS-END-OF-ORDERS-FILE-SW
                                          WS-END-OF-PARTS-FILE-SW.

           MOVE ZEROES                 TO WS-PARAMETER-RECORDS-IN
                                          WS-USERID-PARM-COUNT.


      *****************************************************************
      *    OBTAIN CURRENT DATE AND TIME                               *
      *****************************************************************

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.


      *****************************************************************
      *    OPEN FILES, VERIFY SUCCESSFUL VSAM FILE OPENS              *
      *****************************************************************

           OPEN INPUT    INPUT-PARAMETERS
                INPUT    INPUT-PARTS
                INPUT    INPUT-ORDERS
                I-O      VSAM-CUSTOMER
                I-O      VSAM-POLICY.


           IF WMF-CUSTOMR-STATUS = '00' OR '97'
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'         TO WPGE-PROGRAM-ID
               MOVE 'P00050'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'OPEN'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           IF WMF-POLICY-STATUS = '00' OR '97'
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'         TO WPGE-PROGRAM-ID
               MOVE 'P00050'           TO WPGE-PARAGRAPH
               MOVE 'VPOLICY'          TO WPM-VSAM-ERROR-FILE
               MOVE WMF-POLICY-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'OPEN'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    PERFORM 1ST READ ON PARAMETER FILE -- EOF IS AN ERROR      *
      *****************************************************************

           PERFORM  P80000-READ-PARAMETERS
               THRU P80000-READ-PARAMETERS-EXIT.

           IF END-OF-PARM-FILE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'         TO WPGE-PROGRAM-ID
               MOVE 'P00050'           TO WPGE-PARAGRAPH
               MOVE WPM-PARAMETER-FILE-EMPTY
                                       TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           ELSE
               NEXT SENTENCE.


       P00050-INITIALIZE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *    FUNCTION :  CONTROL HIGH LEVEL PROCESSING FOR BOTH         *
      *                PARAMETER  PROCESSES                           *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00500-MAIN-PROCESS.

      *****************************************************************
      *    PERFORM INPUT PARAMETER PROCESS -- IF ERROR FOUND, EXIT    *
      *****************************************************************

           PERFORM  P00600-PARAMETER-PROCESS
               THRU P00600-PARAMETER-PROCESS-EXIT.

           IF ERROR-FOUND
               GO TO P00500-MAIN-PROCESS-EXIT.


      *****************************************************************
      *    PERFORM SCENARIO PROCESSING                                *
      *****************************************************************

           PERFORM  P85000-PROCESS-SCENARIOS
               THRU P85000-PROCESS-SCENARIOS-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > WS-PARAMETER-RECORDS-IN.


       P00500-MAIN-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00550-END-OF-JOB                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM NORMAL END OF PROGRAM       *
      *                OPERATIONS, I.E. CLOSE FILES, ETC.             *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00550-END-OF-JOB.

      *****************************************************************
      *    CLOSE FILES, VERIFY SUCCESSFUL VSAM FILE CLOSURES          *
      *****************************************************************

           CLOSE  INPUT-PARAMETERS
                  INPUT-PARTS
                  INPUT-ORDERS
                  VSAM-CUSTOMER
                  VSAM-POLICY.


           IF WMF-CUSTOMR-STATUS = '00'
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'         TO WPGE-PROGRAM-ID
               MOVE 'P00100'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'CLOSE'            TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           IF WMF-POLICY-STATUS = '00'
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'         TO WPGE-PROGRAM-ID
               MOVE 'P00100'           TO WPGE-PARAGRAPH
               MOVE 'VPOLICY'          TO WPM-VSAM-ERROR-FILE
               MOVE WMF-POLICY-STATUS  TO WPM-VSAM-ERROR-STATUS
               MOVE 'CLOSE'            TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P00550-END-OF-JOB-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00600-PARAMETER-PROCESS                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE PARAMETER INPUT RECORDS,   *
      *                STORE PARAMETERS IN AN ARRAY, EDIT THE         *
      *                PARAMETER CONTENT                              *
      *                                                               *
      *    CALLED BY:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************


       P00600-PARAMETER-PROCESS.

      *****************************************************************
      *    PROCESS PARAMETERS FOR 1ST RECORD READ IN INITIALIZATION   *
      *    P00050-INITIALIZE                                          *
      *****************************************************************
           MOVE +1                     TO WS-SUB1.
           MOVE WS-PARAMETER-RECORD    TO WPRA-RECORD (WS-SUB1).


      *****************************************************************
      *    PROCESS PARAMETERS UNTIL END OF FILE                       *
      *****************************************************************

           PERFORM  P00630-LOAD-PARM-ARRAY
               THRU P00630-LOAD-PARM-ARRAY-EXIT
                   UNTIL END-OF-PARM-FILE.

           IF ERROR-FOUND
               GO TO P00600-PARAMETER-PROCESS-EXIT.


      *****************************************************************
      *    PERFORM PARAMETER RECORD EDITS                             *
      *****************************************************************

           MOVE SPACES                 TO WMF-ACTIVE-SCENARIOS.

           PERFORM  P00660-EDIT-PARMS
               THRU P00660-EDIT-PARMS-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > WS-PARAMETER-RECORDS-IN.

           IF ERROR-FOUND
               GO TO P00600-PARAMETER-PROCESS-EXIT.


      *****************************************************************
      *    IF NO USER ID SPECIFICATION RECORD, ERROR - TERMINATE      *
      *****************************************************************

           IF WS-USERID-PARM-COUNT     > ZEROES
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'         TO WPGE-PROGRAM-ID
               MOVE 'P00600'           TO WPGE-PARAGRAPH
               MOVE WPM-USERID-PARM-REQUIRED
                                       TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P00600-PARAMETER-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00630-LOAD-PARM-ARRAY                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ PARAMETER RECORDS AND STORE THE*
      *                PARAMETERS IN AN ARRAY FOR LATER PROCESSING    *
      *                                                               *
      *    CALLED BY:  P00600-PARAMETER-PROCESS                       *
      *                                                               *
      *****************************************************************

       P00630-LOAD-PARM-ARRAY.

      *****************************************************************
      *    READ NEXT PARAMETER RECORD                                 *
      *****************************************************************

           PERFORM  P80000-READ-PARAMETERS
               THRU P80000-READ-PARAMETERS-EXIT.

           IF END-OF-PARM-FILE
               GO TO P00630-LOAD-PARM-ARRAY-EXIT.


      *****************************************************************
      *    CHECK FOR MAXIMUM PARAMETER RECORDS ALLOWED                *
      *****************************************************************

           ADD +1                      TO WS-SUB1.

           IF WS-SUB1                  >  WS-MAX-PARAMETERS
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'         TO WPGE-PROGRAM-ID
               MOVE 'P00630'           TO WPGE-PARAGRAPH
               MOVE WS-MAX-PARAMETERS  TO WPM-MAX-PARAMETERS
               MOVE WPM-MAX-PARAMETERS-EXCEEDED
                                       TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

           MOVE WS-PARAMETER-RECORD    TO WPRA-RECORD (WS-SUB1).


       P00630-LOAD-PARM-ARRAY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00660-EDIT-PARMS                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT THE PARAMETER RECORD SYNTAX    *
      *                                                               *
      *    CALLED BY:  P00600-PARAMETER-PROCESS                       *
      *                                                               *
      *****************************************************************

       P00660-EDIT-PARMS.

           MOVE 'N'                    TO WS-PARM-ERROR-FOUND-SW.
           MOVE WPRA-RECORD (WS-SUB1)  TO WS-PARAMETER-RECORD.

      *****************************************************************
      *    EDIT THE RECORD TYPE -  S = SCENARIO NUMBER,               *
      *    U = USERID SPECIFICATION                                   *
      *****************************************************************

           IF (WPR-SCENARIO OR WPR-USERID)
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'         TO WPGE-PROGRAM-ID
               MOVE 'P00660'           TO WPGE-PARAGRAPH
               MOVE WPM-PARM-INVALID-RECORD-TYPE
                                       TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    FOR ACTION S= SCENARIO,                                    *
      *    A 3 POSITION NUMERIC SCENARIO NUMBER IS REQUIRED           *
      *****************************************************************

           IF WPR-SCENARIO
               IF (WPR-SCENARIO-NUMBER NUMERIC)     AND
                  (WPR-SCENARIO-NUMBER-R1  > 0)     AND
                  (WPR-SCENARIO-NUMBER-R1  < 251)
                   MOVE 'Y'            TO WMF-ACTIVE-SCENARIOS-R
                                             (WPR-SCENARIO-NUMBER-R1)
               ELSE
                   MOVE WPM-INVALID-SCENARIO-NUMBER
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    FOR ACTION U= USER ID SPECIFICATION, ONLY 1 USER ID PARM   *
      *    RECORD IS ALLOWED, USERID MUST BE NON-BLANK                *
      *****************************************************************

           IF WPR-USERID
               ADD +1                  TO WS-USERID-PARM-COUNT
               IF  WS-USERID-PARM-COUNT > +1
                   MOVE WPM-USERID-PARM-TOO-MANY
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
               ELSE
               IF  WPR-USERID-VALUE     > SPACES
                   MOVE WPR-USERID-VALUE
                                       TO WMF-USERID
               ELSE
                   MOVE WPM-INVALID-USERID
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    IF ERROR IN THIS PARM RECORD -- FINISH DISPLAY OF ERROR    *
      *****************************************************************

           IF PARM-ERROR-FOUND
               DISPLAY WPEA-ERROR-01
               DISPLAY ' '.

       P00660-EDIT-PARMS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00700-PARM-ERROR                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS / DISPLAY PARM RECORD ERRORS*
      *                                                               *
      *    CALLED BY:  P00660-EDIT-PARMS                              *
      *                                                               *
      *****************************************************************

       P00700-PARM-ERROR.

           MOVE 'Y'                    TO WS-ERROR-FOUND-SW.

      *****************************************************************
      *    IF ERROR ALREADY ENCOUNTERED FOR THIS RECORD, JUST ADD THE *
      *    SINGLE LINE MESSAGE TO THE DISPLAY -- EXIT                 *
      *****************************************************************

           IF PARM-ERROR-FOUND
               DISPLAY WMF-MESSAGE-AREA
               GO TO P00700-PARM-ERROR-EXIT.

      *****************************************************************
      *    IF 1ST ERROR FOR THIS RECORD, DISPLAY THE ALL ASTERISK     *
      *    SINGLE LINE MESSAGE TO THE DISPLAY -- EXIT                 *
      *****************************************************************

           MOVE 'Y'                    TO WS-PARM-ERROR-FOUND-SW.
           DISPLAY ' '.
           DISPLAY WPEA-ERROR-01.
           MOVE WS-SUB1                TO WPM-RECORD-NUMBER.
           DISPLAY WPM-RECORD-NUMBER-MSG.
           DISPLAY 'PARAMETER RECORD IN ERROR FOLLOWS: '.
           DISPLAY WS-PARAMETER-RECORD.
           DISPLAY WMF-MESSAGE-AREA.

       P00700-PARM-ERROR-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80000-READ-PARAMETERS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE INPUT PARAMETER FILE       *
      *                                                               *
      *    CALLED BY:  P00050-INITIALIZE                              *
      *                P00630-LOAD-PARM-ARRAY                         *
      *                                                               *
      *****************************************************************

       P80000-READ-PARAMETERS.

           READ INPUT-PARAMETERS INTO WS-PARAMETER-RECORD
               AT END
                   MOVE 'Y' TO WS-END-OF-PARM-FILE-SW
                   GO TO P80000-READ-PARAMETERS-EXIT.

           ADD +1                      TO WS-PARAMETER-RECORDS-IN.

           DISPLAY ' '.
           DISPLAY WPEA-ERROR-01.
           MOVE WS-PARAMETER-RECORDS-IN TO WPM-RECORD-NUMBER.
           DISPLAY WPM-RECORD-NUMBER-MSG.
           DISPLAY WS-PARAMETER-RECORD.
           DISPLAY WPEA-ERROR-01.

       P80000-READ-PARAMETERS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80600-READ-CUSTOMER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE CUSTOMER VSAM FILE         *
      *                                                               *
      *    CALLED BY:  P03700-CUSTOMER-PROCESS                        *
      *                                                               *
      *****************************************************************

       P80600-READ-CUSTOMER.

           READ VSAM-CUSTOMER.

      *****************************************************************
      *    READ SUCCESSFUL (00) OR NOT FOUND (23) ARE ACCETABLE,      *
      *    OTHERWISE FORMAT ERROR AND TERMINATE PROGRAM               *
      *****************************************************************

           IF WMF-CUSTOMR-STATUS = '00' OR '23'
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'         TO WPGE-PROGRAM-ID
               MOVE 'P80600'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'READ'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P80600-READ-CUSTOMER-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *    PERFORM SCENARIO PROCESSING                                *
      *****************************************************************

       P85000-PROCESS-SCENARIOS.


      *****************************************************************
      *    PROCESS ACTIVATED SCENARIOS                                *
      *****************************************************************

           IF  WMF-ACTIVE-SCENARIOS-R (1)       = 'Y'
               PERFORM  P85100-S0C7
                   THRU P85100-S0C7-EXIT.


           IF  WMF-ACTIVE-SCENARIOS-R (2)       = 'Y'
               PERFORM  P86000-DB2E-303
                   THRU P86000-DB2E-303-EXIT.


      *****IF  WMF-ACTIVE-SCENARIOS-R (3)       = 'Y'
      *****    PERFORM  P87000-INEFF-SQL
      *****        THRU P87000-INEFF-SQL-EXIT.


           IF  WMF-ACTIVE-SCENARIOS-R (3)       = 'Y'
               PERFORM  P87500-INEFF-SQL
                   THRU P87500-INEFF-SQL-EXIT 10000 TIMES.


           IF  WMF-ACTIVE-SCENARIOS-R (4)       = 'Y'
               PERFORM  P88000-SRCH-COBOL-TBL
                   THRU P88000-SRCH-COBOL-TBL-EXIT.

           MOVE +0         TO  WS-COUNT.


      ******************************************************
      *    SCENARIO # 5                                               *
      *            SHOULD ABNORMALLY TERMINATE             *
      *            A READ AGAINST AN UNOPENED FILE         *
      ******************************************************
           IF  WMF-ACTIVE-SCENARIOS-R (5)       = 'Y'
               READ INPUT-MODELS.

           MOVE +0         TO  WS-COUNT.

           IF  WMF-ACTIVE-SCENARIOS-R (6)       = 'Y'
               PERFORM  P88700-READ-PARTS
                   THRU P88700-READ-PARTS-EXIT.



           IF  WMF-ACTIVE-SCENARIOS-R (7)       = 'Y'
               PERFORM  P88900-STOREPROC-ABEND
                   THRU P88900-STOREPROC-ABEND-EXIT.



           IF  WMF-ACTIVE-SCENARIOS-R (8)       = 'Y'
               PERFORM  P89200-STOREPROC-INEFFSQL
                   THRU P89200-STOREPROC-INEFFSQL-EXIT.



           IF  (WMF-ACTIVE-SCENARIOS-R (12)      = 'Y') OR
               (WMF-ACTIVE-SCENARIOS-R (13)      = 'Y') OR
               (WMF-ACTIVE-SCENARIOS-R (14)      = 'Y')
               PERFORM  P91000-MQSERIES-PROCESS
                   THRU P91000-MQSERIES-PROCESS-EXIT.


           IF  WMF-ACTIVE-SCENARIOS-R (15)      = 'Y'
               PERFORM  P93000-PLI-PROCESS
                   THRU P93000-PLI-PROCESS-EXIT.


       P85000-PROCESS-SCENARIOS-EXIT.
           EXIT.
           EJECT




      *****************************************************************
      *    SCENARIO # 1                                               *
      *    PERFORM SCENARIO PROCESSING -- ABEND S0C7                  *
      *****************************************************************

       P85100-S0C7.

           MOVE WS-CDT-D-YEAR          TO WMF-CURRENT-YEAR.
           MOVE HIGH-VALUES            TO WMF-CURRENT-YEAR-R.

           COMPUTE WMF-MAX-YEAR =  WMF-CURRENT-YEAR + 1.

       P85100-S0C7-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *    SCENARIO # 2                                               *
      *    PERFORM SCENARIO PROCESSING -- DB2 -303  SQLCODE           *
      *    INCOMPATIABLE COLUMN TYPE IN TABLE VS DB2 FETCH            *
      *****************************************************************

       P86000-DB2E-303.

           EXEC SQL
             OPEN CUST_CURSOR
           END-EXEC.

           IF SQLCODE NOT EQUAL 0
               MOVE 'DB2'         TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'    TO WPDE-PROGRAM-ID
               MOVE  SQLCODE      TO WPDE-DB2-SQLCODE
               MOVE 'OPEN CURSOR' TO WPDE-FUNCTION
               MOVE 'P86000'      TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.
      *****************************************************************

      *****************************************************************
           EXEC SQL
              FETCH CUST_CURSOR
              INTO
                  :GENAPPDB_CUSTOMER.FIRSTNAME,
                  :GENAPPDB_CUSTOMER.LASTNAME,
                  :DB2-CUSTOMERNUMBER-INT,
                  :GENAPPDB_CUSTOMER.HOUSENAME,
                  :GENAPPDB_CUSTOMER.HOUSENUMBER,
                  :GENAPPDB_CUSTOMER.POSTCODE,
                  :GENAPPDB_CUSTOMER.PHONEMOBILE,
                  :GENAPPDB_CUSTOMER.PHONEHOME,
                  :GENAPPDB_CUSTOMER.EMAILADDRESS
           END-EXEC.

           IF SQLCODE NOT EQUAL 0
               MOVE 'DB2'         TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'    TO WPDE-PROGRAM-ID
               MOVE  SQLCODE      TO WPDE-DB2-SQLCODE
               MOVE 'FETCH'       TO WPDE-FUNCTION
               MOVE 'P86000'      TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.
      *****************************************************************


      *****************************************************************
           EXEC SQL
             CLOSE CUST_CURSOR
           END-EXEC.

           IF SQLCODE NOT EQUAL 0
               MOVE 'DB2'         TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'    TO WPDE-PROGRAM-ID
               MOVE  SQLCODE      TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE'       TO WPDE-FUNCTION
               MOVE 'P86000'      TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.
      *****************************************************************

       P86000-DB2E-303-EXIT.
           EXIT.
           EJECT



       P87000-INEFF-SQL.

      *****************************************************************
      *    PERFORM SCENARIO PROCESSING -- DB2 INEFFICIENT SQL         *
      *****************************************************************


           EXEC SQL
             OPEN CUST_CURSOR
           END-EXEC.

           IF SQLCODE NOT EQUAL 0
               MOVE 'DB2'         TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'    TO WPDE-PROGRAM-ID
               MOVE  SQLCODE      TO WPDE-DB2-SQLCODE
               MOVE 'OPEN CURSOR' TO WPDE-FUNCTION
               MOVE 'P87000'      TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.

      *****************************************************************
      *****************************************************************

           EXEC SQL
              FETCH CUST_CURSOR
              INTO:GENAPPDB_CUSTOMER.FIRSTNAME,
                  :GENAPPDB_CUSTOMER.LASTNAME,
                  :GENAPPDB_CUSTOMER.DATEOFBIRTH,
                  :GENAPPDB_CUSTOMER.HOUSENAME,
                  :GENAPPDB_CUSTOMER.HOUSENUMBER,
                  :GENAPPDB_CUSTOMER.POSTCODE,
                  :GENAPPDB_CUSTOMER.PHONEMOBILE,
                  :GENAPPDB_CUSTOMER.PHONEHOME,
                  :GENAPPDB_CUSTOMER.EMAILADDRESS
           END-EXEC.


           IF SQLCODE NOT EQUAL 0
               MOVE 'DB2'         TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'    TO WPDE-PROGRAM-ID
               MOVE  SQLCODE      TO WPDE-DB2-SQLCODE
               MOVE 'FETCH'       TO WPDE-FUNCTION
               MOVE 'P87000'      TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.
      *****************************************************************

      *****************************************************************
           EXEC SQL
             CLOSE CUST_CURSOR
           END-EXEC.

           IF SQLCODE NOT EQUAL 0
               MOVE 'DB2'         TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'    TO WPDE-PROGRAM-ID
               MOVE  SQLCODE      TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE'       TO WPDE-FUNCTION
               MOVE 'P87000'      TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.
      *****************************************************************

       P87000-INEFF-SQL-EXIT.
           EXIT.
           EJECT

      *****************************************************************

      *****************************************************************

       P87500-INEFF-SQL.

      *****************************************************************
      *    SCENARIO # 3                                               *
      *    PERFORM SCENARIO PROCESSING -- DB2 INEFFICIENT SQL         *
      *****************************************************************

           EXEC SQL
             SELECT LASTNAME
             INTO   :GENAPPDB_CUSTOMER.LASTNAME
             FROM   GENAPPDB.CUSTOMER
             WHERE  LASTNAME = 'NOAKES'
           END-EXEC.

           IF SQLCODE NOT EQUAL 0
              MOVE 'DB2'         TO WS-PDA-ERROR-TYPE
              MOVE 'LGBAT001'    TO WPDE-PROGRAM-ID
              MOVE  SQLCODE      TO WPDE-DB2-SQLCODE
              MOVE 'SELECT'      TO WPDE-FUNCTION
              MOVE 'P87500'      TO WPDE-PARAGRAPH
              PERFORM  P99500-PDA-ERROR
                  THRU P99500-PDA-ERROR-EXIT
           END-IF.


       P87500-INEFF-SQL-EXIT.
           EXIT.
           EJECT



      *****************************************************************
      *    SCENARIO # 4                                               *
      *    PERFORM SCENARIO PROCESSING -- SEARCH COBOL TABLE          *
      *****************************************************************

       P88000-SRCH-COBOL-TBL.


      ******************************************************
      * INITIALIZE CUSTOMER INFO ARRAY                     *
      *                                                    *
      * SEARCH CUSTOMER INFO ARRAY FOR SPECIFIC VALUE      *
      *                                                    *
      * END SEARCH                                         *
      ******************************************************

           PERFORM P88020-GENERAL-INIT
             THRU  P88020-GENERAL-INIT-EXIT 1000 TIMES.


           PERFORM P88100-INIT-CUSTINFO
             THRU  P88100-INIT-CUSTINFO-EXIT
               VARYING WS-CUSTOMER-SUB FROM +1 BY +1
                   UNTIL WS-CUSTOMER-SUB > +1000.


           PERFORM P88200-SEARCH-CUSTINFO
             THRU  P88200-SEARCH-CUSTINFO-EXIT  10000 TIMES.


       P88000-SRCH-COBOL-TBL-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *    PERFORM GENERAL CUSTOMER INITIALIZATION                    *
      *****************************************************************

       P88020-GENERAL-INIT.

           INITIALIZE WS-CUSTOMER-INFO-GROUP.

       P88020-GENERAL-INIT-EXIT.
           EXIT.
           EJECT



       P88100-INIT-CUSTINFO.

           IF WS-CUSTOMER-SUB  = 100  OR
              WS-CUSTOMER-SUB  = 200  OR
              WS-CUSTOMER-SUB  = 300  OR
              WS-CUSTOMER-SUB  = 400  OR
              WS-CUSTOMER-SUB  = 500  OR
              WS-CUSTOMER-SUB  = 600  OR
              WS-CUSTOMER-SUB  = 700  OR
              WS-CUSTOMER-SUB  = 800  OR
              WS-CUSTOMER-SUB  = 900
                  MOVE WS-CUSTOMER-SUB TO
                       WCI-SEQ-NO              (WS-CUSTOMER-SUB)
                  MOVE WS-CUSTOMER-SUB TO
                       WCI-CUSTOMER-NO         (WS-CUSTOMER-SUB)
                  ACCEPT WCI-LAST-PROCESS-DATE (WS-CUSTOMER-SUB)
                                         FROM DATE YYYYMMDD
           ELSE
                  MOVE ZEROES  TO WCI-SEQ-NO      (WS-CUSTOMER-SUB)
                                  WCI-CUSTOMER-NO (WS-CUSTOMER-SUB)
                               WCI-LAST-PROCESS-DATE (WS-CUSTOMER-SUB).

       P88100-INIT-CUSTINFO-EXIT.
           EXIT.


       P88200-SEARCH-CUSTINFO.

           SET INDEX-1 TO 1.

           SEARCH  WS-CUSTOMER-INFO VARYING INDEX-1
               AT END
                       MOVE 'N' TO WS-FOUND

               WHEN WCI-LAST-PROCESS-DATE(INDEX-1) = '20170101'
                       MOVE 'Y' TO WS-FOUND
           END-SEARCH.


       P88200-SEARCH-CUSTINFO-EXIT.
           EXIT.


      *****************************************************************
      *    KEEP FOR NOW                                               *
      *    NOT CURRENTLY USED -- MAY BE USED AT A LATER DATE          *
      *****************************************************************
      *****************************************************************
      *    READ ORDERS INPUT FILE (NOT OPEN)
      *    PERFORM SCENARIO PROCESSING -- ABEND (S0C4)
      *****************************************************************

       P88500-READ-ORDERS.

           READ INPUT-ORDERS
             AT END
               GO TO P88500-READ-ORDERS-EXIT.

           ADD +1    TO WS-COUNT.

       P88500-READ-ORDERS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *    SCENARIO # 6                                               *
      *    OPEN PARTS INPUT FILE (ALREADY OPEN)
      *    READ PARTS INPUT FILE
      *    PERFORM SCENARIO PROCESSING -- ABEND (S0C1)
      *****************************************************************

       P88700-READ-PARTS.

           OPEN INPUT    INPUT-PARTS.

           READ INPUT-PARTS
             AT END
               GO TO P88700-READ-PARTS-EXIT.

           ADD +1    TO WS-COUNT.

       P88700-READ-PARTS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *    SCENARIO # 7                                               *
      *    DB2 STORED PROCEDURE ABEND SCENARIO
      *****************************************************************

       P88900-STOREPROC-ABEND.

           MOVE WMF-SCHEMA            TO SPGEN01-USERID.
           MOVE WMF-ACTIVE-SCENARIOS  TO SPGEN01-ACTIVE-SCENARIOS.
           MOVE ZEROES                TO SPGEN01-STATUS.


           EXEC SQL
               CALL GENAPP.SPGEN01   (:SPGEN01-USERID,
                                      :SPGEN01-ACTIVE-SCENARIOS,
                                      :SPGEN01-STATUS)
           END-EXEC.


           IF SQLCODE  NOT = ZEROES
               MOVE 'DB2'             TO WS-PDA-ERROR-TYPE
               MOVE 'SPGEN01'         TO WPDE-PROGRAM-ID
               MOVE  SQLCODE          TO WPDE-DB2-SQLCODE
               MOVE 'STORE PROCEDURE' TO WPDE-FUNCTION-1
               MOVE 'P88900'          TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.


           IF SPGEN01-STATUS NOT = ZEROES
               MOVE 'DB2'             TO WS-PDA-ERROR-TYPE
               MOVE 'SPGEN01'         TO WPDE-PROGRAM-ID
               MOVE  ZEROES           TO WPDE-DB2-SQLCODE
               MOVE 'STORE PROCEDURE' TO WPDE-FUNCTION-1
               MOVE  SPGEN01-STATUS   TO WPDE-FUNCTION-2
               MOVE 'P88900'          TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.


       P88900-STOREPROC-ABEND-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *    SCENARIO # 8                                               *
      *    DB2 STORED PROCEDURE INEFF SQL SCENARIO
      *****************************************************************

       P89200-STOREPROC-INEFFSQL.

           MOVE WMF-SCHEMA            TO SPGEN01-USERID.
           MOVE WMF-ACTIVE-SCENARIOS  TO SPGEN01-ACTIVE-SCENARIOS.
           MOVE ZEROES                TO SPGEN01-STATUS.


           EXEC SQL
               CALL GENAPP.SPGEN01  (:SPGEN01-USERID,
                                     :SPGEN01-ACTIVE-SCENARIOS,
                                     :SPGEN01-STATUS)
           END-EXEC.


           IF SQLCODE  NOT = ZEROES
               MOVE 'DB2'             TO WS-PDA-ERROR-TYPE
               MOVE 'SPGEN01'         TO WPDE-PROGRAM-ID
               MOVE  SQLCODE          TO WPDE-DB2-SQLCODE
               MOVE 'STORE PROCEDURE' TO WPDE-FUNCTION-1
               MOVE 'P89200'          TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.


           IF SPGEN01-STATUS NOT = ZEROES
               MOVE 'DB2'             TO WS-PDA-ERROR-TYPE
               MOVE 'SPGEN01'         TO WPDE-PROGRAM-ID
               MOVE  ZEROES           TO WPDE-DB2-SQLCODE
               MOVE 'STORE PROCEDURE' TO WPDE-FUNCTION-1
               MOVE  SPGEN01-STATUS   TO WPDE-FUNCTION-2
               MOVE 'P89200'          TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.


       P89200-STOREPROC-INEFFSQL-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *    PERFORM SCENARIO PROCESSING -- MQSERIES SCENARIOS 12,13,14 *
      *    SCENARIO 12 - MQSERIES ERROR, INITIATE ABNORMAL TERMINATE  *
      *    SCENARIO 13 - MQSERIES POOR PERFORMANCE                    *
      *    SCENARIO 14 - MQSERIES S0C7 ABNORMAL TERMINATION           *
      *****************************************************************
      *****************************************************************
      *    REINITIALIZE ALL MQSERIES OBJECTS BEFORE MOTOR PROCESSING  *
      *                    EACH TIME                                  *
      *****************************************************************

       P91000-MQSERIES-PROCESS.

           MOVE ZEROES                 TO MQS-HOBJECT
                                          MQS-COMPCODE
                                          MQS-REASONCODE.

      *****************************************************************
      *    CONNECT TO THE MQSERIES QUEUE MANAGER                      *
      *****************************************************************

           PERFORM  P91700-MQS-CONNECT
               THRU P91700-MQS-CONNECT-EXIT.


      *****************************************************************
      *    CLEAR ALL THE MQSERIES MESSAGES FROM THE                   *
      *****************************************************************

           PERFORM  P92000-CLEAR-MQS-MESSAGES
               THRU P92000-CLEAR-MQS-MESSAGES-EXIT.


      *****************************************************************
      *    PROCESS THE MOTOR (AUTOMOTIVE) DB2 CURSOR                 *
      *    REINITIALIZE ALL MQSERIES OBJECTS BEFORE                   *
      *    MOTOR PROCESSING EACH TIME.                                *
      *****************************************************************

           EXEC SQL
             OPEN MOTOR_CURSOR
           END-EXEC.


           IF SQLCODE NOT EQUAL 0
               MOVE 'DB2'         TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'    TO WPDE-PROGRAM-ID
               MOVE  SQLCODE      TO WPDE-DB2-SQLCODE
               MOVE 'OPEN CURSOR' TO WPDE-FUNCTION
               MOVE 'P91000'      TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.


      *****************************************************************
      ** PERFORM FETCH UNTIL SQLCODE =100 (NO MORE FOUND) OR BAD SQLCODE
      *****************************************************************

           MOVE 'N'    TO WS-PROCESS-COMPLETE-SW.
           PERFORM  P91100-FETCH-MOTOR
               THRU P91100-FETCH-MOTOR-EXIT
                   UNTIL PROCESS-COMPLETE.

      *****************************************************************
      ** CLOSE CURSOR                                                 *
      *****************************************************************

           EXEC SQL
             CLOSE MOTOR_CURSOR
           END-EXEC.

           IF SQLCODE NOT EQUAL 0
               MOVE 'DB2'         TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'    TO WPDE-PROGRAM-ID
               MOVE  SQLCODE      TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE'       TO WPDE-FUNCTION
               MOVE 'P91000'      TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.


            PERFORM  P91900-MQS-DISCONNECT
                 THRU P91900-MQS-DISCONNECT-EXIT.


       P91000-MQSERIES-PROCESS-EXIT.
           EXIT.


      *****************************************************************
      *    FETCH MOTOR DB2 ROWS                                       *
      *    SCENARIO 12 - MQSERIES ERROR, INITIATE ABNORMAL TERMINATE  *
      *    SCENARIO 13 - MQSERIES POOR PERFORMANCE                    *
      *    SCENARIO 14 - MQSERIES S0C7 ABNORMAL TERMINATION           *
      *****************************************************************

       P91100-FETCH-MOTOR.

      *****************************************************************
      ** PERFORM FETCH UNTIL SQLCODE =100  OR BAD SQLCODE             *
      *****************************************************************

           EXEC SQL
              FETCH MOTOR_CURSOR
              INTO
                  :WMF-MOTOR-POLICYNUM,
                  :WMF-MOTOR-MAKE,
                  :WMF-MOTOR-MODEL,
                  :WMF-MOTOR-COLOUR,
                  :WMF-MOTOR-CARYEAR
           END-EXEC.


           IF SQLCODE EQUAL ZEROES
               PERFORM  P91500-ADD-TO-MQSERIES
                   THRU P91500-ADD-TO-MQSERIES-EXIT
           ELSE

           IF SQLCODE EQUAL +100
               MOVE 'Y'           TO WS-PROCESS-COMPLETE-SW
               GO TO P91100-FETCH-MOTOR-EXIT

           ELSE
               MOVE 'DB2'         TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'    TO WPDE-PROGRAM-ID
               MOVE  SQLCODE      TO WPDE-DB2-SQLCODE
               MOVE 'FETCH'       TO WPDE-FUNCTION
               MOVE 'P91100'      TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P91100-FETCH-MOTOR-EXIT.
           EXIT.


      *****************************************************************
      *    ADD MQSERIES RECORDS BASED ON DB2 MOTOR TABLE ROWS         *
      *    SCENARIO 12 - MQSERIES ERROR, INITIATE ABNORMAL TERMINATE  *
      *    SCENARIO 13 - MQSERIES POOR PERFORMANCE                    *
      *****************************************************************

       P91500-ADD-TO-MQSERIES.

      *****************************************************************
      ** MOVE FETCHED DATA TO MQSERIES QUEUE                          *
      *****************************************************************

           MOVE WMF-MOTOR-POLICYNUM              TO MQS-POLICYNUMBER.
           MOVE WMF-MOTOR-MAKE                   TO MQS-MAKE.
           MOVE WMF-MOTOR-MODEL                  TO MQS-MODEL.
           MOVE WMF-MOTOR-COLOUR                 TO MQS-COLOUR.
           MOVE WMF-MOTOR-CARYEAR                TO MQS-CARYEAR.


      *****************************************************************
      *    FORMAT AND WRITE THE MQSERIES MSG TO THE CUSTOMER QUEUE    *
      *****************************************************************

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE MQPER-PERSISTENT       TO MQMD-PERSISTENCE.
           MOVE MQMI-NONE              TO MQMD-MSGID.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQPRI-PRIORITY-AS-Q-DEF
                                       TO MQMD-PRIORITY.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.
           MOVE 5000                   TO MQMD-EXPIRY.

           MOVE MQPMO-CURRENT-VERSION  TO MQPMO-VERSION.
           COMPUTE MQPMO-OPTIONS       =  MQPMO-NO-SYNCPOINT    +
                                          MQPMO-DEFAULT-CONTEXT +
                                          MQPMO-FAIL-IF-QUIESCING +
                                          MQPMO-NEW-MSG-ID.
           MOVE LENGTH OF MQS-BUFFER-INOUT
                                       TO MQS-BUFFERLENGTH.


           PERFORM  P92400-MQS-PUT
               THRU P92400-MQS-PUT-EXIT 1000 TIMES.

           MOVE MQMD-MSGID             TO MQS-MSGID.


           IF  (WMF-ACTIVE-SCENARIOS-R (14)      = 'Y')
               MOVE HIGH-VALUES TO  MQS-CARYEAR-R
               COMPUTE WMF-MAX-YEAR =  MQS-CARYEAR + 1.


       P91500-ADD-TO-MQSERIES-EXIT.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P91700-MQS-CONNECT                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONNECT TO THE MQSERIES QUEUE       *
      *                MANAGER                                        *
      *                                                               *
      *    CALLED BY:  P91000-MQSERIES-PROCESS.                       *
      *                                                               *
      *****************************************************************

       P91700-MQS-CONNECT.



           CALL 'MQCONN'      USING    MQS-QMANAGER-NAME
                                       MQS-HCONN
                                       MQS-COMPCODE
                                       MQS-REASONCODE.


      *****************************************************************
      *    CHECK FOR MQSERIES ERROR, IF ERROR ENCOUNTERED FORMAT      *
      *    ERROR MESSAGE, CALL ERROR ROUTINE TO TERMINATE             *
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'         TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQCONN'           TO WPME-FUNCTION-1
               MOVE 'QUEUE MANAGER'    TO WPME-FUNCTION-2
               MOVE 'P91700'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P91700-MQS-CONNECT-EXIT.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P91900-MQS-DISCONNECT                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DISCONNECT FROM THE MQSERIES QUEUE  *
      *                MANAGER                                        *
      *                                                               *
      *    CALLED BY:  P91000-MQSERIES-PROCESS.                       *
      *                                                               *
      *****************************************************************

       P91900-MQS-DISCONNECT.

           CALL 'MQDISC'      USING    MQS-HCONN
                                       MQS-COMPCODE
                                       MQS-REASONCODE.


      *****************************************************************
      *    CHECK FOR MQSERIES ERROR, IF ERROR ENCOUNTERED FORMAT      *
      *    ERROR MESSAGE, CALL ERROR ROUTINE TO TERMINATE             *
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               MOVE ZEROES             TO MQS-HCONN
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'         TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQDISC'           TO WPME-FUNCTION-1

               MOVE 'QUEUE MANAGER'    TO WPME-FUNCTION-2
               MOVE 'P91900'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P91900-MQS-DISCONNECT-EXIT.
           EXIT.


      *****************************************************************
      *    CLEAR ALL THE MQSERIES MESSAGES FROM THE INPUT QUEUE       *
      *****************************************************************

       P92000-CLEAR-MQS-MESSAGES.

           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.

      **************************************************************
      *    QUERY REQUEST QUEUE NAME IS USERID SPECIFIC, PREFIXED BY*
      *    USERID. FORMAT QUEUE NAME USING PARAMETER INPUT USERID  *
      **************************************************************

           MOVE WMF-USERID             TO MQS-USERID.
           MOVE SPACES                 TO MQS-MOTOR-QUEUE-COMPRESS.
           MOVE ZEROES                 TO WS-SUB2.

           PERFORM  P92030-FORMAT-Q-NAME
               THRU P92030-FORMAT-Q-NAME-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > WS-QUEUE-NAME-LTH.

           MOVE MQS-MOTOR-QUEUE-COMPRESS  TO MQOD-OBJECTNAME.

           DISPLAY 'MQS-MOTOR-QUEUE-COMPRESS = '.
           DISPLAY  MQS-MOTOR-QUEUE-COMPRESS.


      **************************************************************
      *    OPEN QUEUE
      **************************************************************

           PERFORM P92100-MQS-OPEN
               THRU P92100-MQS-OPEN-EXIT.


      **************************************************************
      *    SET OBJECT PARAMETERS                                   *
      **************************************************************

           MOVE 'QUEUE'                   TO MQS-OBJECTTYPE-DESC.
           COMPUTE MQS-OPTIONS         =  MQOO-INPUT-SHARED      +
                                          MQOO-OUTPUT            +
                                          MQOO-SAVE-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.


      *****************************************************************
      *    GET / DELETE MQSERIES CUSTOMER MESSAGE FROM THE QUEUE      *
      *****************************************************************

           MOVE 'Y'                    TO WS-MORE-MESSAGES-SW.

           PERFORM  P92200-GET-MESSAGES
               THRU P92200-GET-MESSAGES-EXIT
                  UNTIL NO-MORE-MESSAGES.


       P92000-CLEAR-MQS-MESSAGES-EXIT.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P92030-FORMAT-Q-NAME                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO COMPRESS ANY BLANKS FROM THE        *
      *                CUSTOMER REQUEST QUEUE NAME. SITUATION OCCURS  *
      *                IF THE USERID PREFIX IS LESS THAN 8 CHARACTERS.*
      *                                                               *
      *    CALLED BY:  P92000-CLEAR-MQS-MESSAGES                      *
      *                                                               *
      *****************************************************************


       P92030-FORMAT-Q-NAME.

      *****************************************************************
      *    MOVE ONLY NON-BLANK CHARACTERS FROM SOURCE TO TARGET       *
      *****************************************************************

           IF MQS-A-BYTE-01 (WS-SUB1)  NOT EQUAL TO SPACES
               ADD +1                  TO WS-SUB2
               MOVE MQS-A-BYTE-01 (WS-SUB1)
                                       TO MQS-A-BYTE-02 (WS-SUB2)
           ELSE
               NEXT SENTENCE.

       P92030-FORMAT-Q-NAME-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P92100-MQS-OPEN                                *
      *                                                               *
      *    FUNCTION :  ROUTINE TO OPEN A MQSERIES OBJECT. ALL         *
      *                OPTIONS AND PARAMETERS ARE SET BY THE CALLING  *
      *                PARAGRAPH AND VARY ACCORDING TO THE OBJECT     *
      *                TYPE BEING OPENED.                             *
      *                                                               *
      *    CALLED BY:  P92000-CLEAR-MQS-MESSAGES                      *
      *                                                               *
      *****************************************************************

       P92100-MQS-OPEN.

      *****************************************************************
      *    OPEN THE MQSERIES CUSTOMER QUEUE FOR OUTPUT                *
      *****************************************************************

           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.
           MOVE MQOT-Q                 TO MQOD-OBJECTTYPE.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.


           IF  (WMF-ACTIVE-SCENARIOS-R (12)      = 'Y')
               MOVE HIGH-VALUES        TO MQOD-OBJECTNAME
           ELSE
               MOVE MQS-MOTOR-QUEUE-COMPRESS
                                       TO MQOD-OBJECTNAME.


           COMPUTE MQS-OPTIONS         =  MQOO-OUTPUT           +
                                          MQOO-INPUT-SHARED     +
                                          MQOO-PASS-ALL-CONTEXT +
                                          MQOO-FAIL-IF-QUIESCING.


      *****************************************************************
      *    OPEN THE MQSERIES CUSTOMER QUEUE                           *
      *****************************************************************

           CALL 'MQOPEN'      USING       MQS-HCONN
                                          MQOD
                                          MQS-OPTIONS
                                          MQS-HOBJECT
                                          MQS-COMPCODE
                                          MQS-REASONCODE.


      *****************************************************************
      *    CHECK FOR MQSERIES ERROR, IF ERROR ENCOUNTERED FORMAT      *
      *    ERROR MESSAGE, CALL ERROR ROUTINE TO TERMINATE             *
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'         TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQOPEN'           TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P92100'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P92100-MQS-OPEN-EXIT.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P92200-GET-MESSAGES                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO GET MQSERIES MESSAGES FROM THE      *
      *                QUEUE                                          *
      *                                                               *
      *    CALLED BY:  P92000-CLEAR-MQS-MESSAGES                      *
      *                                                               *
      *****************************************************************

       P92200-GET-MESSAGES.

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE MQPER-PERSISTENT       TO MQMD-PERSISTENCE.
           MOVE MQS-MSGID              TO MQMD-MSGID.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.


           MOVE MQGMO-CURRENT-VERSION  TO MQGMO-VERSION.
           COMPUTE MQGMO-OPTIONS       =  MQGMO-NO-WAIT           +
                                          MQGMO-CONVERT           +
                                          MQGMO-FAIL-IF-QUIESCING +
                                          MQGMO-NO-SYNCPOINT.
           MOVE MQMO-MATCH-MSG-ID      TO MQGMO-MATCHOPTIONS.
           MOVE LENGTH OF MQS-BUFFER-INOUT
                                       TO MQS-BUFFERLENGTH.


           PERFORM P92300-MQS-GET
              THRU P92300-MQS-GET-EXIT.


           IF (MQS-REASONCODE           =  MQRC-NO-MSG-AVAILABLE)
               MOVE 'N'                TO WS-MORE-MESSAGES-SW
               GO TO P92200-GET-MESSAGES-EXIT.


           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'         TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQOPEN'           TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P92200'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P92200-GET-MESSAGES-EXIT.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P92300-MQS-GET                                 *
      *                                                               *
      *    FUNCTION :  ROUTINE TO GET MQSERIES MESSAGES FROM THE      *
      *                QUEUE                                          *
      *                                                               *
      *    CALLED BY:  P92200-GET-MESSAGES                            *
      *                                                               *
      *****************************************************************

       P92300-MQS-GET.

           CALL 'MQGET'       USING    MQS-HCONN
                                       MQS-HOBJECT
                                       MQMD
                                       MQGMO
                                       MQS-BUFFERLENGTH
                                       MQS-BUFFER-INOUT
                                       MQS-DATALENGTH
                                       MQS-COMPCODE
                                       MQS-REASONCODE.


      *****************************************************************
      *    CHECK FOR MQSERIES ERROR, IF ERROR ENCOUNTERED FORMAT      *
      *    ERROR MESSAGE, CALL ERROR ROUTINE TO TERMINATE             *
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
           IF MQS-REASONCODE           =  MQRC-NO-MSG-AVAILABLE
               GO TO P92300-MQS-GET-EXIT
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT001'         TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQGET'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P92300'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P92300-MQS-GET-EXIT.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P92400-MQS-PUT                                 *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PUT MQSERIES MESSAGES TO THE QUEUE  *
      *                                                               *
      *    CALLED BY:  P92200-GET-MESSAGES                            *
      *                                                               *
      *****************************************************************

       P92400-MQS-PUT.

           CALL 'MQPUT'       USING    MQS-HCONN
                                       MQS-HOBJECT
                                       MQMD
                                       MQPMO
                                       MQS-BUFFERLENGTH
                                       MQS-BUFFER-INOUT
                                       MQS-COMPCODE
                                       MQS-REASONCODE.


      *****************************************************************
      *    CHECK FOR MQSERIES ERROR, IF ERROR ENCOUNTERED FORMAT      *
      *    ERROR MESSAGE, CALL ERROR ROUTINE TO TERMINATE             *
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               MOVE MQMD-MSGID         TO MQS-MSGID
               GO TO P92400-MQS-PUT-EXIT.


           IF MQS-REASONCODE     = (MQRC-PAGESET-FULL OR
                                    MQRC-STORAGE-MEDIUM-FULL)
               GO TO P92400-MQS-PUT-EXIT.


           MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
           MOVE 'LGBAT001'         TO WPME-PROGRAM-ID
           MOVE MQS-REASONCODE     TO WPME-REASON-CODE
           MOVE 'MQPUT'            TO WPME-FUNCTION-1
           MOVE MQS-OBJECTTYPE-DESC
                                   TO WPME-FUNCTION-2
           MOVE 'P92400'           TO WPME-PARAGRAPH
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.


       P92400-MQS-PUT-EXIT.
           EXIT.



      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P93000-PLI-PROCESS                             *
      *                                                               *
      *    FUNCTION :  ROUTINE PERFORMS PLI PROCESS VIA               *
      *                INVOCATION OF PLI MODULE LGBAT002              *
      *                                                               *
      *    CALLED BY:  P92200-GET-MESSAGES                            *
      *                                                               *
      *****************************************************************

       P93000-PLI-PROCESS.

           CALL 'LGBA002'.


       P93000-PLI-PROCESS-EXIT.
           EXIT.



      *****************************************************************
      *                                                               *
      *    P R O D U C T    D E M O N S T R A T I O N     A P P L     *
      *                                                               *
      *             E R R O R    R O U T I N E S                      *
      *                                                               *
      *                                                               *
      *****************************************************************


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99500-PDA-ERROR                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE FATAL / TERMINATING GENERAL, *
      *                DB2, IMS-DLI, MQSERIES ERRORS                  *
      *                                                               *
      *                ERROR TEXT IS DISPLAYED                        *
      *                TO THE USER INDICATING THE NATURE OF THE ERROR *
      *                                                               *
      *                PROGRAM IS ABNORMALLY TERMINATED               *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P99500-PDA-ERROR.

           MOVE 'Y'                    TO WS-ERROR-FOUND-SW.

           DISPLAY ' '.
           DISPLAY WPEA-ERROR-01.
           DISPLAY WPEA-ERROR-02.
           DISPLAY WPEA-ERROR-03.
           DISPLAY WPEA-ERROR-04.
           DISPLAY WPEA-ERROR-05.
           DISPLAY WPEA-ERROR-06.

      *****************************************************************
      *      FORMAT AND SEND ERROR TEXT                               *
      *****************************************************************

           IF PDA-DB2-ERROR
               MOVE WS-PDA-DB2-ERROR-01
                                       TO WPEA-ERROR-07-TEXT
               MOVE WS-PDA-DB2-ERROR-02
                                       TO WPEA-ERROR-08-TEXT
           ELSE
           IF PDA-IMS-ERROR
               MOVE WS-PDA-IMS-ERROR-01
                                       TO WPEA-ERROR-07-TEXT
               MOVE WS-PDA-IMS-ERROR-02
                                       TO WPEA-ERROR-08-TEXT
           ELSE
           IF PDA-MQSERIES-ERROR
               MOVE WS-PDA-MQSERIES-ERROR-01
                                       TO WPEA-ERROR-07-TEXT
               MOVE WS-PDA-MQSERIES-ERROR-02
                                       TO WPEA-ERROR-08-TEXT
           ELSE
               MOVE WS-PDA-GEN-ERROR-01
                                       TO WPEA-ERROR-07-TEXT
               MOVE WS-PDA-GEN-ERROR-02
                                       TO WPEA-ERROR-08-TEXT.

           DISPLAY WPEA-ERROR-07.
           DISPLAY WPEA-ERROR-08.

           DISPLAY WPEA-ERROR-09.
           DISPLAY WPEA-ERROR-10.
           DISPLAY ' '.


           DISPLAY WPM-BLANK.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY WPM-END-PROGRAM.
           DISPLAY WPM-ALL-ASTERISK.


           CALL 'SNAPAID'.

      **** MOVE 99                     TO WS-RETURN-CODE.
      **** CALL 'CEE3ABD'              USING WS-RETURN-CODE, WS-TIMING.
      **** GOBACK.

      **** MOVE 99                     TO WS-RETURN-CODE.
      **** CALL 'ILBOABN0'             USING WS-RETURN-CODE.
      **** MOVE WS-RETURN-CODE         TO RETURN-CODE.
      **** GOBACK.

           GOBACK.

       P99500-PDA-ERROR-EXIT.
           EXIT.
           EJECT
