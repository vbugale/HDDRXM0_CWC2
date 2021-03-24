       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGBAT003.

      *****************************************************************
      *                 GENAPP DEMONSTRATION APPLICATION              *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   LGBAT003                                          *
      *                                                               *
      * FUNCTION:   PROGRAM LGBAT003 IS A GENAPP DEMONSTRATION        *
      *             BATCH PROGRAM WITH AN EMPHASIS ON IMS BATCH       *
      *             PROCESSING.                                       *
      *                                                               *
      *             IMS SCENARIOS:                                    *
      *             1. IMS ABNORMAL TERMINATION (BAD STATUS CODE)     *
      *             2. IMS S0C7 (DATA EXCEPTION) ABNORMAL TERMINATE   *
      *             3. IMS POOR PERFORMANCE SCENARIO                  *
      *                                                               *
      *                                                               *
      * FILES   :   CUSTOMER DATABASE    (IMS)        (INPUT)         *
      *                                                               *
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      * DATE        UPDATED BY            CHANGE DESCRIPTION          *
      * ----------  --------------------  --------------------------  *
      * 10/13/2017  PAUL BARON            INITIAL DEVELOPMENT         *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      * MM/DD/YYYY  XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                               *
      *****************************************************************

       ENVIRONMENT DIVISION.
                                                                        00380000
       INPUT-OUTPUT SECTION.                                            00390000
       FILE-CONTROL.                                                    00410000
                                                                        00420000
           SELECT INPUT-PARAMETERS   ASSIGN TO IPARAMS.                 00420000
                                                                        00420000
                                                                        00420000
       DATA DIVISION.                                                   00610000
       FILE SECTION.                                                    00630000
                                                                        00640000
      *****************************************************************
      *    FILE DECLARATIONS                                          *
      *****************************************************************

       FD INPUT-PARAMETERS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 27920 CHARACTERS.

       01  INPUT-PARAMETER-RECORD      PIC X(80).
                                                                        01220000
                                                                        01220000
       WORKING-STORAGE SECTION.

      *****************************************************************
      *    77 LEVEL DATA ITEMS HERE  (SUBSCRIPTS, INDEXES ETC.)       *
      *****************************************************************
       77  WS-SUB                      PIC S9(04)  COMP   VALUE +0.
       77  WS-SUB1                     PIC S9(05)  COMP   VALUE +0.
       77  WS-RETURN-CODE              PIC  9(04)  COMP   VALUE  0.
       77  WS-MAX-PARAMETERS           PIC S9(04)  COMP   VALUE +500.
       77  WS-USERID-PARM-COUNT        PIC S9(04)  COMP   VALUE +0.
       77  WS-PARAMETER-RECORDS-IN     PIC S9(05)  COMP-3 VALUE +0.
       77  WS-COUNT                    PIC S9(07)  COMP-3 VALUE +0.
       77  WS-CUSTOMER-START-KEY       PIC 9(10)          VALUE  0.


      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.

           05  WS-ERROR-FOUND-SW       PIC X(01)             VALUE 'N'.
               88  ERROR-FOUND                               VALUE 'Y'.
               88  NO-ERROR-FOUND                            VALUE 'N'.

           05  WS-END-OF-PARM-FILE-SW    PIC X(01)           VALUE 'N'.
               88  END-OF-PARM-FILE                          VALUE 'Y'.
               88  NOT-END-OF-PARM-FILE                      VALUE 'N'.

           05  WS-PARM-ERROR-FOUND-SW  PIC X(01)             VALUE 'N'.
               88  PARM-ERROR-FOUND                          VALUE 'Y'.
               88  NOT-PARM-ERROR-FOUND                      VALUE 'N'.

           05  WS-END-OF-DATABASE-SW   PIC X(01)             VALUE 'N'.
               88  END-OF-DATABASE                           VALUE 'Y'.
               88  NOT-END-OF-DATABASE                       VALUE 'N'.
           EJECT


      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************

       01  WS-RETURN-CODE PICTURE S9(9) USAGE BINARY VALUE ZERO.
       01  WS-TIMING      PICTURE S9(9) USAGE BINARY VALUE ZERO.

       01  WS-MISCELLANEOUS-FIELDS.
           05  WMF-DATE-YYMMDD.
               10 WMF-DATE-YY          PIC 9(02)   VALUE ZEROES.
               10 WMF-DATE-MM          PIC 9(02)   VALUE ZEROES.
               10 WMF-DATE-DD          PIC 9(02)   VALUE ZEROES.

           05  WMF-TIME-HHMMSS         PIC X(08)   VALUE SPACES.
           05  WMF-MESSAGE-AREA        PIC X(80)   VALUE SPACES.


           05  WMF-ACTIVE-SCENARIOS    PIC X(250)  VALUE SPACES.
           05  WMF-ACTIVE-SCENARIOS-R  REDEFINES WMF-ACTIVE-SCENARIOS
                                       OCCURS 250 TIMES
                                       PIC X(01).
           05  WMF-USERID              PIC X(08)   VALUE 'USERIDXX'.



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
               10  WPR-SCENARIO-NUMBER-R
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


      ***************************************************************** 03060000
      *    IMS FUNCTION DEFINITIONS                                   * 03070000
      ***************************************************************** 03080000
                                                                        03330000
       01  IMS-CALL-FUNCTIONS.                                          03330000
           05 ICF-GU                   PIC X(04)   VALUE 'GU'.          03330000
           05 ICF-GHU                  PIC X(04)   VALUE 'GHU'.         03330000
           05 ICF-GHN                  PIC X(04)   VALUE 'GHN'.         03330000
           05 ICF-GN                   PIC X(04)   VALUE 'GN'.          03330000
           05 ICF-ISRT                 PIC X(04)   VALUE 'ISRT'.        03330000
           05 ICF-REPL                 PIC X(04)   VALUE 'REPL'.        03330000
           05 ICF-DLET                 PIC X(04)   VALUE 'DLET'.        03330000
                                                                        03330000
                                                                        03330000
      ***************************************************************** 03060000
      *    IMS SEGMENT SEARCH ARGUMENTS (SSA)                         * 03070000
      ***************************************************************** 03080000
                                                                        03090000
       01  CUSTOMER-SSA-UNQUAL.                                         03100000
           03  FILLER                  PIC X(8)  VALUE 'CUSTMR'.        03110000
           03  FILLER                  PIC X     VALUE SPACES.
                                                                        03090000
       01  CUSTOMER-SSA-QUAL.                                           03100000
           03  FILLER                  PIC X(8)  VALUE 'CUSTMR'.        03110000
           03  FILLER                  PIC X     VALUE '('.             03120000
           03  FILLER                  PIC X(8)  VALUE 'CUSKEY'.        03130000
           03  FILLER                  PIC X(3)  VALUE ' = '.           03140000
           03  CUST-KEY                PIC 9(10) VALUE ZEROES.
           03  FILLER                  PIC X     VALUE ')'.             03180000
                                                                        03190000
                                                                        03090000
       01  CUSTOMER-SSA-QUAL-LASTNAME.                                  03100000
           03  FILLER                  PIC X(8)  VALUE 'CUSTMR  '.
           03  FILLER                  PIC X     VALUE '('.
           03  FILLER                  PIC X(8)  VALUE 'LNAME   '.
           03  FILLER                  PIC X(2)  VALUE 'GT'.
           03  CUST-LNAME              PIC X(20) VALUE SPACES.
           03  FILLER                  PIC X     VALUE ')'.             03180000
           EJECT                                                        03270000
                                                                        03330000
                                                                        03330000
      ***************************************************************** 03280000
      *    IMS SEGMENT I/O AREAS                                      * 03290000
      ***************************************************************** 03300000
                                                                        03330000
      ***************************************************************** 03280000
      *    CUSTOMER ROOT SEGMENT                                      * 03290000
      ***************************************************************** 03300000
      ******************************************************************
      * IMS CUSTOMER SEGMENT                                           *
      * SEGMENT     : CUSTMR                                           *
      * DATABASE    : CUSTMRDB                                         *
      * ORGANIZATION: HIDAM                                            *
      ******************************************************************
       01  CUSTOMER-SEGMENT.
           05  CUST-NUM-KEY          PIC 9(10).
           05  CUST-FIRST-NAME       PIC X(10)  VALUE 'JOHN'.
           05  CUST-LAST-NAME        PIC X(20)  VALUE 'SMITH'.
           05  CUST-DOB              PIC X(10)  VALUE '2017-10-31'.
           05  CUST-HOUSE-NAME       PIC X(20)  VALUE 'METAMORA'.
           05  CUST-HOUSE-NUMBER     PIC X(04)  VALUE '1375'.
           05  CUST-POST-CODE        PIC X(08)  VALUE '38-48044'.
           05  CUST-PHONE-HOME       PIC X(20)  VALUE '313-264-5567'.
           05  CUST-PHONE-MOBILE     PIC X(20)  VALUE '978-463-8897'.
           05  CUST-EMAIL-ADDRESS    PIC X(100) VALUE 'ME@GMAIL.COM'.
           05  CUST-NATIONAL-ID-NUM  PIC X(20)  VALUE '97864001'.
           05  CUST-NIN-TYPE         PIC X(02)  VALUE 'A9'.
           05  CUST-STREET-ADDRESS   PIC X(30)  VALUE '123 MAIN'.
           05  CUST-CITY             PIC X(20)  VALUE 'TROY'.
           05  CUST-STATE            PIC X(02)  VALUE 'CA'.
           05  CUST-COUNTRY-CODE     PIC X(03)  VALUE 'USA'.
           05  CUST-SALES-TERRITORY  PIC X(05)  VALUE '68719'.
           EJECT


      *****************************************************************
      *    GENERAL ERROR PROCESSING WORK AREAS                        *
      *****************************************************************
      ******************************************************************
      * PRODUCT DEMONSTRATION APPLICATION (PDA)                        *
      *                                                                *
      * ERROR WORK AREA DEFINITIONS FOR: CICS, IMS-DLI, DB2, MQSERIES  *
      *                                                                *
      ******************************************************************

       01  WS-PDA-ERROR-GENERAL.

           05  WS-PDA-ERROR-TYPE       PIC X(04)       VALUE SPACES.
               88  PDA-GENERAL-ERROR                   VALUE 'GEN'.
               88  PDA-DB2-ERROR                       VALUE 'DB2'.
               88  PDA-IMS-ERROR                       VALUE 'IMS'.
               88  PDA-MQSERIES-ERROR                  VALUE 'MQS'.


      ******************************************************************
      *    PDA FORMATTED ERROR LINES                                   *
      ******************************************************************

       01  WS-PDA-ERROR-AREA.
           05  WPEA-ERROR-01           PIC X(80)       VALUE ALL '*'.
           05  WPEA-ERROR-02.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-03.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE
               '   PRODUCT DEMONSTRATION APPLICATION (PDA) ERROR '.
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


      ******************************************************************
      *    PDA GENERAL ERROR LINES                                     *
      ******************************************************************

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



      ******************************************************************
      *    PDA IMS-DLI ERROR LINES                                     *
      ******************************************************************

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


      ******************************************************************
      *    PDA MQSERIES ERROR LINES                                    *
      ******************************************************************

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

       01  WS-LGBAT003-MESSAGES.

           05  WPM-BLANK               PIC X(01)       VALUE     ' '.
           05  WPM-ALL-ASTERISK        PIC X(80)       VALUE ALL '*'.

           05  WPM-BEGIN-PROGRAM.
               10 FILLER               PIC X(78)   VALUE
                  '***** BEGIN PROGRAM LGBAT003 *****'.

           05  WPM-END-PROGRAM.
               10 FILLER               PIC X(78)   VALUE
                  '***** END PROGRAM LGBAT003 *****'.

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
                  'POSITION 3 - 5, SCENARIO NUMBER MUST BE NUMERIC, VALU
      -           'E 1 THRU 250'.


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

       01  WS-END-OF-WS.
           05  FILLER                  PIC X(05)   VALUE '#####'.


      *****************************************************************
      *    L I N K A G E     S E C T I O N                            *
      *****************************************************************

       LINKAGE SECTION.
                                                                        03780000
      ****************************************************************  03790000
      *****  I-O PCB                                                    03800000
      ****************************************************************  03810000
      *                                                                 03820000
      *01  IO-PCB.                                                      03830000
      *    05  FILLER                  PIC X(10).                       03840000
      *    05  IO-STATUS               PIC XX   .                       03850000
      *    05  FILLER                  PIC X(20).                       03860000

                                                                        03780000
      ****************************************************************  03790000
      *****  CUSTOMER PCB                                               03800000
      ****************************************************************  03810000

       01  CUST-PCB.
           05  CUST-DBDNAME            PIC X(08).
           05  CUST-SEG-LEVEL          PIC X(02).
           05  CUST-STATUS             PIC X(02).
           05  CUST-PROCOPT            PIC X(04).
           05  FILLER                  PIC X(04).
           05  CUST-SEG-NAME-FB        PIC X(08).
           05  CUST-KEY-FB-LTH         PIC S9(5) COMP.
           05  FILLER                  PIC X(4).
           05  CUST-KEY-FB-AREA        PIC X(10).
           EJECT


      *****************************************************************
      *    P R O C E D U R E    D I V I S I O N                       *
      *****************************************************************

       PROCEDURE DIVISION.

           ENTRY 'DLITCBL' USING CUST-PCB.

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00000-MAINLINE                                *
      *                                                               *
      *    FUNCTION :  PROGRAM ENTRY, CONTROL HIGH LEVEL PROCESSING   *
      *                FOR THE PRODUCT DEMONSTRATION APPLICATION      *
      *                BATCH PROCESS                                  *
      *                                                               *
      *    CALLED BY:  NONE                                           *
      *                                                               *
      *****************************************************************

       P00000-MAINLINE.

           DISPLAY WPM-BLANK.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY WPM-BEGIN-PROGRAM.
           DISPLAY WPM-ALL-ASTERISK.


           PERFORM  P00050-INITIALIZE                                   TAGGED
               THRU P00050-INITIALIZE-EXIT.                             CODE
                                                                        TESTING
                                                                        03/13/01
           IF NO-ERROR-FOUND
               PERFORM  P00500-MAIN-PROCESS
                   THRU P00500-MAIN-PROCESS-EXIT.
                                                                        TESTING
                                                                        03/13/01
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
                                          WS-END-OF-DATABASE-SW.


      *****************************************************************
      *    OBTAIN CURRENT DATE AND TIME                               *
      *****************************************************************

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          00020001


      *****************************************************************
      *    OPEN FILES                                                 *
      *****************************************************************

           OPEN INPUT    INPUT-PARAMETERS.                              00020001


      *****************************************************************
      *    PERFORM 1ST READ ON PARAMETER FILE -- EOF IS AN ERROR      *
      *****************************************************************

           PERFORM  P80000-READ-PARAMETERS
               THRU P80000-READ-PARAMETERS-EXIT.

           IF END-OF-PARM-FILE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT003'         TO WPGE-PROGRAM-ID
               MOVE 'P00050'           TO WPGE-PARAGRAPH
               MOVE WPM-PARAMETER-FILE-EMPTY
                                       TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    PERFORM CUSTOMER DATA INITIALIZATION  DELETE ALL SEGMENTS  *
      *****************************************************************

           PERFORM  P90400-INIT-CUSTOMER-DATA
               THRU P90400-INIT-CUSTOMER-DATA-EXIT.


      *****************************************************************
      *    RE-LOAD CUSTOMER DATA IMS SEGMENTS                         *
      *****************************************************************

           MOVE ZEROES    TO WS-CUSTOMER-START-KEY.

           PERFORM  P90600-INSERT-CUSTOMER
               THRU P90600-INSERT-CUSTOMER-EXIT 50 TIMES.


       P00050-INITIALIZE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *    FUNCTION :  CONTROL HIGH LEVEL PROCESSING FOR BOTH         *
      *                PARAMETER AND CUSTOMER PROCESSES               *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00500-MAIN-PROCESS.


      *****************************************************************
      *    PERFORM INPUT PARAMETER PROCESS -- IF ERROR FOUND, EXIT    *
      *****************************************************************

           PERFORM  P00600-PARAMETER-PROCESS                            TAGGED
               THRU P00600-PARAMETER-PROCESS-EXIT.                      CODE
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
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

           CLOSE  INPUT-PARAMETERS.                                     00020001


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
      *    PROCESS PARAMETERS FOR FIRST RECORD READ IN INITIALIZATION *
      *    P00050-INITIALIZE                                          *
      *****************************************************************

           MOVE +1                    TO WS-SUB1.
           MOVE WS-PARAMETER-RECORD   TO WPRA-RECORD (WS-SUB1).


      *****************************************************************
      *    PROCESS PARAMETERS UNTIL END OF FILE                       *
      *****************************************************************

           PERFORM  P00630-LOAD-PARM-ARRAY                              TAGGED
               THRU P00630-LOAD-PARM-ARRAY-EXIT                         CODE
                   UNTIL END-OF-PARM-FILE.                              TESTING
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
               GO TO P00600-PARAMETER-PROCESS-EXIT.


      *****************************************************************
      *    PERFORM PARAMETER RECORD EDITS                             *
      *****************************************************************

           MOVE SPACES                 TO WMF-ACTIVE-SCENARIOS.

           PERFORM  P00660-EDIT-PARMS
               THRU P00660-EDIT-PARMS-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > WS-PARAMETER-RECORDS-IN.
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
               GO TO P00600-PARAMETER-PROCESS-EXIT.


      *****************************************************************
      *    IF NO USER ID SPECIFICATION RECORD, ERROR - TERMINATE      *
      *****************************************************************

           IF WS-USERID-PARM-COUNT     > ZEROES                         00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT003'         TO WPGE-PROGRAM-ID
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
               MOVE 'LGBAT003'         TO WPGE-PROGRAM-ID
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
      *    FUNCTION :  ROUTINE TO EDIT THE PARAMETER RECORD SYNTAX     *
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
               MOVE 'LGBAT003'         TO WPGE-PROGRAM-ID
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
               IF (WPR-SCENARIO-NUMBER NUMERIC)    AND
                  (WPR-SCENARIO-NUMBER-R > 0)      AND
                  (WPR-SCENARIO-NUMBER-R < 251)
                   MOVE 'Y'            TO WMF-ACTIVE-SCENARIOS-R
                                             (WPR-SCENARIO-NUMBER-R)
               ELSE
                   MOVE WPM-INVALID-SCENARIO-NUMBER
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
           ELSE                                                         00020001
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
           ELSE                                                         00020001
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
      *    PROCESS SCENARIOS                                          *
      *****************************************************************

       P85000-PROCESS-SCENARIOS.

      *****************************************************************
      *    PROCESS ACTIVATED SCENARIOS                                *
      *****************************************************************

           IF  WMF-ACTIVE-SCENARIOS-R (9)       = 'Y'
               PERFORM  P85200-IMS-ABEND
                   THRU P85200-IMS-ABEND-EXIT
               MOVE SPACES TO WMF-ACTIVE-SCENARIOS-R (9).


           IF  WMF-ACTIVE-SCENARIOS-R (10)      = 'Y'
               PERFORM  P85600-IMS-BAD-PERFORM
                   THRU P85600-IMS-BAD-PERFORM-EXIT  5000 TIMES
               MOVE SPACES TO WMF-ACTIVE-SCENARIOS-R (10).


           IF  WMF-ACTIVE-SCENARIOS-R (11)      = 'Y'
               PERFORM  P85800-IMS-S0C7
                   THRU P85800-IMS-S0C7-EXIT
               MOVE SPACES TO WMF-ACTIVE-SCENARIOS-R (11).


       P85000-PROCESS-SCENARIOS-EXIT.
           EXIT.


      *****************************************************************
      *    SCENARIO #9 IMS ABEND --  BAD STATUS CODE                  *
      *****************************************************************

       P85200-IMS-ABEND.

           MOVE SPACES                 TO CUSTOMER-SEGMENT.
           MOVE 1                      TO CUST-NUM-KEY.

           PERFORM P90200-ISRT-CUSTOMER
              THRU P90200-ISRT-CUSTOMER-EXIT.

           PERFORM P90200-ISRT-CUSTOMER
              THRU P90200-ISRT-CUSTOMER-EXIT.


       P85200-IMS-ABEND-EXIT.
           EXIT.


      *****************************************************************
      *    SCENARIO #10 IMS BAD PERFORMANCE                           *
      *****************************************************************

       P85600-IMS-BAD-PERFORM.


      *****************************************************************
      *    READ A CUSTOMER ROOT SEGMENT POSITION AT BEGIN OF DB
      *****************************************************************

           CALL 'CBLTDLI' USING
                          ICF-GU
                          CUST-PCB
                          CUSTOMER-SEGMENT
                          CUSTOMER-SSA-UNQUAL
           END-CALL.                                                    13550000
                                                                        13580000
           IF CUST-STATUS              = SPACES OR 'GE' OR 'GB'         13590000
               NEXT SENTENCE                                            13600000
           ELSE                                                         13610000
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE             13620000
               MOVE 'LGBAT003'         TO WPIE-PROGRAM-ID               13630000
               MOVE 'P85600'           TO WPIE-PARAGRAPH
               MOVE CUST-STATUS        TO WPIE-STATUS-CODE              13650000
               MOVE 'GU'               TO WPIE-FUNCTION-CODE            13660000
               MOVE 'CUSTMR'           TO WPIE-SEGMENT-NAME             13670000
               MOVE 'CUSTMRDB'         TO WPIE-DATABASE-NAME            13680000
               MOVE 'GU CUSTMR SEGMENT'
                                       TO WPIE-COMMAND                  13690000
               PERFORM  P99500-PDA-ERROR                                13700000
                   THRU P99500-PDA-ERROR-EXIT                           13700000
           END-IF.                                                      13710000
                                                                        13720000

      *****************************************************************
      *    READ CUSTOMER ROOT SEGMENTS UNTIL END OF DATABASE
      *****************************************************************

           MOVE 'N'      TO WS-END-OF-DATABASE-SW.
           MOVE SPACES TO CUST-STATUS.

           PERFORM  P90130-GN-CUSTOMER
               THRU P90130-GN-CUSTOMER-EXIT
                   UNTIL END-OF-DATABASE.                               13720000


       P85600-IMS-BAD-PERFORM-EXIT.
           EXIT.                                                        13740000
           EJECT                                                        13750000

      ***************************************************************** 13370000
      *                                                               * 13380000
      *    PARAGRAPH:  P90130-GN-CUSTOMER                             * 13390000
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ ORDER ROOT SEGMENTS            *
      *                SEQUENTIALLY                                   *
      *                                                               *
      *****************************************************************
      *
       P90130-GN-CUSTOMER.

           CALL 'CBLTDLI' USING
                          ICF-GN
                          CUST-PCB
                          CUSTOMER-SEGMENT
                          CUSTOMER-SSA-QUAL-LASTNAME
           END-CALL.


           IF CUST-STATUS              = 'GE' OR 'GB'
               MOVE 'Y'                TO WS-END-OF-DATABASE-SW
               GO TO P90130-GN-CUSTOMER-EXIT
           ELSE
           IF CUST-STATUS              = SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT003'         TO WPIE-PROGRAM-ID
               MOVE 'P90130'           TO WPIE-PARAGRAPH
               MOVE CUST-STATUS        TO WPIE-STATUS-CODE
               MOVE 'GN'               TO WPIE-FUNCTION-CODE
               MOVE 'CUSTMR'           TO WPIE-SEGMENT-NAME
               MOVE 'CUSTMRDB'         TO WPIE-DATABASE-NAME
               MOVE 'GN ORDER SEGMENT' TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.

       P90130-GN-CUSTOMER-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *    SCENARIO #11 ABNORMAL TERMINATION S0C7                     *
      *****************************************************************

       P85800-IMS-S0C7.

            MOVE WS-CDT-D-YEAR          TO WMF-CURRENT-YEAR.
            MOVE HIGH-VALUES            TO WMF-CURRENT-YEAR-R.

            COMPUTE WMF-MAX-YEAR =  WMF-CURRENT-YEAR + 1.

       P85800-IMS-S0C7-EXIT.
           EXIT.


      ***************************************************************** 13370000
      *                                                               * 13380000
      *    PARAGRAPH:  P90000-GU-CUSTOMER                             * 13390000
      *                                                               * 13400000
      *    FUNCTION :  ROUTINE TO READ A CUSTOMER ROOT SEGMENT
      *                CUSTMR                                         * 13420000
      *                                                               * 13430000
      *                                                               * 13450000
      ***************************************************************** 13460000
                                                                        13470000
       P90000-GU-CUSTOMER.                                              13480000
                                                                        13490000
           CALL 'CBLTDLI' USING                                         13500000
                          ICF-GU
                          CUST-PCB
                          CUSTOMER-SEGMENT
                          CUSTOMER-SSA-QUAL
           END-CALL.                                                    13550000
                                                                        13580000
           IF CUST-STATUS              = SPACES OR 'GE' OR 'GB'         13590000
               NEXT SENTENCE                                            13600000
           ELSE                                                         13610000
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE             13620000
               MOVE 'LGBAT003'         TO WPIE-PROGRAM-ID               13630000
               MOVE 'P90000'           TO WPIE-PARAGRAPH
               MOVE CUST-STATUS        TO WPIE-STATUS-CODE              13650000
               MOVE 'GU'               TO WPIE-FUNCTION-CODE            13660000
               MOVE 'CUSTMR'           TO WPIE-SEGMENT-NAME             13670000
               MOVE 'CUSTMRDB'         TO WPIE-DATABASE-NAME            13680000
               MOVE 'GU CUSTMR SEGMENT'
                                       TO WPIE-COMMAND                  13690000
               PERFORM  P99500-PDA-ERROR                                13700000
                   THRU P99500-PDA-ERROR-EXIT                           13700000
           END-IF.                                                      13710000
                                                                        13720000
       P90000-GU-CUSTOMER-EXIT.
           EXIT.                                                        13740000
           EJECT                                                        13750000

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P90200-ISRT-ORDER                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INSERT AN ORDER ROOT SEGMENT        * 13410000
      *                                                               * 13430000
      *                                                               * 13450000
      ***************************************************************** 13460000
                                                                        13470000
       P90200-ISRT-CUSTOMER.

           CALL 'CBLTDLI' USING
                          ICF-ISRT
                          CUST-PCB
                          CUSTOMER-SEGMENT
                          CUSTOMER-SSA-UNQUAL
           END-CALL.


           IF CUST-STATUS              = SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'LGBAT003'         TO WPIE-PROGRAM-ID
               MOVE 'P80200'           TO WPIE-PARAGRAPH
               MOVE CUST-STATUS        TO WPIE-STATUS-CODE
               MOVE 'ISRT'             TO WPIE-FUNCTION-CODE
               MOVE 'CUSTMR'           TO WPIE-SEGMENT-NAME
               MOVE 'CUSTMRDB'         TO WPIE-DATABASE-NAME
               MOVE 'ISRT CUSTOMER SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.

       P90200-ISRT-CUSTOMER-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P90400-INIT-CUSTOMER-DATA                      *
      *                                                               *
      *    FUNCTION :  INITIALIZE CUSTOMER DATA                       *
      *                                                               *
      *****************************************************************

       P90400-INIT-CUSTOMER-DATA.

           MOVE 'N'      TO WS-END-OF-DATABASE-SW.

           PERFORM  P90500-DELETE-CUSTOMER
               THRU P90500-DELETE-CUSTOMER-EXIT
                   UNTIL END-OF-DATABASE.


       P90400-INIT-CUSTOMER-DATA-EXIT.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P90500-DELETE-CUSTOMER                         *
      *                                                               *
      *    FUNCTION :  REMOVE ALL CUSTOMER SEGMENTS                   *
      *                                                               *
      *****************************************************************
      *

       P90500-DELETE-CUSTOMER.


           CALL 'CBLTDLI' USING
                          ICF-GHU
                          CUST-PCB
                          CUSTOMER-SEGMENT
                          CUSTOMER-SSA-UNQUAL
           END-CALL.


           IF CUST-STATUS =  'GB' OR 'GE'
               MOVE 'Y'   TO WS-END-OF-DATABASE-SW
               GO TO P90500-DELETE-CUSTOMER-EXIT.


           IF CUST-STATUS =   SPACES
               CALL 'CBLTDLI' USING
                              ICF-DLET
                              CUST-PCB
                              CUSTOMER-SEGMENT
               GO TO P90500-DELETE-CUSTOMER-EXIT
           END-IF.



      *****************************************************************
      *    OTHERWISE IMS ERRORHAS OCCURRED, FORMAT ERROR MESSAGES,         *
      *    TERMINATE PROGRAM                                          *
      *****************************************************************

           MOVE 'IMS'             TO WS-PDA-ERROR-TYPE.
           MOVE 'LGBAT003'        TO WPIE-PROGRAM-ID.
           MOVE 'P90500'          TO WPIE-PARAGRAPH.
           MOVE CUST-STATUS       TO WPIE-STATUS-CODE.
           MOVE 'DLET'            TO WPIE-FUNCTION-CODE.
           MOVE 'CUSTMR'          TO WPIE-SEGMENT-NAME.
           MOVE 'CUSTMRDB'        TO WPIE-DATABASE-NAME.
           MOVE 'DELETE CUSTOMER DATABASE' TO WPIE-COMMAND.
           PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P90500-DELETE-CUSTOMER-EXIT.
           EXIT.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P90600-INSERT-CUSTOMER                         *
      *                                                               *
      *    FUNCTION :  CREATE CUSTOMER SEGMENTS                       *
      *                                                               *
      *****************************************************************
      *

       P90600-INSERT-CUSTOMER.

           ADD 1                      TO WS-CUSTOMER-START-KEY.
           MOVE WS-CUSTOMER-START-KEY TO CUST-NUM-KEY.


           CALL 'CBLTDLI' USING
                          ICF-ISRT
                          CUST-PCB
                          CUSTOMER-SEGMENT
                          CUSTOMER-SSA-UNQUAL
           END-CALL.


           IF CUST-STATUS =  SPACES
               GO TO P90600-INSERT-CUSTOMER-EXIT.


      *****************************************************************
      *    OTHERWISE IMS HAS OCCURRED, FORMAT ERROR MESSAGES,         *
      *    TERMINATE PROGRAM                                          *
      *****************************************************************

           MOVE 'IMS'             TO WS-PDA-ERROR-TYPE.
           MOVE 'LGBAT003'        TO WPIE-PROGRAM-ID.
           MOVE 'P90600'          TO WPIE-PARAGRAPH.
           MOVE CUST-STATUS       TO WPIE-STATUS-CODE.
           MOVE 'ISRT'            TO WPIE-FUNCTION-CODE.
           MOVE 'CUSTMR'          TO WPIE-SEGMENT-NAME.
           MOVE 'CUSTMRDB'        TO WPIE-DATABASE-NAME.
           MOVE 'ADD CUSTOMER DATABASE' TO WPIE-COMMAND.
           PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P90600-INSERT-CUSTOMER-EXIT.
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
                                                                        00636300
           IF PDA-DB2-ERROR                                             00636400
               MOVE WS-PDA-DB2-ERROR-01                                 00636500
                                       TO WPEA-ERROR-07-TEXT            00636600
               MOVE WS-PDA-DB2-ERROR-02                                 00636700
                                       TO WPEA-ERROR-08-TEXT            00636800
           ELSE                                                         00636900
           IF PDA-IMS-ERROR                                             00637000
               MOVE WS-PDA-IMS-ERROR-01                                 00637100
                                       TO WPEA-ERROR-07-TEXT            00637200
               MOVE WS-PDA-IMS-ERROR-02                                 00637300
                                       TO WPEA-ERROR-08-TEXT            00637400
           ELSE                                                         00637500
           IF PDA-MQSERIES-ERROR                                        00637602
               MOVE WS-PDA-MQSERIES-ERROR-01                            00637702
                                       TO WPEA-ERROR-07-TEXT            00637802
               MOVE WS-PDA-MQSERIES-ERROR-02                            00637902
                                       TO WPEA-ERROR-08-TEXT            00638002
           ELSE                                                         00638102
               MOVE WS-PDA-GEN-ERROR-01                                 00638200
                                       TO WPEA-ERROR-07-TEXT            00638300
               MOVE WS-PDA-GEN-ERROR-02                                 00638400
                                       TO WPEA-ERROR-08-TEXT.           00638500
                                                                        00638600
           DISPLAY WPEA-ERROR-07.
           DISPLAY WPEA-ERROR-08.
                                                                        00638600
           DISPLAY WPEA-ERROR-09.
           DISPLAY WPEA-ERROR-10.
           DISPLAY ' '.

           DISPLAY WPM-BLANK.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY WPM-END-PROGRAM.
           DISPLAY WPM-ALL-ASTERISK.


           CALL 'SNAPAID'.


      **** MOVE 99                     TO WS-RETURN-CODE.
      **** CALL 'ILBOABN0'          USING WS-RETURN-CODE.
      **** MOVE WS-RETURN-CODE         TO RETURN-CODE.
      **** GOBACK.

           GOBACK.


       P99500-PDA-ERROR-EXIT.
           EXIT.
           EJECT
