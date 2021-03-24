       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPGEN01.

      *****************************************************************
      *                 GENNAPP DEMONSTRATION APPLICATION             *
      *                       CUSTOMIZED FOR AND BY                   *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   SPGEN01                                           *
      *                                                               *
      * FUNCTION:   PROGRAM SPGEN01 IS A BATCH PROGRAM USED FOR       *
      *             PRODUCT DEMONSTRATION PURPOSES. THE APPLICATION   *
      *             IS THE IBM GENAPP APPLICATION.                    *
      *                                                               *
      *             SPGEN01 IS A MAIN MODULE AND IS A DB2 STORED      *
      *             PROCEDURE INVOKED BY BATCH PROGRAM LGBAT001       *
      *                                                               *
      *                                                               *
      *                                                               *
      * FILES   :   CUSTOMER              (DB2)                       *
      *
      *                                                               *
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      * DATE        UPDATED BY            CHANGE DESCRIPTION          *
      * ----------  --------------------  --------------------------  *
      * 06/16/2017                        INITIAL DEVELOPMENT         *
      *                                                               *
      * MM/DD/YYYY  XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                               *
      *****************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           EJECT
       DATA DIVISION.
       FILE SECTION.

      *****************************************************************
      *    FILE DECLARATIONS                                          *
      *****************************************************************

           EJECT


       WORKING-STORAGE SECTION.

      *****************************************************************
      *    77 LEVEL DATA ITEMS HERE  (SUBSCRIPTS, INDEXES ETC.)       *
      *****************************************************************
       77  WS-SUB                      PIC S9(04)  COMP   VALUE +0.


      *****************************************************************
      *    STORED PROCEDURE INPUT / OUTPUT VARIABLES                  *
      *****************************************************************
      *01  SP-IN-CUSTOMER         PIC S9(10) USAGE BINARY VALUE +0.
      *01  SP-OUT-MAKE            PIC X(15)         VALUE SPACES.
      *01  SP-OUT-MODEL           PIC X(15)         VALUE SPACES.
      *01  SP-OUT-CAR-YEAR        PIC X(04)         VALUE SPACES.
      *01  SP-OUT-PREMIUM         PIC S9(10) USAGE BINARY VALUE +0.
      *****************************************************************
      *****************************************************************


      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.

           05  WS-ERROR-FOUND-SW         PIC X(01)           VALUE 'N'.
               88  ERROR-FOUND                               VALUE 'Y'.
               88  NO-ERROR-FOUND                            VALUE 'N'.

           EJECT

      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************


       01  WS-RETURN-CODE PICTURE S9(9) USAGE BINARY VALUE ZERO.
       01  WS-TIMING      PICTURE S9(9) USAGE BINARY VALUE ZERO.


       01  WS-MISCELLANEOUS-FIELDS.
           05  WMF-NAME-MIN-VALUE           PIC X(01) VALUE SPACES.


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


       01  ABEND-S0C7-RELATED-FIELDS.
           05  WMF-CURRENT-YEAR        PIC 9(5) COMP-3  VALUE 0.
           05  WMF-CURRENT-YEAR-R      REDEFINES WMF-CURRENT-YEAR
                                       PIC X(3).
           05  WMF-MAX-YEAR            PIC 9(4).

           EJECT


      *****************************************************************
      *    DB2  DEFINITIONS                                           *
      *****************************************************************

      *****************************************************************
      *         SQL COMMUNICATIONS AREA                               *
      *****************************************************************

           EXEC SQL INCLUDE SQLCA END-EXEC.
           EJECT

      *****************************************************************
      *         CUSTOMER TABLE                   -- DCLGEN DCUSTOMR   *
      *****************************************************************

           EXEC SQL
               INCLUDE DCUSTOMR
           END-EXEC.
           EJECT

      ****************************************************************
      * DECLARE CURSORS                                              *
      ****************************************************************

      ****************************************************************
      * SCENARIO PROCESSING (GENAPP) INEFFICIENT DB2 SQL QUERY       *
      ****************************************************************
           EXEC SQL
              DECLARE CUST_CURSOR CURSOR FOR
              SELECT LASTNAME
              FROM   GENAPPDB.CUSTOMER
              WHERE  CUSTOMERNUMBER > 10
              ORDER BY  LASTNAME
           END-EXEC.


      ****************************************************************
      * IBM DSNTIAR DEFINITIONS                                      *
      ****************************************************************

       01  DSNTIAR-ERROR-MESSAGE.
              02  DSNTIAR-ERROR-LEN   PIC S9(4)  COMP VALUE +1320.
              02  DSNTIAR-ERROR-TEXT  PIC X(132) OCCURS 10 TIMES
                                          INDEXED BY ERROR-INDEX.

       77  DSNTIAR-ERROR-TEXT-LEN     PIC S9(9)  COMP VALUE +132.



      *****************************************************************
      *    GENERAL ERROR PROCESSING WORK AREAS                        *
      *****************************************************************
      ******************************************************************
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
      *    FORMATTED ERROR LINES                                       *
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
      *    PDA DB2 ERROR LINES                                         *
      ******************************************************************

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

       01  WS-SPGEN01-MESSAGES.

           05  WPM-BLANK               PIC X(01)       VALUE     ' '.
           05  WPM-ALL-ASTERISK        PIC X(80)       VALUE ALL '*'.

           05  WPM-BEGIN-PROGRAM.
               10 FILLER               PIC X(78)   VALUE
                  '***** BEGIN PROGRAM SPGEN01 *****'.

           05  WPM-END-PROGRAM.
               10 FILLER               PIC X(78)   VALUE
                  '***** END PROGRAM SPGEN01 *****'.

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

       01  LS-SPGEN01-USERID               PIC X(08).

       01  LS-SPGEN01-ACTIVE-SCENARIOS     PIC X(250).
       01  LS-SPGEN01-ACTIVE-SCENARIOS-R   REDEFINES
           LS-SPGEN01-ACTIVE-SCENARIOS.
           05 LS-SCENARIOS                 PIC X(01) OCCURS 250 TIMES.

       01  LS-SPGEN01-STATUS               PIC X(04).


      *****************************************************************
      *    P R O C E D U R E    D I V I S I O N                       *
      *****************************************************************

       PROCEDURE DIVISION USING  LS-SPGEN01-USERID,
                                 LS-SPGEN01-ACTIVE-SCENARIOS,
                                 LS-SPGEN01-STATUS.


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

           DISPLAY 'LS-SPGEN01-USERID ' LS-SPGEN01-USERID.
           DISPLAY 'LS-SPGEN01-STATUS ' LS-SPGEN01-STATUS.


           PERFORM  P00050-INITIALIZE                                   TAGGED
               THRU P00050-INITIALIZE-EXIT.                             CODE
                                                                        TESTING
                                                                        03/13/01
           PERFORM  P00500-MAIN-PROCESS
               THRU P00500-MAIN-PROCESS-EXIT.


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

      *****************************************************************
      *    OBTAIN CURRENT DATE AND TIME                               *
      *****************************************************************

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.

           MOVE ZEROES                TO LS-SPGEN01-STATUS.

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

           DISPLAY 'P00500-MAIN-PROCESS '.

      *****************************************************************
      *    PERFORM SCENARIO PROCESSING                                *
      *****************************************************************
                                                                        03/13/01
           PERFORM  P85000-PROCESS-SCENARIOS
               THRU P85000-PROCESS-SCENARIOS-EXIT.


       P00500-MAIN-PROCESS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *    PERFORM SCENARIO PROCESSING (STORED PROCEDURE ONLY)        *
      *****************************************************************

       P85000-PROCESS-SCENARIOS.

           DISPLAY 'P85000-PROCESS-SCENARIOS '.


      *****************************************************************
      *    PROCESS ACTIVATED SCENARIOS                                *
      *****************************************************************

           IF  LS-SCENARIOS (7)       = 'Y'
               PERFORM  P85100-S0C7
                   THRU P85100-S0C7-EXIT.

      *****
      *****IF  LS-SCENARIOS (8)       = 'Y'
      *****    PERFORM  P87000-INEFF-SQL
      *****        THRU P87000-INEFF-SQL-EXIT.
      *****


           IF  LS-SCENARIOS (8)       = 'Y'

               DISPLAY 'P87500-INEFF-SQL'

               PERFORM  P87500-INEFF-SQL
                   THRU P87500-INEFF-SQL-EXIT 10000 TIMES.


       P85000-PROCESS-SCENARIOS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *    SCENARIO #7                                                *
      *    PERFORM SCENARIO PROCESSING -- ABEND S0C7                  *
      *****************************************************************

       P85100-S0C7.

           DISPLAY 'P85100-PROC-S0C7 '.

           MOVE WS-CDT-D-YEAR          TO WMF-CURRENT-YEAR.
           MOVE HIGH-VALUES            TO WMF-CURRENT-YEAR-R.

           COMPUTE WMF-MAX-YEAR =  WMF-CURRENT-YEAR + 1.

       P85100-S0C7-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *    SCENARIO #8  INEFFICIENT  DB2 SQL                          *
      *****************************************************************
       P87500-INEFF-SQL.

           EXEC SQL
                SELECT LASTNAME
                INTO   :LASTNAME
                FROM   GENAPPDB.CUSTOMER
                WHERE  (CUSTOMERNUMBER > 1) AND (CUSTOMERNUMBER < 3)
           END-EXEC.


           IF SQLCODE NOT EQUAL 0
              MOVE 'DB2'         TO WS-PDA-ERROR-TYPE
              MOVE 'SPGEN01'     TO WPDE-PROGRAM-ID
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
      *    PERFORM SCENARIO PROCESSING -- DB2 INEFFICIENT SQL         *
      *****************************************************************
       P87000-INEFF-SQL.

           EXEC SQL
             OPEN CUST_CURSOR
           END-EXEC.

           IF SQLCODE NOT EQUAL 0
               MOVE 'DB2'         TO WS-PDA-ERROR-TYPE
               MOVE 'SPGEN01'     TO WPDE-PROGRAM-ID
               MOVE  SQLCODE      TO WPDE-DB2-SQLCODE
               MOVE 'OPEN CURSOR' TO WPDE-FUNCTION
               MOVE 'P87000'      TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.

      *****************************************************************

           EXEC SQL
                   FETCH CUST_CURSOR
                   INTO
                       :LASTNAME
           END-EXEC.


           IF SQLCODE = 0 OR 100
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'         TO WS-PDA-ERROR-TYPE
               MOVE 'SPGEN01'     TO WPDE-PROGRAM-ID
               MOVE  SQLCODE      TO WPDE-DB2-SQLCODE
               MOVE 'FETCH'       TO WPDE-FUNCTION
               MOVE 'P87000'      TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.

      *****************************************************************

           EXEC SQL
             CLOSE CUST_CURSOR
           END-EXEC.

           IF SQLCODE NOT EQUAL 0
               MOVE 'DB2'         TO WS-PDA-ERROR-TYPE
               MOVE 'SPGEN01'     TO WPDE-PROGRAM-ID
               MOVE  SQLCODE      TO WPDE-DB2-SQLCODE
               MOVE 'FETCH'       TO WPDE-FUNCTION
               MOVE 'P87000'      TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.


      *****************************************************************

       P87000-INEFF-SQL-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *    P R O D U C T    D E M O N S T R A T I O N     A P P L     *
      *                                                               *
      *             E R R O R    R O U T I N E S                      *
      *                                                               *
      *                                                               *
      *****************************************************************

KCS305                                                                  KCS32005
KCS305***************************************************************** KCS32005
KCS305*                                                               * KCS32005
KCS305*    PARAGRAPH:  P99400-ERROR-ROUTINE                           * KCS32005
KCS305*                                                               * KCS32005
KCS305*    FUNCTION :  ROUTINE TO FORMAT AND DISPLAY NON-FATAL ERRORS * KCS32005
KCS305*                                                               * KCS32005
KCS305*                ERROR TEXT IS DISPLAYED                        * KCS32005
KCS305*                TO THE USER INDICATING THE NATURE OF THE ERROR * KCS32005
KCS305*                                                               * KCS32005
KCS305*                CONTROL IS RETURNED TO CALLING ROUTINE         * KCS32005
KCS305*                                                               * KCS32005
KCS305*    CALLED BY:  GLOBAL                                         * KCS32005
KCS305*                                                               * KCS32005
KCS305***************************************************************** KCS32005
KCS305                                                                  KCS32005
KCS305 P99400-ERROR-ROUTINE.                                            KCS32005
KCS305                                                                  KCS32005
KCS305     MOVE 'Y'                    TO WS-ERROR-FOUND-SW.            KCS32005
KCS305                                                                  KCS32005
KCS305     DISPLAY ' '.                                                 KCS32005
KCS305     DISPLAY WPEA-ERROR-01.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-02.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-03.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-04.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-05.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-06.                                       KCS32005
KCS305                                                                  00636300
KCS305     DISPLAY WPEA-ERROR-07.                                       KCS32005
KCS305                                                                  00638600
KCS305     DISPLAY WPEA-ERROR-08.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-09.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-10.                                       KCS32005
KCS305     DISPLAY ' '.                                                 KCS32005
KCS305                                                                  KCS32005
KCS305 P99400-ERROR-ROUTINE-EXIT.                                       KCS32005
KCS305     EXIT.                                                        KCS32005
KCS305     EJECT                                                        KCS32005

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
           MOVE '0008'                 TO LS-SPGEN01-STATUS.

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
               CALL 'DSNTIAR' USING SQLCA,
                                    DSNTIAR-ERROR-MESSAGE,
                                    DSNTIAR-ERROR-TEXT-LEN

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


      **** MOVE 99                     TO WS-RETURN-CODE.
      **** CALL "CEE3ABD"              USING WS-RETURN-CODE, WS-TIMING.

           GOBACK.

      *****CALL 'ILBOABN0'          USING WS-RETURN-CODE.
      *****MOVE WS-RETURN-CODE         TO RETURN-CODE.

       P99500-PDA-ERROR-EXIT.
           EXIT.
           EJECT
