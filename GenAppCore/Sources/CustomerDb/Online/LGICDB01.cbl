      ******************************************************************
      *                                                                *
      * LICENSED MATERIALS - PROPERTY OF IBM                           *
      *                                                                *
      * "RESTRICTED MATERIALS OF IBM"                                  *
      *                                                                *
      * CB12                                                           *
      *                                                                *
      * (C) COPYRIGHT IBM CORP. 2011, 2013 ALL RIGHTS RESERVED         *
      *                                                                *
      * US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,      *
      * OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE                   *
      * CONTRACT WITH IBM CORPORATION                                  *
      *                                                                *
      *                                                                *
      *                    Inquire Customer                            *
      *                                                                *
      * Select customer details from DB2 table                         *
      *                                                                *
      *                                                                *
      ******************************************************************
      *             PROGRAM CHANGE LOG COMPUWARE                       *
      *             ----------------------------                       *
      *                                                                *
      *  DATE       UPDATED BY            CHANGE DESCRIPTION           *
      *  --------   --------------------  --------------------------   *
      *  02/12/16   P BARON               ADDED SCENARIO PROCESSING    *
      *                                   INEFFICIENT DB2 QUERY        *
      *                                                                *
      *  07/29/16   P BARON               ADDED CUSTOMER SCROLLING     *
      *                                   SCROLL FORWARD  (PF8)        *
      *                                   SCROLL BACKWARD (PF7)        *
      *                                                                *
      *  01/16/17   P BARON               ADDED SCENARIO PROCESSING    *
      *                                   DB2 -303 INCOMPATABLE        *
      *                                   COLUMNS BETWEEN DB2 TABLE    *
      *                                   AND A FETCH COLUMN VARIABLE  *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGICDB01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.

       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*
      * Run time (debug) infomation for this invocation
        01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'LGICDB01------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-FILLER                PIC X.
           03 WS-ADDR-DFHCOMMAREA      USAGE is POINTER.
           03 WS-CALEN                 PIC S9(4) COMP.

      * Variables for time/date processing
       01  WS-ABSTIME                  PIC S9(8) COMP VALUE +0.
       01  WS-TIME                     PIC X(8)  VALUE SPACES.
       01  WS-DATE                     PIC X(10) VALUE SPACES.

      * Miscellaneous variables
       01  WS-NAME-MIN-VALUE           PIC X(01) Value SPACES.
       01  FROM-SCROLLING              PIC X(01) Value 'N'.

      * Error Message structure
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' LGICUS01'.
           03 EM-VARIABLE.
             05 FILLER                 PIC X(6)  VALUE ' CNUM='.
             05 EM-CUSNUM              PIC X(10)  VALUE SPACES.
             05 FILLER                 PIC X(6)  VALUE ' PNUM='.
             05 EM-POLNUM              PIC X(10)  VALUE SPACES.
             05 EM-SQLREQ              PIC X(16) VALUE SPACES.
             05 FILLER                 PIC X(9)  VALUE ' SQLCODE='.
             05 EM-SQLRC               PIC +9(5) USAGE DISPLAY.

       01 CA-ERROR-MSG.
           03 FILLER                PIC X(9)  VALUE 'COMMAREA='.
           03 CA-DATA               PIC X(90) VALUE SPACES.
      *----------------------------------------------------------------*
      * Fields to be used to calculate if commarea is large enough
       01  WS-COMMAREA-LENGTHS.
           03 WS-CA-HEADERTRAILER-LEN  PIC S9(4) COMP VALUE +18.
           03 WS-REQUIRED-CA-LEN       PIC S9(4)      VALUE +0.

      *----------------------------------------------------------------*
      * Definitions required by SQL statement                          *
      *   DB2 datatypes to COBOL equivalents                           *
      *     SMALLINT    :   PIC S9(4) COMP                             *
      *     INTEGER     :   PIC S9(9) COMP                             *
      *     BIGINT      :   PIC S9(18) COMP                            *
      *     DATE        :   PIC X(10)                                  *
      *     TIMESTAMP   :   PIC X(26)                                  *
      *----------------------------------------------------------------*
      * Host variables for input to DB2 integer types
      *01  DB2-IN-INTEGERS
       01 DB2-CUSTOMERNUMBER-INT          PIC S9(9)  COMP.
       01 DB2-CUSTOMERNUMBER-INT-1        PIC S9(9)  COMP.
       01 DB2-CUSTOMERNUMBER-BIGINT       PIC S9(18) COMP.
       01 DB2-CUSTOMERNUMBER-BEG-BIGINT   PIC S9(18) COMP.
       01 DB2-CUSTOMERNUMBER-END-BIGINT   PIC S9(18) COMP.


      *----------------------------------------------------------------*
      *    DB2 CONTROL
      *----------------------------------------------------------------*
      * SQLCA DB2 communications area
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

      *  Must be an SQL INCLUDE so available to SQL pre-compiler
           EXEC SQL
               INCLUDE LGPOLICY
           END-EXEC.


      *----------------------------------------------------------------*
      * Declare Cursors
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * SCENARIO PROCESSING (GENAPP) INEFFICIENT DB2 SQL QUERY
      *----------------------------------------------------------------*
           EXEC SQL
             DECLARE Cust_Cursor Insensitive Scroll Cursor For
             SELECT    FIRSTNAME,
                       LASTNAME,
                       DATEOFBIRTH,
                       HOUSENAME,
                       HOUSENUMBER,
                       POSTCODE,
                       PHONEMOBILE,
                       PHONEHOME,
                       EMAILADDRESS
             FROM      CUSTOMER
             WHERE     LASTNAME  > :WS-NAME-MIN-VALUE   AND
                       FIRSTNAME > :WS-NAME-MIN-VALUE   AND
                       CUSTOMERNUMBER < 10
             ORDER BY  LASTNAME, HOUSENAME, EMAILADDRESS
           END-EXEC.


      ******************************************************************
      *    L I N K A G E     S E C T I O N
      ******************************************************************
       LINKAGE SECTION.

       01  DFHCOMMAREA.
           EXEC SQL
             INCLUDE LGCMAREA
           END-EXEC.

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
      **PWB**MAINLINE*SECTION.

      *----------------------------------------------------------------*
      * Common code                                                    *
      *----------------------------------------------------------------*
      * initialize working storage variables
      *    INITIALIZE WS-HEADER.
      * set up general variable
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      * Check commarea and obtain required details                     *
      *----------------------------------------------------------------*
      * If NO commarea received issue an ABEND
           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO EM-VARIABLE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('LGCA') NODUMP END-EXEC
           END-IF

      * initialize commarea return code to zero
           MOVE '00' TO CA-RETURN-CODE
           MOVE EIBCALEN TO WS-CALEN.
           SET WS-ADDR-DFHCOMMAREA TO ADDRESS OF DFHCOMMAREA.

      * initialize DB2 host variables
      *    INITIALIZE DB2-IN-INTEGERS.

      *----------------------------------------------------------------*
      * Process incoming commarea                                      *
      *----------------------------------------------------------------*
      * check commarea length - meets minimum requirement
           MOVE WS-CUSTOMER-LEN        TO WS-REQUIRED-CA-LEN
           ADD WS-CA-HEADERTRAILER-LEN TO WS-REQUIRED-CA-LEN
      * if less set error return code and return to caller
           IF EIBCALEN IS LESS THAN WS-REQUIRED-CA-LEN
             MOVE '98' TO CA-RETURN-CODE
             EXEC CICS RETURN END-EXEC
           END-IF

      * Convert commarea customer number to DB2 integer format
           MOVE CA-CUSTOMER-NUM TO DB2-CUSTOMERNUMBER-INT
           MOVE CA-CUSTOMER-NUM TO DB2-CUSTOMERNUMBER-BIGINT
      * and save in error msg field incase required
           MOVE CA-CUSTOMER-NUM TO EM-CUSNUM

      *----------------------------------------------------------------*
      * SCENARIO PROCESSING (GENAPP) EXECUTE INEFFICIENT DB2 QUERY     *
      *----------------------------------------------------------------*

           MOVE '00' TO CA-RETURN-CODE
           PERFORM GET-CUSTOMER-INFO-1

      *****IF (CA-CUSTOMER-NUM  = 0001000144)
      ******   MOVE 'N'  TO FROM-SCROLLING
      ******   PERFORM GET-CUSTOMER-INFO-1
      ******   MOVE '00' TO CA-RETURN-CODE
      ******   GO TO MAINLINE-EXIT
      *****END-IF


      *----------------------------------------------------------------*
      * Check for CUSTOMER SCROLL, PF7=SCROLL BACK, PF8=SCROLL FORWARD *
      *----------------------------------------------------------------*
      *
            IF CA-CUSTOMER-PFKEY = '07' OR '08'
               PERFORM GET-CUSTOMER-BEGEND-VAL
            END-IF


           IF CA-CUSTOMER-PFKEY = '07'
              MOVE 'Y'  TO FROM-SCROLLING
              PERFORM SCROLL-BACKWARD-PROCESS
              MOVE SPACES TO CA-CUSTOMER-PFKEY
              MOVE 'N'  TO FROM-SCROLLING
           END-IF


           IF CA-CUSTOMER-PFKEY = '08'
              MOVE 'Y'  TO FROM-SCROLLING
              PERFORM SCROLL-FORWARD-PROCESS
              MOVE SPACES TO CA-CUSTOMER-PFKEY
              MOVE 'N'  TO FROM-SCROLLING
           END-IF


      *----------------------------------------------------------------*
      * Obtain details from DB2                                        *
      *----------------------------------------------------------------*
      *    Call routine to issue SQL to obtain info from DB2
           PERFORM GET-CUSTOMER-INFO.

      *----------------------------------------------------------------*
      * END PROGRAM and return to caller                               *
      *----------------------------------------------------------------*
      *****PWB***MAINLINE-END.

      ****End-Program.
       End-Program.
           EXEC CICS RETURN END-EXEC.

       MAINLINE-EXIT.
           EXIT.


      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      * OBTAIN BEGIN AND END CUSTOMER VALUES
      *----------------------------------------------------------------*
      *
       GET-CUSTOMER-BEGEND-VAL.

           MOVE ZEROES  TO CA-CUSTOMER-BEGIN
                           CA-CUSTOMER-END.

      *----------------------------------------------------------------*
      *        BEGIN CUSTOMER VALUE
      *----------------------------------------------------------------*
           EXEC SQL
               SELECT MIN(CUSTOMERNUMBER)
               INTO  :DB2-CUSTOMERNUMBER-BEG-BIGINT
               FROM  CUSTOMER
               GROUP BY CUSTOMERNUMBER
               ORDER BY CUSTOMERNUMBER ASC FETCH FIRST 1 ROWS ONLY
           END-EXEC


           Evaluate SQLCODE
              When 0
                 MOVE '00'    TO CA-RETURN-CODE
                 MOVE DB2-CUSTOMERNUMBER-BEG-BIGINT
                                                 TO CA-CUSTOMER-BEGIN
              When 100
                 MOVE '01' TO  CA-RETURN-CODE
              When -913
                 MOVE '01' TO CA-RETURN-CODE
              When Other
                 MOVE '90' TO CA-RETURN-CODE
                 PERFORM WRITE-ERROR-MESSAGE
                 EXEC CICS RETURN END-EXEC
           END-Evaluate.


           IF SQLCODE NOT = 0
               GO TO GET-CUSTOMER-BEGEND-VAL-EXIT.


      *----------------------------------------------------------------*
      *        END CUSTOMER VALUE
      *----------------------------------------------------------------*

           EXEC SQL
               SELECT MAX(CUSTOMERNUMBER)
               INTO  :DB2-CUSTOMERNUMBER-END-BIGINT
               FROM  CUSTOMER
               GROUP BY CUSTOMERNUMBER
               ORDER BY CUSTOMERNUMBER DESC FETCH FIRST 1 ROWS ONLY
           END-EXEC


           Evaluate SQLCODE
              When 0
                 MOVE '00' TO CA-RETURN-CODE
                 MOVE DB2-CUSTOMERNUMBER-END-BIGINT
                                               TO CA-CUSTOMER-END
              When 100
                 MOVE '01' TO  CA-RETURN-CODE
              When -913
                 MOVE '01' TO CA-RETURN-CODE
              When Other
                    MOVE '90' TO CA-RETURN-CODE
                    PERFORM WRITE-ERROR-MESSAGE
                    EXEC CICS RETURN END-EXEC
           END-Evaluate.


       GET-CUSTOMER-BEGEND-VAL-EXIT.
           EXIT.



      *----------------------------------------------------------------*
      * Check for CUSTOMER SCROLL, PF7=SCROLL BACKWARD                 *
      *----------------------------------------------------------------*
      *
       SCROLL-BACKWARD-PROCESS.

      *****MOVE 'Y' TO FROM-SCROLLING.
           MOVE CA-CUSTOMER-NUM TO DB2-CUSTOMERNUMBER-INT-1.


           IF CA-CUSTOMER-NUM   =  ZEROES
               EXEC SQL
                  SELECT MAX(CUSTOMERNUMBER)
                  INTO  :DB2-CUSTOMERNUMBER-INT
                  FROM  CUSTOMER
               END-EXEC

           ELSE

               EXEC SQL
                  SELECT CUSTOMERNUMBER
                  INTO  :DB2-CUSTOMERNUMBER-INT
                  FROM  CUSTOMER
                  WHERE CUSTOMERNUMBER < :DB2-CUSTOMERNUMBER-INT-1
                  ORDER BY CUSTOMERNUMBER DESC FETCH FIRST 1 ROWS ONLY
               END-EXEC.


           Evaluate SQLCODE
             When 0
               MOVE '00' TO CA-RETURN-CODE
               MOVE DB2-CUSTOMERNUMBER-INT  TO  CA-CUSTOMER-NUM
      ******** IF (CA-CUSTOMER-NUM  = 0001000144)
      ********     MOVE 'Y' TO FROM-SCROLLING
      ******** END-IF
             When 100
               MOVE '00'    TO  CA-RETURN-CODE
               MOVE ZEROES  TO  CA-CUSTOMER-NUM
             When -913
               MOVE '01' TO CA-RETURN-CODE
             When Other
               MOVE '90' TO CA-RETURN-CODE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-Evaluate.

           EXIT.


      *----------------------------------------------------------------*
      * Check for CUSTOMER SCROLL, PF8=SCROLL FORWARD                  *
      *----------------------------------------------------------------*
      *
       SCROLL-FORWARD-PROCESS.


      **** MOVE 'Y' TO FROM-SCROLLING.
           MOVE CA-CUSTOMER-NUM TO DB2-CUSTOMERNUMBER-INT-1.


           EXEC SQL
               SELECT CUSTOMERNUMBER
               INTO  :DB2-CUSTOMERNUMBER-INT
               FROM  CUSTOMER
               WHERE CUSTOMERNUMBER > :DB2-CUSTOMERNUMBER-INT-1
               ORDER BY CUSTOMERNUMBER FETCH FIRST 1 ROWS ONLY
           END-EXEC.


           Evaluate SQLCODE
             When 0
               MOVE '00' TO CA-RETURN-CODE
               MOVE DB2-CUSTOMERNUMBER-INT  TO  CA-CUSTOMER-NUM
      *******  IF (CA-CUSTOMER-NUM  = 0001000144)
      *******      MOVE 'Y' TO FROM-SCROLLING
      *******  END-IF
             When 100
               MOVE '00'    TO  CA-RETURN-CODE
               MOVE ZEROES  TO  CA-CUSTOMER-NUM
             When -913
               MOVE '01' TO CA-RETURN-CODE
             When Other
               MOVE '90' TO CA-RETURN-CODE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-Evaluate.

           EXIT.



       GET-CUSTOMER-INFO.

           EXEC SQL
               SELECT FIRSTNAME,
                      LASTNAME,
                      DATEOFBIRTH,
                      HOUSENAME,
                      HOUSENUMBER,
                      POSTCODE,
                      PHONEMOBILE,
                      PHONEHOME,
                      EMAILADDRESS,
                      NATIONALIDNBR,
                      NINTYPE,
                      STREETADDRESS,
                      CITY,
                      STATE,
                      COUNTRYCODE,
                      SALESTERRITORY
               INTO  :CA-FIRST-NAME,
                     :CA-LAST-NAME,
                     :CA-DOB,
                     :CA-HOUSE-NAME,
                     :CA-HOUSE-NUM,
                     :CA-POSTCODE,
                     :CA-PHONE-MOBILE,
                     :CA-PHONE-HOME,
                     :CA-EMAIL-ADDRESS,
                     :CA-NATIONAL-ID-NBR,
                     :CA-NIN-TYPE,
                     :CA-STREET-ADDRESS,
                     :CA-CITY,
                     :CA-STATE,
                     :CA-COUNTRY-CODE,
                     :CA-SALES-TERRITORY
               FROM CUSTOMER
               WHERE CUSTOMERNUMBER = :DB2-CUSTOMERNUMBER-BIGINT
           END-EXEC.

           Evaluate SQLCODE
             When 0
               MOVE '00' TO CA-RETURN-CODE
             When 100
               MOVE '01' TO CA-RETURN-CODE
             When -913
               MOVE '01' TO CA-RETURN-CODE
             When Other
               MOVE '90' TO CA-RETURN-CODE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-Evaluate.

           EXIT.




      *================================================================*
      * SCENARIO PROCESSING (GENAPP) Inefficient DB2 Query             *
      * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   *
      *                                                                *
      *================================================================*
      *
       GET-CUSTOMER-INFO-1.

           MOVE ' SELECT Customer-1 ' TO EM-SQLREQ.

           EXEC SQL
             OPEN Cust_Cursor
           END-EXEC.

           IF SQLCODE NOT EQUAL 0
             MOVE '89' TO CA-RETURN-CODE
             PERFORM WRITE-ERROR-MESSAGE
             PERFORM END-PROGRAM
           END-IF.


           Perform GET-CUSTOMER-INFO-1-FETCH
                     Until SQLCODE NOT = 0.


           EXEC SQL
             Close Cust_Cursor
           END-EXEC

           IF SQLCODE NOT EQUAL 0
             MOVE '01' TO CA-RETURN-CODE
             PERFORM WRITE-ERROR-MESSAGE
             PERFORM END-PROGRAM
           END-IF.

           EXIT.


       GET-CUSTOMER-INFO-1-FETCH.

           MOVE ' FETCH  Customer-1 ' TO EM-SQLREQ.

           MOVE 0  TO SQLCODE.

      ********* -303 DB2 SQLCODE ***** DB2-CUSTOMERNUMBER-INT

           IF (CA-CUSTOMER-PFKEY     = '07' OR '08')
               MOVE 'Y'              TO FROM-SCROLLING
           ELSE
               MOVE 'N'              TO FROM-SCROLLING.


           IF (CA-CUSTOMER-NUM       = 0001000021)
               IF (FROM-SCROLLING    = 'N')

                   EXEC SQL
                      Fetch Cust_Cursor
                      INTO
                          :DB2-FIRSTNAME,
                          :DB2-LASTNAME,
                          :DB2-CUSTOMERNUMBER-INT,
                          :DB2-HOUSENAME,
                          :DB2-HOUSENUMBER,
                          :DB2-POSTCODE,
                          :DB2-PHONE-MOBILE,
                          :DB2-PHONE-HOME,
                          :DB2-EMAIL-ADDRESS
                   END-EXEC

               ELSE

                   EXEC SQL
                      Fetch Cust_Cursor
                      INTO
                          :DB2-FIRSTNAME,
                          :DB2-LASTNAME,
                          :DB2-DATEOFBIRTH,
                          :DB2-HOUSENAME,
                          :DB2-HOUSENUMBER,
                          :DB2-POSTCODE,
                          :DB2-PHONE-MOBILE,
                          :DB2-PHONE-HOME,
                          :DB2-EMAIL-ADDRESS

                   END-EXEC

           ELSE
                   EXEC SQL
                   Fetch Cust_Cursor
                   INTO
                       :DB2-FIRSTNAME,
                       :DB2-LASTNAME,
                       :DB2-DATEOFBIRTH,
                       :DB2-HOUSENAME,
                       :DB2-HOUSENUMBER,
                       :DB2-POSTCODE,
                       :DB2-PHONE-MOBILE,
                       :DB2-PHONE-HOME,
                       :DB2-EMAIL-ADDRESS
                   END-EXEC.


           Evaluate SQLCODE
             When 0
               MOVE '00' TO CA-RETURN-CODE
             When 100
               MOVE '01' TO CA-RETURN-CODE
             When -913
               MOVE '01' TO CA-RETURN-CODE
             When -303

               IF (FROM-SCROLLING    = 'N')
                  MOVE '01' TO CA-RETURN-CODE
                  EXEC CICS DUMP
                            TRANSACTION
                            DUMPCODE('DB2E')
                            COMPLETE
                            NOHANDLE
                  END-EXEC
               END-IF


      ******** EXEC CICS ABEND
      ********           ABCODE('DB2E')
      ********           NODUMP
      ******** END-EXEC


             When Other
               MOVE '90' TO CA-RETURN-CODE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-Evaluate.

           EXIT.


      *================================================================*
      * Procedure to write error message to Queues                     *
      *   message will include Date, Time, Program Name, Customer      *
      *   Number, Policy Number and SQLCODE.                           *
      *================================================================*
       WRITE-ERROR-MESSAGE.
      * Save SQLCODE in message
           MOVE SQLCODE TO EM-SQLRC
      * Obtain and format current time and date
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     MMDDYYYY(WS-DATE)
                     TIME(WS-TIME)
           END-EXEC
           MOVE WS-DATE TO EM-DATE
           MOVE WS-TIME TO EM-TIME
      * Write output message to TDQ
           EXEC CICS LINK PROGRAM('LGSTSQ')
                     COMMAREA(ERROR-MSG)
                     LENGTH(LENGTH OF ERROR-MSG)
           END-EXEC.
      * Write 90 bytes or as much as we have of commarea to TDQ
           IF EIBCALEN > 0 THEN
             IF EIBCALEN < 91 THEN
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CA-DATA
               EXEC CICS LINK PROGRAM('LGSTSQ')
                         COMMAREA(CA-ERROR-MSG)
                         LENGTH(LENGTH OF CA-ERROR-MSG)
               END-EXEC
             ELSE
               MOVE DFHCOMMAREA(1:90) TO CA-DATA
               EXEC CICS LINK PROGRAM('LGSTSQ')
                         COMMAREA(CA-ERROR-MSG)
                         LENGTH(LENGTH OF CA-ERROR-MSG)
               END-EXEC
             END-IF
           END-IF.
           EXIT.
