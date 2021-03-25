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
      *   To obtain Customer's details from database.                  *
      *                                                                *
      * Customer Inquire Business logic                                *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGICUS01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.

       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*
      * Run time (debug) infomation for this invocation
      *                                 VALUE 'LGICUS01------WS'.

        77  WS-CUSTOMER-SUB            PIC S9(4) VALUE +0  COMP.

        01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'LGICUS01------WS'.
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

       01 LGICDB01                  PIC x(8) Value 'LGICDB01'.
       01  ATRANID                     PIC X(4)       VALUE 'DSC1'.
      *----------------------------------------------------------------*
      * Fields to be used to calculate if commarea is large enough
       01  WS-COMMAREA-LENGTHS.
           03 WS-CA-HEADERTRAILER-LEN  PIC S9(4) COMP VALUE +18.
           03 WS-REQUIRED-CA-LEN       PIC S9(4)      VALUE +0.

       01  WS-Resp                     PIC S9(8) Comp.
       01  MQ-Hit                      PIC S9(4).
       01  MQ-Read-Record              PIC X(80).
       77  MQ-Control                  Pic X(8) Value 'GENAWMQC'.

           COPY LGPOLICY.


      *----------------------------------------------------------------*
      *CUSTOMER INFORMATION ARRAY USED IN SCENARIO                     *
      *----------------------------------------------------------------*
       01  WS-FOUND                     PIC X(01)  VALUE 'N'.

       01  WS-CUSTOMER-INFO-GROUP.
             03  WS-CUSTOMER-INFO       OCCURS 1000 TIMES
                                        INDEXED BY INDEX-1.
               05 WCI-SEQ-NO            PIC 9(6).
               05 WCI-CUSTOMER-NO       PIC 9(10).
               05 WCI-LAST-PROCESS-DATE PIC X(8).


      ******************************************************************
      *    L I N K A G E     S E C T I O N
      ******************************************************************
       LINKAGE SECTION.

       01  DFHCOMMAREA.
             COPY LGCMAREA.

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
       MAINLINE SECTION.
      *
           INITIALIZE WS-HEADER.
      *
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.
      *----------------------------------------------------------------*
      * Check commarea and obtain required details                     *
      *----------------------------------------------------------------*
           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO EM-VARIABLE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('LGCA') NODUMP END-EXEC
           END-IF

           MOVE '00' TO CA-RETURN-CODE
           MOVE '00' TO CA-NUM-POLICIES
           MOVE EIBCALEN TO WS-CALEN.
           SET WS-ADDR-DFHCOMMAREA TO ADDRESS OF DFHCOMMAREA.

      *----------------------------------------------------------------*
      * Process incoming commarea                                      *
      *----------------------------------------------------------------*
      * check commarea length
           MOVE WS-CUSTOMER-LEN        TO WS-REQUIRED-CA-LEN
           ADD WS-CA-HEADERTRAILER-LEN TO WS-REQUIRED-CA-LEN
           IF EIBCALEN IS LESS THAN WS-REQUIRED-CA-LEN
             MOVE '98' TO CA-RETURN-CODE
             EXEC CICS RETURN END-EXEC
           END-IF


      ******************************************************
      * INITIALIZE CUSTOMER INFO ARRAY                     *
      *                                                    *
      * SEARCH CUSTOMER INFO ARRAY FOR SPECIFIC VALUE      *
      *                                                    *
      * END SEARCH                                         *
      ******************************************************

           PERFORM INITIALIZE-CUSTOMER-INFO
               VARYING WS-CUSTOMER-SUB FROM +1 BY +1
                   UNTIL WS-CUSTOMER-SUB > +1000.


           PERFORM SEARCH-CUSTOMER-INFO  150 TIMES.




      ******************************************************
      * OBTAIN CUSTOMER INFO                               *
      ******************************************************

           MOVE CA-CUSTOMER-NUM TO EM-CUSNUM

           PERFORM GET-CUSTOMER-INFO.

      *----------------------------------------------------------------*
      * END PROGRAM and return to caller                               *
      *----------------------------------------------------------------*
       MAINLINE-END.
           EXEC CICS RETURN END-EXEC.

       MAINLINE-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       GET-CUSTOMER-INFO.

           Move 0 To MQ-Hit
           Exec CICS ReadQ TS Queue(MQ-Control)
                     Into(MQ-Read-Record)
                     Resp(WS-RESP)
                     Item(1)
           End-Exec.
           If WS-RESP = DFHRESP(NORMAL)
              Perform With Test after Until WS-RESP > 0
                 Exec CICS ReadQ TS Queue(MQ-Control)
                     Into(MQ-Read-Record)
                     Resp(WS-RESP)
                     Next
                 End-Exec
                 If WS-RESP = DFHRESP(NORMAL) And
                      MQ-Read-Record(1:6) = 'MQHIT='
                      Move 1 To MQ-Hit
                 End-If
              End-Perform
           End-If.

           If MQ-Hit = 0
             EXEC CICS LINK Program(LGICDB01)
                 Commarea(DFHCOMMAREA)
                 LENGTH(32500)
             END-EXEC
           Else
             EXEC CICS LINK Program('AAAAAAAA')
                 Commarea(DFHCOMMAREA)
                 LENGTH(32500)
             END-EXEC
           End-If.

           EXIT.


      *****PWB
       INITIALIZE-CUSTOMER-INFO.

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

           EXIT.


      *****PWB
       SEARCH-CUSTOMER-INFO.


           SET INDEX-1 TO 1.

           SEARCH  WS-CUSTOMER-INFO VARYING INDEX-1
               AT END
                       MOVE 'N' TO WS-FOUND

               WHEN WCI-LAST-PROCESS-DATE(INDEX-1) = '20170101'
                       MOVE 'Y' TO WS-FOUND
           END-SEARCH.


           EXIT.


      *================================================================*
      * Procedure to write error message to Queues                     *
      *   message will include Date, Time, Program Name, Customer      *
      *   Number, Policy Number and SQLCODE.                           *
      *================================================================*
       WRITE-ERROR-MESSAGE.
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
