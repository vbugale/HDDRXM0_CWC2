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
      *                    Customer menu                               *
      *                                                                *
      * Menu for Customer transactions                                 *
      *                                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGTESTC1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 LGICDB01                  PIC x(8) Value 'LGICDB01'.

       01  WS-RESP                   PIC S9(8) COMP.
       01  WS-Item-Count             PIC S9(4) Comp.
       01  WS-FLAG-TSQH              PIC X.
       01  READ-MSG.
         03 READ-MSG-MSG             PIC X(80).
       01  FILLER REDEFINES Read-MSG.
         03 FILLER                   PIC X(14).
         03 READ-CUST-HIGH           PIC 9(10).
      ******************************
       01  WS-Cust-High              Pic S9(10).
       01  WS-Customer-Key           Pic  9(10).
      ******************************

       01  WRITE-MSG.
         03 WRITE-MSG-E            PIC X(20) Value '**** GENAPP CNTL'.
         03 WRITE-MSG-L              PIC X(13) Value 'LOW CUSTOMER='.
         03 WRITE-MSG-LOW            PIC 9(10).
         03 FILLER                   PIC X.
         03 WRITE-MSG-H              PIC X(14) Value 'HIGH CUSTOMER='.
         03 WRITE-MSG-High           PIC 9(10).
       01  STSQ.
         03  STSQ-NAME                 PIC X(8) Value 'GENACNTL'.
      *
       77 F24                        Pic S9(4) Comp Value 24.
       77 MSGEND                       PIC X(24) VALUE
                                        'Transaction ended      '.

       77 WS-EDIT-ERRORS             PIC X(01) VALUE 'N'.
       77 WS-COUNT                   PIC 9(02) VALUE 0.

       01 WS-AREA.
         03  WS-CUSTOMER-NUM         Pic X(10).
         03  Filler                  Pic X(215).

       77 F82                        Pic S9(4) Comp Value 225.
       77 F10                        Pic S9(4) Comp Value 10.


      ******************************************************************
      *     NUMERIC CONVERSION WORK FIELDS                             *
      ******************************************************************
        77  WS-SUB1                     PIC S9(04)   COMP    VALUE +0.
        77  WS-SUB2                     PIC S9(04)   COMP    VALUE +0.


       01 WMF-NUMERIC-WORK-FIELDS.
           05  WMF-NUM-ERROR           PIC S9(04)  VALUE +0  COMP.
           05  WMF-NUM-LTH             PIC S9(04)  VALUE +0  COMP.
           05  WMF-NUM-INPUT           PIC X(18)   VALUE SPACES.
           05  WMF-NUM-INPUT-R         REDEFINES   WMF-NUM-INPUT
                                       OCCURS 18 TIMES
                                       PIC X(01).
           05  WMF-NUM-OUTPUT          PIC 9(18)   VALUE ZEROES.
           05  WMF-NUM-OUTPUT-R        REDEFINES   WMF-NUM-OUTPUT
                                       OCCURS 18 TIMES
                                       PIC X(01).
           05  WMF-NUM-OUTPUT-910      PIC 9(10).



      ******************************************************************
      *     DATE RELATED WORK FIELDS                                   *
      ******************************************************************

       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR    PIC  9(4).
               10  WS-CURRENT-MONTH   PIC  9(2).
               10  WS-CURRENT-DAY     PIC  9(2).


       01 WMF-WORK-DATES.
           05  WMF-WORK-DATE         PIC X(10).
           05  WMF-WORK-DATE-R       REDEFINES WMF-WORK-DATE.
               10 WMF-WORK-YEAR      PIC X(4).
               10 WMF-WORK-DASH1     PIC X(1).
               10 WMF-WORK-MONTH     PIC X(2).
               10 WMF-WORK-DASH2     PIC X(1).
               10 WMF-WORK-DAY       PIC X(2).


       01 WMF-WORK-FIELDS.
           05  WMF-DATE-OF-BIRTH       PIC X(10).
           05  WMF-DATE-OF-BIRTH-R     REDEFINES WMF-DATE-OF-BIRTH.
               10 WMF-YEAR             PIC X(4).
               10 WMF-YEAR-R           REDEFINES WMF-YEAR  PIC 9(4).
               10 WMF-DASH1            PIC X(1).
               10 WMF-MONTH            PIC X(2).
               10 WMF-MONTH-R          REDEFINES WMF-MONTH PIC 9(2).
               10 WMF-DASH2            PIC X(1).
               10 WMF-DAY              PIC X(2).
               10 WMF-DAY-R            REDEFINES WMF-DAY   PIC 9(2).


           05  WMF-CURRENT-YEAR        PIC 9(5) COMP-3  VALUE 0.
           05  WMF-CURRENT-YEAR-R      REDEFINES WMF-CURRENT-YEAR
                                       PIC X(3).
           05  WMF-MAX-YEAR            PIC 9(4).

      ******************************************************************
      *     CICS COPYBOOKS                                             *
      ******************************************************************

        COPY DFHAID.
        COPY DFHBMSCA.


      ******************************************************************
      *     CICS MAP DSECTS, CICS DFHCOMMAREA
      ******************************************************************

        COPY SSMAP.

        01 COMM-AREA.
        COPY LGCMAREA.


      *****************************************************************
        PROCEDURE DIVISION.
      *****************************************************************
      ***PWB***MAINLINE SECTION.


           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE.


           IF EIBCALEN > 0
              GO TO A-GAIN.

           Initialize SSMAPC1I.
           Initialize SSMAPC1O.
           Initialize COMM-AREA.

           MOVE SPACES             To ERRFLDO
           MOVE DFHBMASB           To ERRFLDA
           MOVE DFHBMASB           To ENT1CDTA
           MOVE '0000000000'       To ENT1CNOO
           MOVE DFHBMFSE           To ENT1CNOA
           MOVE DFHBMASK           To ENT1DATA
           MOVE '_'                To ENT1OPTO.


           MOVE WS-CURRENT-YEAR    To WMF-WORK-YEAR
           MOVE WS-CURRENT-MONTH   To WMF-WORK-MONTH
           MOVE WS-CURRENT-DAY     To WMF-WORK-DAY
           MOVE '-'                To WMF-WORK-DASH1
                                      WMF-WORK-DASH2
           MOVE WMF-WORK-DATE      To ENT1CDTO


      * Display Main Menu
           EXEC CICS SEND MAP ('SSMAPC1')
                     FROM(SSMAPC1O)
                     MAPSET ('SSMAP')
                     ERASE
                     END-EXEC.

           GO TO ENDIT-STARTIT.




       A-GAIN.

           MOVE SPACES         To ERRFLDO
           MOVE DFHBMASK       To ENT1DATA

           EXEC CICS HANDLE AID
                     CLEAR(CLEARIT)
                     PF3(ENDIT) END-EXEC.
           EXEC CICS HANDLE CONDITION
                     MAPFAIL(ENDIT)
                     END-EXEC.

           EXEC CICS RECEIVE MAP('SSMAPC1')
                     INTO(SSMAPC1I) ASIS
                     MAPSET('SSMAP') END-EXEC.



           MOVE WS-CURRENT-YEAR    To WMF-WORK-YEAR
           MOVE WS-CURRENT-MONTH   To WMF-WORK-MONTH
           MOVE WS-CURRENT-DAY     To WMF-WORK-DAY
           MOVE '-'                To WMF-WORK-DASH1
                                      WMF-WORK-DASH2
           MOVE WMF-WORK-DATE      To ENT1CDTO


      *****************************************************************
      *    VALIDATE ATTENTION IDENTIFIER USAGE                        *
      *****************************************************************

           IF EIBAID  = DFHENTER  OR  DFHCLEAR  OR  DFHPF3 OR
                        DFHPF7    OR  DFHPF8
               NEXT SENTENCE
           ELSE
               GO TO ER-INVALID-PFKEY.


      *****************************************************************
      *    SCREEN OPTION EDIT                                         *
      *****************************************************************

           IF ENT1OPTO = '1' OR '2' OR '4'
               NEXT SENTENCE
           ELSE
               Move 'Please enter a valid option'
                       To  ERRFLDO
               Move -1 To  ENT1OPTL

               MOVE DFHBMFSE       To ENT1CNOA

               EXEC CICS SEND MAP ('SSMAPC1')
                         FROM(SSMAPC1O)
                         MAPSET ('SSMAP')
                         CURSOR
               END-EXEC
               GO TO ENDIT-STARTIT.


      *****************************************************************
      *    SCREEN CUSTOMER NUMBER EDIT -- VALUE MUST BE NUMERIC       *
      *****************************************************************

           MOVE +10                    TO WMF-NUM-LTH
           MOVE ENT1CNOO               TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENT1CNOL
              MOVE DFHBMFSE           TO ENT1CNOA
              GO TO ER-CUSTOMER.


           MOVE WMF-NUM-OUTPUT        TO WMF-NUM-OUTPUT-910.
           MOVE WMF-NUM-OUTPUT-910    TO ENT1CNOO
                                         CA-CUSTOMER-NUM.


           IF (EIBAID  = DFHENTER)                    AND
              (CA-CUSTOMER-NUM       = 0001000022)
                 MOVE WS-CURRENT-YEAR        To WMF-CURRENT-YEAR
                 MOVE HIGH-VALUES            To WMF-CURRENT-YEAR-R.


      *****PWB ****************************
           IF EIBAID  = DFHPF7  OR  DFHPF8
              Perform SCROLL-PROCESS
              Move '1'  To ENT1OPTO
           END-IF
      *****PWB ****************************


           EVALUATE ENT1OPTO

             WHEN '1'
                 Move '01ICUS'   To CA-REQUEST-ID
                 EXEC CICS LINK PROGRAM('LGICUS01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC

                 IF CA-CUSTOMER-NUM  = 0
                    GO TO NO-DATA
                 END-IF


                 IF CA-CUSTOMER-NUM  =  CA-CUSTOMER-BEGIN
                    MOVE 'Start of data'     To  ENT1DATO
                 END-IF


                 IF CA-CUSTOMER-NUM  =  CA-CUSTOMER-END
                    MOVE 'End of data'       To  ENT1DATO
                 END-IF


                 IF CA-RETURN-CODE > 0
                   GO TO NO-DATA
                 END-IF


      *****************************************************************
      *    GET CUSTOMER CONTROL RECORD                                *
      *****************************************************************

                 PERFORM  P70000-GET-CONTROL-REC
                    THRU  P70000-GET-CONTROL-REC-EXIT


      *****************************************************************
      *    READ NEXT RECORD                                           *
      *****************************************************************

                 PERFORM  P70100-READNEXT
                     THRU P70100-READNEXT-EXIT


                 Move CA-CUSTOMER-NUM    to ENT1CNOO
                 Move CA-FIRST-NAME      to ENT1FNAI
                 Move CA-LAST-NAME       to ENT1LNAI
                 Move CA-DOB             to ENT1DOBI
                 Move CA-POSTCODE        to ENT1POSI
                 Move CA-PHONE-HOME      to ENT1PH1I
                 Move CA-PHONE-MOBILE    to ENT1PH2I
                 Move CA-EMAIL-ADDRESS   to ENT1EMAI
                 Move CA-NATIONAL-ID-NBR to ENT1NINI
                 Move CA-NIN-TYPE        to ENT1TYPI
                 Move CA-STREET-ADDRESS  to ENT1STRI
                 Move CA-CITY            to ENT1CITI
                 Move CA-STATE           to ENT1STAI
                 Move CA-COUNTRY-CODE    to ENT1COUI
                 Move CA-SALES-TERRITORY to ENT1TERI


                 IF (CA-CUSTOMER-NUM  = 0001000021)    AND
                    (EIBAID           = DFHENTER)
                     Go To ER-DB2-ERROR
                 END-IF

      ***************************************************************
      **
      *    CALCULATE THE MAXIMUM YEAR VALUE ALLOWED
      *
      ***************************************************************
      **

                 COMPUTE WMF-MAX-YEAR =  WMF-CURRENT-YEAR + 1


                 MOVE DFHBMFSE       To ENT1CNOA

                 EXEC CICS SEND MAP ('SSMAPC1')
                           FROM(SSMAPC1O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT


             WHEN '2'

                 MOVE 'N' TO WS-EDIT-ERRORS

                 PERFORM  P80000-EDIT-FIELDS
                     THRU P80000-EDIT-FIELDS-EXIT


                 IF WS-EDIT-ERRORS = 'Y'
                     GO TO ERROR-OUT
                 END-IF


                 Move '01ACUS'   To CA-REQUEST-ID
                 Move 0          To CA-CUSTOMER-NUM
                 Move ENT1FNAI   To CA-FIRST-NAME
                 Move ENT1LNAI   To CA-LAST-NAME
                 Move ENT1DOBI   To CA-DOB
                 Move ENT1POSI   To CA-POSTCODE
                 Move ENT1PH1I   To CA-PHONE-HOME
                 Move ENT1PH2I   To CA-PHONE-MOBILE
                 Move ENT1EMAI   To CA-EMAIL-ADDRESS
                 Move ENT1NINI   To CA-NATIONAL-ID-NBR
                 Move ENT1TYPI   To CA-NIN-TYPE
                 Move ENT1STRI   To CA-STREET-ADDRESS
                 Move ENT1CITI   To CA-CITY
                 Move ENT1STAI   To CA-STATE
                 Move ENT1COUI   To CA-COUNTRY-CODE
                 Move ENT1TERI   To CA-SALES-TERRITORY


                 Inspect COMM-AREA Replacing All x'00'  by x'40'
                 Move Function UPPER-CASE(CA-POSTCODE)
                      TO CA-POSTCODE

                 EXEC CICS LINK PROGRAM('LGACUS01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC


                 IF CA-RETURN-CODE > 0
                   Exec CICS Syncpoint Rollback End-Exec
                   GO TO NO-ADD
                 END-IF

                 Perform WRITE-GENACNTL
                 Move CA-CUSTOMER-NUM To ENT1CNOI
                 Move ' '             To ENT1OPTI
                 Move 'New Customer Inserted'
                                      To  ERRFLDO



                 MOVE DFHBMFSE       To ENT1CNOA

                 EXEC CICS SEND MAP ('SSMAPC1')
                           FROM(SSMAPC1O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT


             WHEN '4'
                 Move '01ICUS'   To CA-REQUEST-ID
                 Move ENT1CNOO   To CA-CUSTOMER-NUM

                 EXEC CICS LINK PROGRAM('LGICUS01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC


                 IF CA-RETURN-CODE > 0
                   GO TO NO-DATA
                 END-IF


                 Move CA-FIRST-NAME to ENT1FNAI
                 Move CA-LAST-NAME       to ENT1LNAI
                 Move CA-DOB             to ENT1DOBI
                 Move CA-POSTCODE        to ENT1POSI
                 Move CA-PHONE-HOME      to ENT1PH1I
                 Move CA-PHONE-MOBILE    to ENT1PH2I
                 Move CA-EMAIL-ADDRESS   to ENT1EMAI
                 Move CA-NATIONAL-ID-NBR to ENT1NINI
                 Move CA-NIN-TYPE        to ENT1TYPI
                 Move CA-STREET-ADDRESS  to ENT1STRI
                 Move CA-CITY            to ENT1CITI
                 Move CA-STATE           to ENT1STAI
                 Move CA-COUNTRY-CODE    to ENT1COUI
                 Move CA-SALES-TERRITORY to ENT1TERI

                 MOVE DFHBMFSE       To ENT1CNOA


                 EXEC CICS SEND MAP ('SSMAPC1')
                           FROM(SSMAPC1O)
                           MAPSET ('SSMAP')
                 END-EXEC


                 EXEC CICS RECEIVE MAP('SSMAPC1')
                           INTO(SSMAPC1I) ASIS
                           MAPSET('SSMAP') END-EXEC


                 Move '01UCUS'   To CA-REQUEST-ID
                 Move ENT1CNOI   To CA-CUSTOMER-NUM
                 Move ENT1FNAI   To CA-FIRST-NAME
                 Move ENT1LNAI   To CA-LAST-NAME
                 Move ENT1DOBI   To CA-DOB
                 Move ENT1POSI   To CA-POSTCODE
                 Move ENT1PH1I   To CA-PHONE-HOME
                 Move ENT1PH2I   To CA-PHONE-MOBILE
                 Move ENT1EMAI   To CA-EMAIL-ADDRESS
                 Move ENT1NINI   To CA-NATIONAL-ID-NBR
                 Move ENT1TYPI   To CA-NIN-TYPE
                 Move ENT1STRI   To CA-STREET-ADDRESS
                 Move ENT1CITI   To CA-CITY
                 Move ENT1STAI   To CA-STATE
                 Move ENT1COUI   To CA-COUNTRY-CODE
                 Move ENT1TERI   To CA-SALES-TERRITORY


                 Inspect COMM-AREA Replacing All x'00'  by x'40'
                 Move Function UPPER-CASE(CA-POSTCODE)
                      TO CA-POSTCODE

                 EXEC CICS LINK PROGRAM('LGUCUS01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC

                 IF CA-RETURN-CODE > 0
                   GO TO NO-UPD
                 END-IF

                 Move CA-CUSTOMER-NUM To ENT1CNOI
                 Move ' '             To ENT1OPTI
                 Move 'Customer details updated'
                   To  ERRFLDO

                 MOVE DFHBMFSE       To ENT1CNOA

                 EXEC CICS SEND MAP ('SSMAPC1')
                           FROM(SSMAPC1O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT

             WHEN OTHER

                 Move 'Please enter a valid option'
                   To  ERRFLDO
                 Move -1 To ENT1OPTL

                 MOVE DFHBMFSE       To ENT1CNOA

                 EXEC CICS SEND MAP ('SSMAPC1')
                           FROM(SSMAPC1O)
                           MAPSET ('SSMAP')
                           CURSOR
                 END-EXEC
                 GO TO ENDIT-STARTIT

           END-EVALUATE.


      *    Send message to terminal and return

           EXEC CICS RETURN
           END-EXEC.

       ENDIT-STARTIT.
           MOVE DFHBMASK       To ENT1DATA
           EXEC CICS RETURN
                TRANSID('SSC1')
                COMMAREA(COMM-AREA)
                END-EXEC.

       ENDIT.
           EXEC CICS SEND TEXT
                     FROM(MSGEND)
                     LENGTH(LENGTH OF MSGEND)
                     ERASE
                     FREEKB
           END-EXEC
           EXEC CICS RETURN
           END-EXEC.

       CLEARIT.

           Initialize SSMAPC1I.
           MOVE DFHBMFSE       To ENT1CNOA

           EXEC CICS SEND MAP ('SSMAPC1')
                     MAPSET ('SSMAP')
                     MAPONLY
           END-EXEC

           EXEC CICS RETURN
                TRANSID('SSC1')
                COMMAREA(COMM-AREA)
                END-EXEC.


      *****PWB ****************************
       SCROLL-PROCESS.

           IF EIBAID  = DFHPF7
               Move '07'          To CA-CUSTOMER-PFKEY
           END-IF

           IF EIBAID  = DFHPF8
               Move '08'          To CA-CUSTOMER-PFKEY
           END-IF


           EXEC CICS LINK Program(LGICDB01)
               Commarea(COMM-AREA)
               LENGTH(32500)
           END-EXEC.


      *****IF CA-CUSTOMER-NUM  =  0001000144
      *****   Go To ER-DB2-ERROR.


      *****PWB ****************************

       ER-INVALID-PFKEY.
           Move 'Invalid PFKEY Selection'          To  ERRFLDO.
           MOVE -1                                 To  ENT1OPTL.
           Go To ERROR-OUT.


       ER-CUSTOMER.
           Move 'Cust Number must be numeric'      To  ERRFLDO.
           MOVE -1                                 To  ENT1CNOL.
           Go To ERROR-OUT.


       NO-UPD.
           Move 'Error Updating Customer'          To  ERRFLDO.
           MOVE -1                                 To  ENT1CNOL.
           Go To ERROR-OUT.


       NO-ADD.
           Move 'Error Adding Customer'            To  ERRFLDO.
           MOVE -1                                 To  ENT1CNOL.
           Go To ERROR-OUT.


       ER-DOB-NUMERIC.
           Move 'DOB YYYY,MM,DD must be numeric'   To  ERRFLDO.
           MOVE -1                                 To  ENT1DOBL.
           Go To ERROR-OUT.


       ER-DOB-FORMAT.
           Move 'DOB format is YYYY-MM-DD'         To  ERRFLDO.
           MOVE -1                                 To  ENT1DOBL.
           Go To ERROR-OUT.


       ER-DOB-YYYY-MAX.
           Move 'DOB YYYY exceeds current year '   To  ERRFLDO.
           MOVE -1                                 To  ENT1DOBL.
           Go To ERROR-OUT.


       ER-DOB-MONTH-RANGE.
           Move 'DOB MM (month) range 01-12'       To  ERRFLDO.
           MOVE -1                                 To  ENT1DOBL.
           Go To ERROR-OUT.


       ER-DOB-DAY-RANGE.
           Move 'DOB DD (day) range 01-31'         To  ERRFLDO.
           MOVE -1                                 To  ENT1DOBL.
           Go To ERROR-OUT.


       ER-DOB-31-DAYS.
           Move '31 DAYS not valid for month'      To  ERRFLDO.
           MOVE -1                                 To  ENT1DOBL.
           Go To ERROR-OUT.


       ER-NATIONAL-ID-TYPE.
           Move 'National ID Type is invalid'      To  ERRFLDO.
           MOVE -1                                 To  ENT1TYPL.
           Go To ERROR-OUT.


       ER-SALES-TERRITORY.
           Move 'Sales Territory is invalid'       To  ERRFLDO.
           MOVE -1                                 To  ENT1TERL.
           Go To ERROR-OUT.


       ER-PHONE-HOME.
           Move 'Phone Home must be numeric'       To  ERRFLDO.
           MOVE -1                                 To  ENT1PH1L.
           Go To ERROR-OUT.


       ER-PHONE-MOBILE.
           Move 'Phone Mobile must be numeric'     To  ERRFLDO.
           MOVE -1                                 To  ENT1PH2L.
           Go To ERROR-OUT.


       ER-EMAIL-ADDRESS.
           Move 'Email Address requires one @ symbol'   To ERRFLDO.
           MOVE -1                                      To ENT1EMAL.
           Go To ERROR-OUT.


       ER-DB2-ERROR.
           Move 'DB2 Error SQLCODE = -303'         To  ERRFLDO.
           MOVE -1                                 To  ENT1OPTL.
           Go To ERROR-OUT.



       NO-DATA.
           Move 'No data was returned.'            To  ERRFLDO.
           MOVE -1                                 To  ENT1CNOL.


           IF EIBAID NOT = DFHENTER
               MOVE '0000000000'         To ENT1CNOO.


           Initialize                       ENT1FNAI
           Initialize                       ENT1LNAI
           Initialize                       ENT1DOBI
           Initialize                       ENT1POSI
           Initialize                       ENT1PH1I
           Initialize                       ENT1PH2I
           Initialize                       ENT1EMAI
           Initialize                       ENT1NINI
           Initialize                       ENT1TYPI
           Initialize                       ENT1STRI
           Initialize                       ENT1CITI
           Initialize                       ENT1STAI
           Initialize                       ENT1COUI
           Initialize                       ENT1TERI

           Go To ERROR-OUT.


       ERROR-OUT.

           MOVE DFHBMFSE       To ENT1CNOA

           EXEC CICS SEND MAP ('SSMAPC1')
                     FROM(SSMAPC1O)
                     MAPSET ('SSMAP')
                     CURSOR
           END-EXEC.

           Initialize SSMAPC1I.
           Initialize SSMAPC1O.
           Initialize COMM-AREA.

           MOVE DFHBMFSE       To ENT1CNOA.

           GO TO ENDIT-STARTIT.
      *--------------------------------------------------------------*
       WRITE-GENACNTL.

           EXEC CICS ENQ Resource(STSQ-NAME)
                         Length(Length Of STSQ-NAME)
           END-EXEC.
           Move 'Y' To WS-FLAG-TSQH
           Move 1   To WS-Item-Count
           Exec CICS ReadQ TS Queue(STSQ-NAME)
                     Into(READ-MSG)
                     Resp(WS-RESP)
                     Item(1)
           End-Exec.
           If WS-RESP = DFHRESP(NORMAL)
              Perform With Test after Until WS-RESP > 0
                 Exec CICS ReadQ TS Queue(STSQ-NAME)
                     Into(READ-MSG)
                     Resp(WS-RESP)
                     Next
                 End-Exec
                 Add 1 To WS-Item-Count
                 If WS-RESP = DFHRESP(NORMAL) And
                      Read-Msg-Msg(1:13) = 'HIGH CUSTOMER'
                      Move CA-Customer-Num To Write-Msg-High
                      Move Space to WS-FLAG-TSQH
                      Exec CICS WriteQ TS Queue(STSQ-NAME)
                          From(Write-Msg-H)
                          Length(F24)
                          Resp(WS-RESP)
                          ReWrite
                          Item(WS-Item-Count)
                      End-Exec
                      MOVE 99 To WS-RESP
                 End-If
              End-Perform
           End-If.
      *
      *
           If WS-FLAG-TSQH = 'Y'
             EXEC CICS WRITEQ TS QUEUE(STSQ-NAME)
                       FROM(WRITE-MSG-E)
                       RESP(WS-RESP)
                       NOSUSPEND
                       LENGTH(20)
             END-EXEC
             Move CA-Customer-Num To Write-Msg-Low
             Move CA-Customer-Num To Write-Msg-High
             EXEC CICS WRITEQ TS QUEUE(STSQ-NAME)
                       FROM(WRITE-MSG-L)
                       RESP(WS-RESP)
                       NOSUSPEND
                       LENGTH(23)
             END-EXEC
             EXEC CICS WRITEQ TS QUEUE(STSQ-NAME)
                       FROM(WRITE-MSG-H)
                       RESP(WS-RESP)
                       NOSUSPEND
                       LENGTH(24)
             END-EXEC
           End-If.

           EXEC CICS DEQ Resource(STSQ-NAME)
                         Length(Length Of STSQ-NAME)
           END-EXEC.



      *****************************************************************
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE CUSTOMER CONTROL RECORD    *
      *                                                               *
      *****************************************************************

       P70000-GET-CONTROL-REC.

           MOVE 0019999001 To WRITE-MSG-HIGH.


           IF (EIBAID  = DFHENTER)                    AND
              (CA-CUSTOMER-NUM       = 0000000002)

               Exec CICS Read File('KSDSCUST')
                         Into(WS-AREA)
                         Length(F82)
                         Ridfld(WRITE-MSG-HIGH)
                         KeyLength(F10)
                         EQUAL
               End-Exec
           ELSE
               Exec CICS Read File('KSDSCUST')
                         Into(WS-AREA)
                         Length(F82)
                         Ridfld(WRITE-MSG-HIGH)
                         KeyLength(F10)
                         RESP(WS-RESP)
                         EQUAL
               End-Exec.

      *****************************************************
      **REMOVE RESP(WS-RESP) ABOVE IN THE READ TO GET AEIM
      *****************************************************

       P70000-GET-CONTROL-REC-EXIT.
           EXIT.


      *****************************************************************
      *                                                               *
      *    FUNCTION :  READ NEXT RECORD                               *
      *                                                               *
      *****************************************************************

       P70100-READNEXT.


           IF (EIBAID  = DFHENTER)                    AND
              (CA-CUSTOMER-NUM       = 0001000001)

               Exec CICS READNEXT  File('KSDSCUST')
                         Into(WS-AREA)
                         Length(F82)
                         Ridfld(WRITE-MSG-HIGH)
                         KeyLength(F10)
               End-Exec
           ELSE
               Exec CICS READNEXT  File('KSDSCUST')
                         Into(WS-AREA)
                         Length(F82)
                         Ridfld(WRITE-MSG-HIGH)
                         KeyLength(F10)
                         RESP(WS-RESP)
               End-Exec.

       P70100-READNEXT-EXIT.
           EXIT.


      *****************************************************************
      *                                                               *
      *    FUNCTION :  ROUTINE TO RIGHT JUSTIFY AND VALIDATE NUMERICS *
      *                IN A FIELD                                     *
      *                                                               *
      *****************************************************************

       P70500-EDIT-NUMERIC-FIELD.


           MOVE ZEROES                 TO WMF-NUM-ERROR.
           MOVE ZEROES                 TO WMF-NUM-OUTPUT.
           MOVE +18                    TO WS-SUB2.

           PERFORM  P70550-EDIT-NUMERIC
               THRU P70550-EDIT-NUMERIC-EXIT
                   VARYING WS-SUB1 FROM WMF-NUM-LTH BY -1
                       UNTIL WS-SUB1 < 1.


       P70500-EDIT-NUMERIC-FIELD-EXIT.
               EXIT.


      *****************************************************************
      *    FUNCTION :  ROUTINE TO RIGHT JUSTIFY AND VALIDATE NUMERICS *
      *                IN A FIELD                                     *
      *****************************************************************

       P70550-EDIT-NUMERIC.


           IF WMF-NUM-INPUT-R (WS-SUB1) > SPACES
               IF WMF-NUM-INPUT-R (WS-SUB1) NUMERIC
                   MOVE WMF-NUM-INPUT-R (WS-SUB1)
                                       TO WMF-NUM-OUTPUT-R (WS-SUB2)
                   COMPUTE WS-SUB2  =  WS-SUB2 - 1
               ELSE
                   ADD +1              TO WMF-NUM-ERROR
           ELSE
                   NEXT SENTENCE.


       P70550-EDIT-NUMERIC-EXIT.
           EXIT.



      *****************************************************************
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM SCREEN DATA FIELD EDITS     *
      *                                                               *
      *****************************************************************

       P80000-EDIT-FIELDS.


      ********************************************
      *    DATE OF BIRTH                         *
      ********************************************

           MOVE ENT1DOBI               TO WMF-DATE-OF-BIRTH.

           IF WMF-DASH1 = '-'  AND
              WMF-DASH2 = '-'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              GO TO ER-DOB-FORMAT.


           IF WMF-YEAR  NUMERIC    AND
              WMF-MONTH NUMERIC    AND
              WMF-DAY   NUMERIC
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              GO TO ER-DOB-NUMERIC.


           IF WMF-YEAR-R > WS-CURRENT-YEAR
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              GO TO ER-DOB-YYYY-MAX.


           IF (WMF-MONTH-R > 0) AND (WMF-MONTH-R < 13)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              GO TO ER-DOB-MONTH-RANGE.


           IF (WMF-DAY-R > 0) AND (WMF-DAY-R < 32)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              GO TO ER-DOB-DAY-RANGE.


           IF (WMF-DAY-R = 31)
               IF (WMF-MONTH-R = 01)  OR
                  (WMF-MONTH-R = 03)  OR
                  (WMF-MONTH-R = 05)  OR
                  (WMF-MONTH-R = 07)  OR
                  (WMF-MONTH-R = 08)  OR
                  (WMF-MONTH-R = 10)  OR
                  (WMF-MONTH-R = 12)
                  NEXT SENTENCE
               ELSE
                  MOVE 'Y'            TO WS-EDIT-ERRORS
                  GO TO ER-DOB-31-DAYS
           ELSE
                  NEXT SENTENCE.


      ********************************************
      *    NATIONAL ID NUMBER TYPE               *
      ********************************************

           IF ENT1TYPI = 'SS' OR
              ENT1TYPI = 'PP' OR
              ENT1TYPI = 'DL'
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              GO TO ER-NATIONAL-ID-TYPE.


      ********************************************
      *    SALES TERRITORY                       *
      ********************************************

           IF ENT1TERI = 'NA'           OR
              ENT1TERI = 'EMEA'         OR
              ENT1TERI = 'APAC'         OR
              ENT1TERI = 'LATAM'
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                 TO WS-EDIT-ERRORS
               GO TO ER-SALES-TERRITORY.



      ********************************************
      *    PHONE HOME MUST BE NUMERIC            *
      ********************************************

           MOVE +10                   TO WMF-NUM-LTH
           MOVE ENT1PH1I              TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE 'Y'                TO WS-EDIT-ERRORS
              MOVE DFHBMFSE           TO ENT1CNOA
              GO TO ER-PHONE-HOME.


           MOVE WMF-NUM-OUTPUT        TO WMF-NUM-OUTPUT-910
           MOVE WMF-NUM-OUTPUT-910    TO ENT1PH1O.


      ********************************************
      *    PHONE MOBILE MUST BE NUMERIC          *
      ********************************************

           MOVE +10                   TO WMF-NUM-LTH
           MOVE ENT1PH2I              TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE 'Y'                TO WS-EDIT-ERRORS
              MOVE DFHBMFSE           TO ENT1CNOA
              GO TO ER-PHONE-MOBILE.


           MOVE WMF-NUM-OUTPUT        TO WMF-NUM-OUTPUT-910
           MOVE WMF-NUM-OUTPUT-910    TO ENT1PH2O.


      ********************************************
      *    EMAIL ADDRESS                         *
      *    ONE @ SIGN REQUIRED                   *
      ********************************************

           IF ENT1EMAI > SPACES
               INSPECT ENT1EMAI TALLYING WS-COUNT FOR ALL '@'
               IF WS-COUNT = 1
                   NEXT SENTENCE
               ELSE
                   MOVE 'Y'                TO WS-EDIT-ERRORS
                   MOVE DFHBMFSE           TO ENT1EMAA
                   GO TO ER-EMAIL-ADDRESS
           ELSE
                   NEXT SENTENCE.


       P80000-EDIT-FIELDS-EXIT.
           EXIT.

