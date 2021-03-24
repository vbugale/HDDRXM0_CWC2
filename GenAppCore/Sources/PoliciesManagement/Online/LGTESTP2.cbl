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
      *                Endowment Policy Menu                           *
      *                                                                *
      * Menu for Endowment Policy Transactions                         *
      *                                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGTESTP2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77 MSGEND                       PIC X(24) VALUE
                                        'Transaction ended      '.

       77 WS-EDIT-ERRORS             PIC X(01) VALUE 'N'.


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

       01 WMF-WORK-FIELDS.
           05  WMF-DATE                PIC X(10).
           05  WMF-DATE-R              REDEFINES WMF-DATE.
               10 WMF-YEAR             PIC X(4).
               10 WMF-YEAR-R           REDEFINES WMF-YEAR  PIC 9(4).
               10 WMF-DASH1            PIC X(1).
               10 WMF-MONTH            PIC X(2).
               10 WMF-MONTH-R          REDEFINES WMF-MONTH PIC 9(2).
               10 WMF-DASH2            PIC X(1).
               10 WMF-DAY              PIC X(2).
               10 WMF-DAY-R            REDEFINES WMF-DAY   PIC 9(2).

           05  WMF-DATE-MMYY           PIC X(05).
           05  WMF-DATE-MMYY-R         REDEFINES WMF-DATE-MMYY.
               10 WMF-MM               PIC X(2).
               10 WMF-MM-R             REDEFINES WMF-MM    PIC 9(2).
               10 WMF-SLASH1           PIC X(1).
               10 WMF-YY               PIC X(2).
               10 WMF-YY-R             REDEFINES WMF-YY    PIC 9(2).

           05  WMF-TERM-YEARS          PIC 9(2).
           05  WMF-SUM-ASSURED         PIC 9(6).
           05  WMF-LIFE-ASSURED        PIC 9(12).
           05  WMF-FROM-CHECKING       PIC 9(12).
           05  WMF-BANK-ROUTING        PIC 9(9).


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

       MAINLINE SECTION.

           IF EIBCALEN > 0
              GO TO A-GAIN.

           Initialize SSMAPP2I.
           Initialize SSMAPP2O.
           Initialize COMM-AREA.


           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE.


           MOVE '0000000000'   To ENP2PNOO.
           MOVE  DFHBMFSE      To ENP2PNOA.
           MOVE '0000000000'   To ENP2CNOO.
           MOVE  DFHBMASK      To ENP2DATA.
           MOVE  DFHBMASB      To ERP2FLDA.
           MOVE  DFHBMASB      To ENP2CDTA.
           MOVE  '_'           To ENP2OPTO.


           MOVE WS-CURRENT-YEAR    To WMF-YEAR
           MOVE WS-CURRENT-MONTH   To WMF-MONTH
           MOVE WS-CURRENT-DAY     To WMF-DAY
           MOVE '-'                To WMF-DASH1
                                      WMF-DASH2
           MOVE WMF-DATE           To ENP2CDTO


      * Display Main Menu
           EXEC CICS SEND MAP ('SSMAPP2')
                     MAPSET ('SSMAP')
                     FROM   (SSMAPP2O)
                     ERASE
                     END-EXEC.


       A-GAIN.

           MOVE  DFHBMASK      To ENP2DATA.

           EXEC CICS HANDLE AID
                     CLEAR(CLEARIT)
                     PF3(ENDIT) END-EXEC.
           EXEC CICS HANDLE CONDITION
                     MAPFAIL(ENDIT)
                     END-EXEC.

           EXEC CICS RECEIVE MAP('SSMAPP2')
                     INTO(SSMAPP2I)
                     MAPSET('SSMAP') END-EXEC.


           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE.

           MOVE WS-CURRENT-YEAR    To WMF-YEAR
           MOVE WS-CURRENT-MONTH   To WMF-MONTH
           MOVE WS-CURRENT-DAY     To WMF-DAY
           MOVE '-'                To WMF-DASH1
                                      WMF-DASH2
           MOVE WMF-DATE           To ENP2CDTO



      *****************************************************************
      *    VALIDATE ATTENTION IDENTIFIER USAGE                        *
      *****************************************************************

            IF EIBAID  = DFHENTER  OR  DFHCLEAR  OR  DFHPF3 OR
                         DFHPF7    OR  DFHPF8 OR  DFHPF24
                NEXT SENTENCE
            ELSE
                GO TO ER-INVALID-PFKEY.


      *****************************************************************
      *    SCREEN OPTION EDIT                                         *
      *****************************************************************

           IF ENP2OPTO = '1' OR '2' OR '3' OR '4'
                 NEXT SENTENCE
           ELSE
                 Move 'Please enter a valid option'
                             To  ERP2FLDO
                 Move -1 To  ENP2OPTL

                 EXEC CICS SEND MAP ('SSMAPP2')
                           FROM(SSMAPP2O)
                               MAPSET ('SSMAP')
                               CURSOR
                 END-EXEC
                 GO TO ENDIT-STARTIT.

      ***************************************************************
      **
      **   SCREEN OPTION VS PFKEY EDIT, PF24 ONLY USED FOR OPTION 3
      *
      ***************************************************************

           IF ENP2OPTO NOT = '3'
               IF EIBAID = DFHPF24
                    Move 'Invalid PFKEY Selection' To  ERP2FLDO
                    Move -1 To  ENP2OPTL

                    EXEC CICS SEND MAP ('SSMAPP2')
                         FROM(SSMAPP2O)
                         MAPSET ('SSMAP')
                         CURSOR
                    END-EXEC
                    GO TO ENDIT-STARTIT
               ELSE
                    NEXT SENTENCE
           ELSE
                    NEXT SENTENCE.


      *****************************************************************
      *    SCREEN POLICY   NUMBER EDIT -- VALUE MUST BE NUMERIC       *
      *****************************************************************

           MOVE +10                    TO WMF-NUM-LTH
           MOVE ENP2PNOO               TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP2PNOL
              MOVE DFHBMFSE           TO ENP2PNOA
              GO TO ER-POLICY.


           MOVE WMF-NUM-OUTPUT        TO WMF-NUM-OUTPUT-910
           MOVE WMF-NUM-OUTPUT-910    TO ENP2PNOO
                                         CA-POLICY-NUM


      *****************************************************************
      *    SCREEN CUSTOMER NUMBER EDIT -- VALUE MUST BE NUMERIC       *
      *****************************************************************

           MOVE +10                    TO WMF-NUM-LTH
           MOVE ENP2CNOO               TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP2CNOL
              MOVE DFHBMFSE           TO ENP2CNOA
              GO TO ER-CUSTOMER.


           MOVE WMF-NUM-OUTPUT        TO WMF-NUM-OUTPUT-910
           MOVE WMF-NUM-OUTPUT-910    TO ENP2CNOO
                                         CA-CUSTOMER-NUM


      *******************************************************************
      *    SCREEN OPTION 3 (DELETE) REQUIRES AUTHORIZATION            *
      *****************************************************************

           IF (ENP2OPTO = '3')
               IF (EIBAID = DFHPF24)
                   NEXT SENTENCE
               ELSE
                   Move 'DELETE REQUIRES AUTHORIZATION'
                           To  ERP2FLDO
                   Move -1 To  ENP2OPTL

                   EXEC CICS SEND MAP ('SSMAPP2')
                             FROM(SSMAPP2O)
                             MAPSET ('SSMAP')
                               CURSOR
                     END-EXEC
                     GO TO ENDIT-STARTIT
           ELSE
                     NEXT SENTENCE.

      *****************************************************************
      *    CHECK FOR SCROLLING PF7-BACKWARD, PF8-FORWARD              *
      *****************************************************************

      *****PWB ****************************
                IF EIBAID  = DFHPF7  OR  DFHPF8
                     Perform SCROLL-PROCESS
                     Move '1'  To ENP2OPTO
                     Move ENP2PNOO        To CA-POLICY-NUM
                     Move ENP2CNOO        To CA-CUSTOMER-NUM
                END-IF
      *****PWB ****************************


      *****************************************************************
      *    PROCESS USER OPTION SELECTION                              *
      *****************************************************************

           EVALUATE ENP2OPTO

             WHEN '1'
                 Move '01IEND'   To CA-REQUEST-ID
                 Move ENP2CNOO   To CA-CUSTOMER-NUM
                 Move ENP2PNOO   To CA-POLICY-NUM
                 EXEC CICS LINK PROGRAM('LGIPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC


                 IF CA-POLICY-NUM  = 0
                    MOVE 0       To CA-CUSTOMER-NUM
                    GO TO NO-DATA
                 END-IF


                 IF CA-POLICY-NUM  =  CA-POLICY-BEGIN
                    MOVE 'Start of data'     To  ENP2DATO
                 END-IF


                 IF CA-POLICY-NUM  =  CA-POLICY-END
                    MOVE 'End of data'       To  ENP2DATO
                 END-IF


                 IF CA-RETURN-CODE > 0
                   GO TO NO-DATA
                 END-IF


                 Move CA-CUSTOMER-NUM       To  ENP2CNOO
                 Move CA-POLICY-NUM         To  ENP2PNOO

                 Move CA-ISSUE-DATE         To  ENP2IDAI
                 Move CA-EXPIRY-DATE        To  ENP2EDAI
                 Move CA-E-FUND-NAME        To  ENP2FNMI
                 Move CA-E-TERM             To  ENP2TERI
                 Move CA-E-SUM-ASSURED      To  ENP2SUMI
                 Move CA-E-LIFE-ASSURED     To  ENP2LIFI
                 Move CA-E-WITH-PROFITS     To  ENP2WPRI
                 Move CA-E-MANAGED-FUND     To  ENP2MANI
                 Move CA-E-EQUITIES         To  ENP2EQUI
                 Move CA-E-AUTO-PAY         To  ENP2PAYI
                 Move CA-E-CHECK-ACCT-NBR   To  ENP2ACTI
                 Move CA-E-BANK-ROUTE-CODE  To  ENP2ROUI
                 Move CA-E-CREDIT-CARD-TYP  To  ENP2CCTI
                 Move CA-E-CREDIT-CARD-NBR  To  ENP2CCNI
                 Move CA-E-CREDIT-CARD-PIN  To  ENP2CCPI
                 Move CA-E-CREDIT-CARD-VAL  To  ENP2CCVI


                 EXEC CICS SEND MAP ('SSMAPP2')
                           FROM(SSMAPP2O)
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


                 Move '01AEND'          To CA-REQUEST-ID
                 Move ENP2CNOI          To CA-CUSTOMER-NUM
                 Move 0                 To CA-PAYMENT
                 Move 0                 To CA-BROKERID
                 Move '        '        To CA-BROKERSREF
                 Move ENP2IDAI          To CA-ISSUE-DATE
                 Move ENP2EDAI          To CA-EXPIRY-DATE
                 Move ENP2FNMI          To CA-E-FUND-NAME
                 Move ENP2TERI          To CA-E-TERM
                 Move ENP2SUMI          To CA-E-SUM-ASSURED
                 Move ENP2LIFI          To CA-E-LIFE-ASSURED
                 Move ENP2WPRI          To CA-E-WITH-PROFITS
                 Move ENP2MANI          To CA-E-MANAGED-FUND
                 Move ENP2EQUI          To CA-E-EQUITIES
                 Move ENP2PAYI          To CA-E-AUTO-PAY
                 Move ENP2ACTI          To CA-E-CHECK-ACCT-NBR
                 Move ENP2ROUI          To CA-E-BANK-ROUTE-CODE
                 Move ENP2CCTI          To CA-E-CREDIT-CARD-TYP
                 Move ENP2CCNI          To CA-E-CREDIT-CARD-NBR
                 Move ENP2CCPI          To CA-E-CREDIT-CARD-PIN
                 Move ENP2CCVI          To CA-E-CREDIT-CARD-VAL


                 EXEC CICS LINK PROGRAM('LGAPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC


                 IF CA-RETURN-CODE > 0
                   Exec CICS Syncpoint Rollback End-Exec
                   GO TO NO-ADD
                 END-IF

                 Move CA-CUSTOMER-NUM To ENP2CNOI
                 Move CA-POLICY-NUM   To ENP2PNOI
                 Move CA-E-FUND-NAME  To ENP2FNMI
                 Move ' '             To ENP2OPTI
                 Move 'New Life Policy Inserted'
                   To  ERP2FLDO


                 EXEC CICS SEND MAP ('SSMAPP2')
                           FROM(SSMAPP2O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT


             WHEN '3'
                 Move '01DEND'   To CA-REQUEST-ID
                 Move ENP2CNOO   To CA-CUSTOMER-NUM
                 Move ENP2PNOO   To CA-POLICY-NUM

                 EXEC CICS LINK PROGRAM('LGDPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC

                 IF CA-RETURN-CODE > 0
                   Exec CICS Syncpoint Rollback End-Exec
                   GO TO NO-DELETE
                 END-IF

                 Move Spaces            To  ENP2IDAI
                 Move Spaces            To  ENP2EDAI
                 Move Spaces            To  ENP2FNMI
                 Move Spaces            To  ENP2TERI
                 Move Spaces            To  ENP2SUMI
                 Move Spaces            To  ENP2LIFI
                 Move Spaces            To  ENP2WPRI
                 Move Spaces            To  ENP2MANI
                 Move Spaces            To  ENP2EQUI
                 Move Spaces            To  ENP2PAYI
                 Move Spaces            To  ENP2ACTI
                 Move Spaces            To  ENP2ROUI
                 Move Spaces            To  ENP2CCTI
                 Move Spaces            To  ENP2CCNI
                 Move Spaces            To  ENP2CCPI
                 Move Spaces            To  ENP2CCVI

                 Move 'Life Policy Deleted'
                   To  ERP2FLDO

                 EXEC CICS SEND MAP ('SSMAPP2')
                           FROM(SSMAPP2O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT


             WHEN '4'
                 Move '01IEND'   To CA-REQUEST-ID
                 Move ENP2CNOO   To CA-CUSTOMER-NUM
                 Move ENP2PNOO   To CA-POLICY-NUM

                 EXEC CICS LINK PROGRAM('LGIPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC

                 IF CA-RETURN-CODE > 0
                   GO TO NO-DATA
                 END-IF

                 Move CA-ISSUE-DATE         To  ENP2IDAI
                 Move CA-EXPIRY-DATE        To  ENP2EDAI
                 Move CA-E-FUND-NAME        To  ENP2FNMI
                 Move CA-E-TERM             To  ENP2TERI
                 Move CA-E-SUM-ASSURED      To  ENP2SUMI
                 Move CA-E-LIFE-ASSURED     To  ENP2LIFI
                 Move CA-E-WITH-PROFITS     To  ENP2WPRI
                 Move CA-E-MANAGED-FUND     To  ENP2MANI
                 Move CA-E-EQUITIES         To  ENP2EQUI
                 Move CA-E-AUTO-PAY         To  ENP2PAYI
                 Move CA-E-CHECK-ACCT-NBR   To  ENP2ACTI
                 Move CA-E-BANK-ROUTE-CODE  To  ENP2ROUI
                 Move CA-E-CREDIT-CARD-TYP  To  ENP2CCTI
                 Move CA-E-CREDIT-CARD-NBR  To  ENP2CCNI
                 Move CA-E-CREDIT-CARD-PIN  To  ENP2CCPI
                 Move CA-E-CREDIT-CARD-VAL  To  ENP2CCVI


                 EXEC CICS SEND MAP ('SSMAPP2')
                           FROM(SSMAPP2O)
                           MAPSET ('SSMAP')
                 END-EXEC

                 EXEC CICS RECEIVE MAP('SSMAPP2')
                           INTO(SSMAPP2I)
                           MAPSET('SSMAP') END-EXEC

                 Move '01UEND'          To CA-REQUEST-ID
                 Move ENP2CNOI          To CA-CUSTOMER-NUM
                 Move 0                 To CA-PAYMENT
                 Move 0                 To CA-BROKERID
                 Move '        '        To CA-BROKERSREF
                 Move ENP2IDAI          To CA-ISSUE-DATE
                 Move ENP2EDAI          To CA-EXPIRY-DATE
                 Move ENP2FNMI          To CA-E-FUND-NAME
                 Move ENP2TERI          To CA-E-TERM
                 Move ENP2SUMI          To CA-E-SUM-ASSURED
                 Move ENP2LIFI          To CA-E-LIFE-ASSURED
                 Move ENP2WPRI          To CA-E-WITH-PROFITS
                 Move ENP2MANI          To CA-E-MANAGED-FUND
                 Move ENP2EQUI          To CA-E-EQUITIES
                 Move ENP2PAYI          To CA-E-AUTO-PAY
                 Move ENP2ACTI          To CA-E-CHECK-ACCT-NBR
                 Move ENP2ROUI          To CA-E-BANK-ROUTE-CODE
                 Move ENP2CCTI          To CA-E-CREDIT-CARD-TYP
                 Move ENP2CCNI          To CA-E-CREDIT-CARD-NBR
                 Move ENP2CCPI          To CA-E-CREDIT-CARD-PIN
                 Move ENP2CCVI          To CA-E-CREDIT-CARD-VAL

                 EXEC CICS LINK PROGRAM('LGUPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC

                 IF CA-RETURN-CODE > 0
                   GO TO NO-UPD
                 END-IF

                 Move CA-CUSTOMER-NUM To ENP2CNOI
                 Move CA-POLICY-NUM   To ENP2PNOI
                 Move ' '             To ENP2OPTI
                 Move 'Life Policy Updated'
                   To  ERP2FLDO
                 EXEC CICS SEND MAP ('SSMAPP2')
                           FROM(SSMAPP2O)
                           MAPSET ('SSMAP')
                 END-EXEC

                 GO TO ENDIT-STARTIT

             WHEN OTHER

                 Move 'Please enter a valid option'
                   To  ERP2FLDO
                 Move -1 To ENP2OPTL

                 EXEC CICS SEND MAP ('SSMAPP2')
                           FROM(SSMAPP2O)
                           MAPSET ('SSMAP')
                           CURSOR
                 END-EXEC
                 GO TO ENDIT-STARTIT

           END-EVALUATE.


      *    Send message to terminal and return

           EXEC CICS RETURN
           END-EXEC.

       ENDIT-STARTIT.
           MOVE  DFHBMASK      To ENP2DATA.
           EXEC CICS RETURN
                TRANSID('SSP2')
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

           Initialize SSMAPP2I.
           EXEC CICS SEND MAP ('SSMAPP2')
                     MAPSET ('SSMAP')
                     MAPONLY
           END-EXEC

           EXEC CICS RETURN
                TRANSID('SSP2')
                COMMAREA(COMM-AREA)
                END-EXEC.

      *****PWB ****************************

       SCROLL-PROCESS.

           IF EIBAID  = DFHPF7
              Move '07'        To CA-POLICY-PFKEY
           END-IF

           IF EIBAID  = DFHPF8
              Move '08'        To CA-POLICY-PFKEY
           END-IF.

      *****PWB ****************************

       ER-INVALID-PFKEY.
           Move 'Invalid PFKEY Selection'      To  ERP2FLDO
           MOVE  -1                            To  ENP2OPTL
           Go To ERROR-OUT.


       ER-POLICY.
           Move 'Policy Number must be numeric'   To  ERP2FLDO
           Go To ERROR-OUT.

       ER-CUSTOMER.
           Move 'Cust Number must be numeric'     To  ERP2FLDO
           Go To ERROR-OUT.

       ER-DATE-FORMAT.
           Move 'DATE format is YYYY-MM-DD'        To  ERP2FLDO.
           Go To ERROR-OUT.

       ER-DATE-NUMERIC.
           Move 'DATE YYYY,MM,DD must be numeric'  To  ERP2FLDO.
           Go To ERROR-OUT.

       ER-DATE-MONTH-RANGE.
           Move 'DATE MM (month) range 01-12'      To  ERP2FLDO.
           Go To ERROR-OUT.

       ER-DATE-DAY-RANGE.
           Move 'DATE DD (day) range 01-31'        To  ERP2FLDO.
           Go To ERROR-OUT.

       ER-DATE-31-DAYS.
           Move '31 DAYS not valid for month'      To  ERP2FLDO.
           Go To ERROR-OUT.

       ER-EXPIRATION-VS-ISSUE.
           Move 'Expiration Date must be > Issue Date'
                                                   To  ERP2FLDO.
           MOVE -1                                 To  ENP2EDAL.
           Go To ERROR-OUT.

       ER-TERM-YEARS.
           Move 'Term Years must be numeric'       To  ERP2FLDO.
           MOVE -1                                 To  ENP2TERL.
           Go To ERROR-OUT.

       ER-SUM-ASSURED.
           Move 'Sum Assured must be numeric'      To  ERP2FLDO.
           MOVE -1                                 To  ENP2SUML.
           Go To ERROR-OUT.

       ER-LIFE-ASSURED.
           Move 'Life Assured must be numeric'     To  ERP2FLDO.
           MOVE -1                                 To  ENP2LIFL.
           Go To ERROR-OUT.

       ER-WITH-PROFITS.
           Move 'With Profits must be Y or N'      To  ERP2FLDO.
           MOVE -1                                 To  ENP2WPRL.
           Go To ERROR-OUT.

       ER-EQUITIES.
           Move 'Equities must be Y or N'          To  ERP2FLDO.
           MOVE -1                                 To  ENP2EQUL.
           Go To ERROR-OUT.

       ER-MANAGED-FUNDS.
           Move 'Managed Funds must be Y or N'     To  ERP2FLDO.
           MOVE -1                                 To  ENP2MANL.
           Go To ERROR-OUT.

       ER-AUTO-PAY.
           Move 'Auto Pay must be Y or N'          To  ERP2FLDO.
           MOVE -1                                 To  ENP2PAYL.
           Go To ERROR-OUT.

       ER-AUTO-PAY1.
           Move 'No checking or credit fields are allowed'
                                                   To  ERP2FLDO.
           MOVE -1                                 To  ENP2PAYL.
           Go To ERROR-OUT.


       ER-AUTO-PAY2.
           Move 'Checking OR Credit allowed but not both'
                                                   To  ERP2FLDO.
           MOVE -1                                 To  ENP2PAYL.
           Go To ERROR-OUT.


       ER-AUTO-PAY3.
           Move 'Either checking or credit is required'
                                                   To  ERP2FLDO.
           MOVE -1                                 To  ENP2PAYL.
           Go To ERROR-OUT.

       ER-FROM-CHECKING.
           Move 'Checking must be numeric'         To  ERP2FLDO.
           MOVE -1                                 To  ENP2ACTL.
           Go To ERROR-OUT.


       ER-BANK-ROUTING.
           Move 'Bank Routing must be numeric'     To  ERP2FLDO.
           MOVE -1                                 To  ENP2ROUL.
           Go To ERROR-OUT.


       ER-CREDIT-CARD-TYPE.
           Move 'Credit card type is invalid'      To  ERP2FLDO.
           MOVE -1                                 To  ENP2CCTL.
           Go To ERROR-OUT.


       ER-CREDIT-CARD-NUMBER.
           Move 'Credit card number must be numeric'
                                                   To  ERP2FLDO.
           MOVE -1                                 To  ENP2CCNL.
           Go To ERROR-OUT.


       ER-CREDIT-CARD-PIN-NUMBER.
           Move 'Credit card PIN Number must be numeric'
                                                   To  ERP2FLDO.
           MOVE -1                                 To  ENP2CCPL.
           Go To ERROR-OUT.


       ER-VALID-THRU-MMYY-FORMAT.
           Move 'Valid Thru format is MM/YY'       To  ERP2FLDO.
           MOVE -1                                 To  ENP2CCVL.
           Go To ERROR-OUT.


       ER-VALID-THRU-NUMERIC.
           Move 'Valid Thru MM, YY must be numeric'
                                                   To  ERP2FLDO.
           MOVE -1                                 To  ENP2CCVL.
           Go To ERROR-OUT.


       ER-VALID-THRU-MM-MONTH-RANGE.
           Move 'DATE MM (month) range 01-12'      To  ERP2FLDO.
           MOVE -1                                 To  ENP2CCVL.
           Go To ERROR-OUT.


       NO-ADD.
           Evaluate CA-RETURN-CODE
             When 70
               Move 'Customer does not exist'     To ERP2FLDO
               Go To ERROR-OUT
             When Other
               Move 'Error Adding Life Policy'    To  ERP2FLDO
               Go To ERROR-OUT
           End-Evaluate.


       NO-UPD.
           Move 'Error Updating Life Policy'      To  ERP2FLDO
           Go To ERROR-OUT.

       NO-DELETE.
           Move 'Error Deleting Life Policy'      To  ERP2FLDO
           MOVE -1                                To  ENP2OPTL.
           Go To ERROR-OUT.


       NO-DATA.

           Move 'No data was returned.'            To  ERP2FLDO
           MOVE -1                                 To  ENP2PNOL.



           IF EIBAID NOT = DFHENTER
              MOVE '0000000000'   To ENP2PNOO
              MOVE '0000000000'   To ENP2CNOO.


           Initialize             ENP2IDAI
           Initialize             ENP2EDAI
           Initialize             ENP2FNMI
           Initialize             ENP2TERI
           Initialize             ENP2SUMI
           Initialize             ENP2LIFI
           Initialize             ENP2WPRI
           Initialize             ENP2MANI
           Initialize             ENP2EQUI
           Initialize             ENP2PAYI
           Initialize             ENP2ACTI
           Initialize             ENP2ROUI
           Initialize             ENP2CCTI
           Initialize             ENP2CCNI
           Initialize             ENP2CCPI
           Initialize             ENP2CCVI

           Go To ERROR-OUT.


       ERROR-OUT.
           EXEC CICS SEND MAP ('SSMAPP2')
                     FROM(SSMAPP2O)
                     MAPSET ('SSMAP')
                     CURSOR
           END-EXEC.

           Initialize SSMAPP2I.
           Initialize SSMAPP2O.
           Initialize COMM-AREA.

           GO TO ENDIT-STARTIT.

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
      *    INITIALIZE CHECKING, CREDIT SCREEN    *
      *    VARIABLES FOR LOW-VALUES.             *
      ********************************************

           IF ENP2ACTI = LOW-VALUES  MOVE SPACES TO ENP2ACTI.
           IF ENP2ROUI = LOW-VALUES  MOVE SPACES TO ENP2ROUI.
           IF ENP2CCTI = LOW-VALUES  MOVE SPACES TO ENP2CCTI.
           IF ENP2CCNI = LOW-VALUES  MOVE SPACES TO ENP2CCNI.
           IF ENP2CCPI = LOW-VALUES  MOVE SPACES TO ENP2CCPI.
           IF ENP2CCVI = LOW-VALUES  MOVE SPACES TO ENP2CCVI.


      ********************************************
      *    ISSUE DATE                            *
      ********************************************

           MOVE ENP2IDAI               TO WMF-DATE.


           IF WMF-DASH1 = '-'  AND
              WMF-DASH2 = '-'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP2IDAL
              GO TO ER-DATE-FORMAT.


           IF WMF-YEAR  NUMERIC    AND
              WMF-MONTH NUMERIC    AND
              WMF-DAY   NUMERIC
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP2IDAL
              GO TO ER-DATE-NUMERIC.


           IF (WMF-MONTH-R > 0) AND (WMF-MONTH-R < 13)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP2IDAL
              GO TO ER-VALID-THRU-MM-MONTH-RANGE.


           IF (WMF-DAY-R > 0) AND (WMF-DAY-R < 32)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP2IDAL
              GO TO ER-DATE-DAY-RANGE.


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
                  MOVE -1             TO ENP2IDAL
                  GO TO ER-DATE-31-DAYS
           ELSE
                  NEXT SENTENCE.


      ********************************************
      *    EXPIRATION DATE                       *
      ********************************************

           MOVE ENP2EDAI               TO WMF-DATE.


           IF WMF-DASH1 = '-'  AND
              WMF-DASH2 = '-'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP2EDAL
              GO TO ER-DATE-FORMAT.


           IF WMF-YEAR  NUMERIC    AND
              WMF-MONTH NUMERIC    AND
              WMF-DAY   NUMERIC
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP2EDAL
              GO TO ER-DATE-NUMERIC.


           IF (WMF-MONTH-R > 0) AND (WMF-MONTH-R < 13)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP2EDAL
              GO TO ER-DATE-MONTH-RANGE.


           IF (WMF-DAY-R > 0) AND (WMF-DAY-R < 32)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP2EDAL
              GO TO ER-DATE-DAY-RANGE.


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
                  MOVE -1             TO ENP2EDAL
                  GO TO ER-DATE-31-DAYS
           ELSE
                  NEXT SENTENCE.


      ********************************************
      * EXPIRATION DATE MUST BE > ISSUE DATE     *
      ********************************************

           IF (WS-EDIT-ERRORS = 'N')
               IF (ENP2EDAI > ENP2IDAI)
                   NEXT SENTENCE
               ELSE
                   GO TO ER-EXPIRATION-VS-ISSUE
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    TERM YEARS  -- VALUE MUST BE NUMERIC, RIGHT JUSTIFY        *
      *****************************************************************

           MOVE +2                     TO WMF-NUM-LTH
           MOVE ENP2TERO               TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP2TERL
              MOVE DFHBMFSE           TO ENP2TERA
              MOVE 'Y'                TO WS-EDIT-ERRORS
              GO TO ER-TERM-YEARS.


           MOVE WMF-NUM-OUTPUT        TO WMF-TERM-YEARS
           MOVE WMF-TERM-YEARS        TO ENP2TERO
                                         CA-E-TERM


      *****************************************************************
      *    SUM ASSURED -- VALUE MUST BE NUMERIC, RIGHT JUSTIFY        *
      *****************************************************************

           MOVE +6                     TO WMF-NUM-LTH
           MOVE ENP2SUMO               TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP2SUML
              MOVE DFHBMFSE           TO ENP2SUMA
              MOVE 'Y'                TO WS-EDIT-ERRORS
              GO TO ER-SUM-ASSURED.


           MOVE WMF-NUM-OUTPUT        TO WMF-SUM-ASSURED
           MOVE WMF-SUM-ASSURED       TO ENP2SUMO
                                         CA-E-SUM-ASSURED


      *****************************************************************
      *    LIFE ASSURED -- VALUE MUST BE NUMERIC, RIGHT JUSTIFY       *
      *****************************************************************

           MOVE +12                    TO WMF-NUM-LTH
           MOVE ENP2LIFO               TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP2LIFL
              MOVE DFHBMFSE           TO ENP2LIFA
              MOVE 'Y'                TO WS-EDIT-ERRORS
              GO TO ER-LIFE-ASSURED.


           MOVE WMF-NUM-OUTPUT        TO WMF-LIFE-ASSURED
           MOVE WMF-LIFE-ASSURED      TO ENP2LIFO
                                         CA-E-LIFE-ASSURED


      *****************************************************************
      *    WITH PROFITS   -- VALUE MUST BE   Y  OR  N                 *
      *****************************************************************

           IF ENP2WPRI =     'Y' OR
              ENP2WPRI =     'N'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              GO TO ER-WITH-PROFITS.


      *****************************************************************
      *    EQUITIES     -- VALUE MUST BE   Y  OR  N                   *
      *****************************************************************

           IF ENP2EQUI =     'Y' OR
              ENP2EQUI =     'N'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              GO TO ER-EQUITIES.


      *****************************************************************
      *    MANAGED FUNDS -- VALUE MUST BE   Y  OR  N                  *
      *****************************************************************

           IF ENP2MANI =     'Y' OR
              ENP2MANI =     'N'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              GO TO ER-MANAGED-FUNDS.


      *****************************************************************
      *    AUTO PAY       -- VALUE MUST BE   Y  OR  N                 *
      *****************************************************************

           IF ENP2PAYI =     'Y' OR
              ENP2PAYI =     'N'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              GO TO ER-AUTO-PAY.


      *****************************************************************
      *    AUTO PAY = N   -- checking, routing, card type, number,    *
      *                   -- pin, valid thru, NOT ALLOWED             *
      *****************************************************************

           IF ENP2PAYI = 'N'
               IF (ENP2ACTI > SPACES)        OR
                  (ENP2ROUI > SPACES)        OR
                  (ENP2CCTI > SPACES)        OR
                  (ENP2CCNI > SPACES)        OR
                  (ENP2CCPI > SPACES)        OR
                  (ENP2CCVI > SPACES)
                   MOVE 'Y'                  TO WS-EDIT-ERRORS
                   GO TO ER-AUTO-PAY1
               ELSE
                   NEXT SENTENCE
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    AUTO PAY = Y   -- Either checking or credit card info      *
      *                      is allowed but not both                  *
      *****************************************************************

           IF ENP2PAYI = 'Y'
               IF ((ENP2ACTI > SPACES)       OR
                   (ENP2ROUI > SPACES))      AND
                  ((ENP2CCTI > SPACES)       OR
                   (ENP2CCNI > SPACES)       OR
                   (ENP2CCPI > SPACES)       OR
                   (ENP2CCVI > SPACES))
                   MOVE 'Y'                  TO WS-EDIT-ERRORS
                   GO TO ER-AUTO-PAY2
               ELSE
                   NEXT SENTENCE
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    AUTO PAY = Y   -- Either checking or credit card info      *
      *                      is required                              *
      *****************************************************************

           IF ENP2PAYI = 'Y'
               IF ((ENP2ACTI = SPACES)       AND
                   (ENP2ROUI = SPACES))      AND
                  ((ENP2CCTI = SPACES)       AND
                   (ENP2CCNI = SPACES)       AND
                   (ENP2CCPI = SPACES)       AND
                   (ENP2CCVI = SPACES))
                   MOVE 'Y'                  TO WS-EDIT-ERRORS
                   GO TO ER-AUTO-PAY3
               ELSE
                   NEXT SENTENCE
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    FROM CHECKING   -- VALUE MUST BE NUMERIC                    *
      *****************************************************************

           IF (ENP2ACTI > SPACES)  OR
              (ENP2ROUI > SPACES)

               IF ENP2ACTI  NUMERIC
                   NEXT SENTENCE
               ELSE
                   MOVE 'Y'             TO WS-EDIT-ERRORS
                   GO TO ER-FROM-CHECKING
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    BANK ROUTING    -- VALUE MUST BE NUMERIC                    *
      *****************************************************************

           IF (ENP2ACTI > SPACES)  OR
              (ENP2ROUI > SPACES)

               IF ENP2ROUI  NUMERIC
                   NEXT SENTENCE
               ELSE
                   MOVE 'Y'             TO WS-EDIT-ERRORS
                   GO TO ER-BANK-ROUTING
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    CREDIT CARD TYPE                                           *
      *****************************************************************

           IF (ENP2ACTI = SPACES) AND
              (ENP2ROUI = SPACES) AND
              (ENP2PAYI = 'Y')
               IF ENP2CCTI = 'AMEX'           OR
                  ENP2CCTI = 'VISA'           OR
                  ENP2CCTI = 'MC'             OR
                  ENP2CCTI = 'DISC'
                      NEXT SENTENCE
               ELSE
                      MOVE 'Y'            TO WS-EDIT-ERRORS
                      GO TO ER-CREDIT-CARD-TYPE
           ELSE
                NEXT SENTENCE.


      *****************************************************************
      *    CREDIT CARD NUMBER                                         *
      *****************************************************************

           IF (ENP2ACTI = SPACES)  AND
              (ENP2ROUI = SPACES)  AND
              (ENP2PAYI = 'Y')
               IF ENP2CCNI  NUMERIC
                   NEXT SENTENCE
               ELSE
                   MOVE 'Y'             TO WS-EDIT-ERRORS
                   GO TO ER-CREDIT-CARD-NUMBER
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    PIN NUMBER                                                 *
      *****************************************************************

           IF (ENP2ACTI = SPACES)  AND
              (ENP2ROUI = SPACES)  AND
              (ENP2PAYI = 'Y')
               IF ENP2CCPI  NUMERIC
                   NEXT SENTENCE
               ELSE
                   MOVE 'Y'             TO WS-EDIT-ERRORS
                   GO TO ER-CREDIT-CARD-PIN-NUMBER
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    CREDIT CARD VALID THRU                                     *
      *****************************************************************

           IF (ENP2ACTI = SPACES)  AND
              (ENP2ROUI = SPACES)  AND
              (ENP2PAYI = 'Y')
               MOVE ENP2CCVI               TO WMF-DATE-MMYY
           ELSE
               GO TO P80000-EDIT-FIELDS-EXIT.


           IF WMF-SLASH1  = '/'
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-EDIT-ERRORS
               GO TO ER-VALID-THRU-MMYY-FORMAT.


           IF WMF-MM    NUMERIC    AND
              WMF-YY    NUMERIC
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-EDIT-ERRORS
               GO TO ER-VALID-THRU-NUMERIC.


           IF (WMF-MM-R > 0) AND (WMF-MM-R < 13)
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-EDIT-ERRORS
               GO TO ER-VALID-THRU-MM-MONTH-RANGE.


       P80000-EDIT-FIELDS-EXIT.
           EXIT.

