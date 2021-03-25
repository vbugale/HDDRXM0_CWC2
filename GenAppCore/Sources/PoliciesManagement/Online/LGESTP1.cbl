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
      *                    Motor Policy Menu                           *
      *                                                                *
      * Menu for Motor Policy Transactions                             *
      *                                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGTESTP1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 LGIPDB01                  PIC x(8) Value 'LGIPDB01'.

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

           05  WMF-CAR-YEAR            PIC 9(4).
           05  WMF-MAX-YEAR            PIC 9(4).
           05  WMF-PREMIUM             PIC 9(6).
           05  WMF-FROM-CHECKING       PIC 9(12).
           05  WMF-BANK-ROUTING        PIC 9(9).

           05  WMF-MAX-CAR-YEAR        PIC 9(5) COMP-3  VALUE 0.
           05  WMF-CURRENT-YEAR        PIC 9(5) COMP-3  VALUE 0.
           05  WMF-CURRENT-YEAR-R      REDEFINES WMF-CURRENT-YEAR
                                       PIC X(3).


      ******************************************************************
      *     Miscellaneous Work fields                                  *
      ******************************************************************

       01  WS-MISCELLANEOUS-FIELDS.
           05 MSG-TEXT                 PIC X(40) VALUE SPACES.

           05 MSG-CAR-YEAR-TEXT-GROUP.
              10 MSG-CAR-YEAR-TEXT     PIC X(33) VALUE
                 'Car Year numeric, may not exceed '.
              10 MSG-CAR-YEAR-MAX      PIC X(04).
              10 FILLER                PIC X(03) VALUE SPACES.


       01  WS-CASE-CONVERSION.
           05  WCC-UPPERCASE           PIC X(01).
           05  WCC-LOWERCASE           PIC X(01).


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

           IF EIBCALEN > 0
              GO TO A-GAIN.

           Initialize SSMAPP1I.
           Initialize SSMAPP1O.

           Initialize COMM-AREA.
           Initialize CA-MOTOR.

           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE.

           MOVE  SPACES            To ERP1FLDO.
           MOVE  DFHBMASB          To ERP1FLDA.
           MOVE  DFHBMASB          To ENP1CDTA.
           MOVE '0000000000'       To ENP1PNOO.
           MOVE  DFHBMFSE          To ENP1PNOA.
           MOVE '0000000000'       To ENP1CNOO.
           MOVE '000000'           To ENP1PREO.
           MOVE  DFHBMASK          To ENP1DATA.
           MOVE  '_'               To ENP1OPTO.


           MOVE WS-CURRENT-YEAR    To WMF-YEAR
           MOVE WS-CURRENT-MONTH   To WMF-MONTH
           MOVE WS-CURRENT-DAY     To WMF-DAY
           MOVE '-'                To WMF-DASH1
                                      WMF-DASH2
           MOVE WMF-DATE           To ENP1CDTO


      * Display Main Menu
           EXEC CICS SEND MAP ('SSMAPP1')
                     MAPSET ('SSMAP')
                     FROM   (SSMAPP1O)
                     ERASE
                     END-EXEC.


       A-GAIN.

           MOVE  SPACES        To ERP1FLDO.
           MOVE  DFHBMASK      To ENP1DATA.

           EXEC CICS HANDLE AID
                     CLEAR(CLEARIT)
                     PF3(ENDIT) END-EXEC.
           EXEC CICS HANDLE CONDITION
                     MAPFAIL(ENDIT)
                     END-EXEC.

           EXEC CICS RECEIVE MAP('SSMAPP1')
                     INTO(SSMAPP1I) ASIS
                     MAPSET('SSMAP') END-EXEC.


           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE.

           MOVE WS-CURRENT-YEAR    To WMF-YEAR
           MOVE WS-CURRENT-MONTH   To WMF-MONTH
           MOVE WS-CURRENT-DAY     To WMF-DAY
           MOVE '-'                To WMF-DASH1
                                      WMF-DASH2
           MOVE WMF-DATE           To ENP1CDTO


      *****************************************************************
      *    VALIDATE ATTENTION IDENTIFIER USAGE                        *
      *****************************************************************

            IF EIBAID  = DFHENTER  OR  DFHCLEAR  OR  DFHPF3 OR
                         DFHPF7    OR  DFHPF8    OR  DFHPF24
                NEXT SENTENCE
            ELSE
                GO TO ER-INVALID-PFKEY.


      *****************************************************************
      *    SCREEN OPTION EDIT                                         *
      *****************************************************************

           IF ENP1OPTO = '1' OR '2' OR '3' OR '4'
               NEXT SENTENCE
           ELSE
               Move 'Please enter a valid option'
                       To  ERP1FLDO
               Move -1 To  ENP1OPTL

               EXEC CICS SEND MAP ('SSMAPP1')
                         FROM(SSMAPP1O)
                         MAPSET ('SSMAP')
                         CURSOR
               END-EXEC
               GO TO ENDIT-STARTIT.


      ***NCR ADD OPT '4'
      *****************************************************************
      *    SCREEN OPTION VS PFKEY EDIT, PF24 USED FOR OPTION 3 and 4  *
      *****************************************************************


           IF  ((ENP1OPTO NOT = '3')  OR
                (ENP1OPTO NOT = '4'))
               IF EIBAID = DFHPF24
                    Move 'Invalid PFKEY Selection' To  ERP1FLDO
                    Move -1 To  ENP1OPTL

                    EXEC CICS SEND MAP ('SSMAPP1')
                         FROM(SSMAPP1O)
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
           MOVE ENP1PNOO               TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP1PNOL
              MOVE DFHBMFSE           TO ENP1PNOA
              GO TO ER-POLICY.


           MOVE WMF-NUM-OUTPUT        TO WMF-NUM-OUTPUT-910
           MOVE WMF-NUM-OUTPUT-910    TO ENP1PNOO
                                         CA-POLICY-NUM


      *****************************************************************
      *    SCREEN CUSTOMER NUMBER EDIT -- VALUE MUST BE NUMERIC       *
      *****************************************************************

           MOVE +10                    TO WMF-NUM-LTH
           MOVE ENP1CNOO               TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP1CNOL
              MOVE DFHBMFSE           TO ENP1CNOA
              GO TO ER-CUSTOMER.


           MOVE WMF-NUM-OUTPUT        TO WMF-NUM-OUTPUT-910
           MOVE WMF-NUM-OUTPUT-910    TO ENP1CNOO
                                         CA-CUSTOMER-NUM


      *****************************************************************
      *    SCREEN OPTION 3 (DELETE) REQUIRES AUTHORIZATION            *
      *****************************************************************

           IF (ENP1OPTO = '3')
               IF (EIBAID = DFHPF24)
                   NEXT SENTENCE
               ELSE
                   Move 'DELETE REQUIRES AUTHORIZATION'
                           To  ERP1FLDO
                   Move -1 To  ENP1OPTL

                   EXEC CICS SEND MAP ('SSMAPP1')
                             FROM(SSMAPP1O)
                             MAPSET ('SSMAP')
                             CURSOR
                   END-EXEC
                   GO TO ENDIT-STARTIT
           ELSE
                   NEXT SENTENCE.


      *** NCR    ADD SECTION FOR UPDATE
	  *****************************************************************
      *    SCREEN OPTION 4 (UPDATE) REQUIRES AUTHORIZATION            *
      *****************************************************************

           IF (ENP1OPTO = '4')
               IF (EIBAID = DFHPF24)
                   NEXT SENTENCE
               ELSE
                   Move 'UPDATE REQUIRES AUTHORIZATION'
                           To  ERP1FLDO
                   Move -1 To  ENP1OPTL

                   EXEC CICS SEND MAP ('SSMAPP1')
                             FROM(SSMAPP1O)
                             MAPSET ('SSMAP')
                             CURSOR
                   END-EXEC
                   GO TO ENDIT-STARTIT
           ELSE
                   NEXT SENTENCE.

      *****************************************************************
      *    CHECK FOR SCROLLING PF7-BACKWARD, PF8-FORWARD              *
      *****************************************************************

           IF EIBAID  = DFHPF7  OR  DFHPF8
                Perform SCROLL-PROCESS
                Move '1'  To ENP1OPTO
                Move ENP1PNOO        To CA-POLICY-NUM
                Move ENP1CNOO        To CA-CUSTOMER-NUM
           END-IF


      *****************************************************************
      *    PROCESS USER OPTION SELECTION                              *
      *****************************************************************

           EVALUATE ENP1OPTO

             WHEN '1'
                 Move '01IMOT'   To CA-REQUEST-ID
                 Move ENP1CNOO   To CA-CUSTOMER-NUM
                 Move ENP1PNOO   To CA-POLICY-NUM
                 EXEC CICS LINK PROGRAM('LGIPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC


                 IF CA-POLICY-NUM  = 0
                    MOVE 0       To CA-CUSTOMER-NUM
                    GO TO NO-DATA
                 END-IF


                 IF CA-POLICY-NUM  =  CA-POLICY-BEGIN
                    MOVE 'Start of data'     To  ENP1DATO
                 END-IF

                 IF CA-POLICY-NUM  =  CA-POLICY-END
                    MOVE 'End of data'       To  ENP1DATO
                 END-IF


                 IF CA-RETURN-CODE > 0
                   GO TO NO-DATA
                 END-IF


      *****************************************************************
      *    CALCULATE THE MAXIMUM CAR YEAR VALUE ALLOWED               *
      *****************************************************************

                 COMPUTE WMF-MAX-CAR-YEAR =  WMF-CURRENT-YEAR + 1


                 Move CA-CUSTOMER-NUM        To  ENP1CNOO
                 Move CA-POLICY-NUM          To  ENP1PNOO
                 Move CA-ISSUE-DATE          To  ENP1IDAI
                 Move CA-EXPIRY-DATE         To  ENP1EDAI
                 Move CA-M-CAR-YEAR          To  ENP1YEAI
                 Move CA-M-MAKE              To  ENP1CMKI
                 Move CA-M-MODEL             To  ENP1CMOI
                 Move CA-M-COLOUR            To  ENP1COLI
                 Move CA-M-CAR-VIN           To  ENP1VINI
                 Move CA-M-PREMIUM           To  ENP1PREI
                 Move CA-M-AUTO-PAY          To  ENP1PAYI
                 Move CA-M-CHECK-ACCT-NBR    To  ENP1ACTI
                 Move CA-M-BANK-ROUTE-CODE   To  ENP1ROUI
                 Move CA-M-CREDIT-CARD-TYP   To  ENP1CCTI
                 Move CA-M-CREDIT-CARD-NBR   To  ENP1CCNI
                 Move CA-M-CREDIT-CARD-PIN   To  ENP1CCPI
                 Move CA-M-CREDIT-CARD-VAL   To  ENP1CCVI



                 EXEC CICS SEND MAP ('SSMAPP1')
                           FROM(SSMAPP1O)
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


                 Move '01AMOT'          To CA-REQUEST-ID
                 Move ENP1CNOI          To CA-CUSTOMER-NUM
                 Move 0                 To CA-PAYMENT
                 Move 0                 To CA-BROKERID
                 Move '        '        To CA-BROKERSREF
                 Move ENP1IDAI          To CA-ISSUE-DATE
                 Move ENP1EDAI          To CA-EXPIRY-DATE
                 Move ENP1YEAI          To CA-M-CAR-YEAR
                 Move ENP1CMKI          To CA-M-MAKE
                 Move ENP1CMOI          To CA-M-MODEL
                 Move ENP1COLI          To CA-M-COLOUR
                 Move ENP1VINI          To CA-M-CAR-VIN
                 Move ENP1PREI          To CA-M-PREMIUM
                 Move ENP1PAYI          To CA-M-AUTO-PAY
                 Move ENP1ACTI          To CA-M-CHECK-ACCT-NBR
                 Move ENP1ROUI          To CA-M-BANK-ROUTE-CODE
                 Move ENP1CCTI          To CA-M-CREDIT-CARD-TYP
                 Move ENP1CCNI          To CA-M-CREDIT-CARD-NBR
                 Move ENP1CCPI          To CA-M-CREDIT-CARD-PIN
                 Move ENP1CCVI          To CA-M-CREDIT-CARD-VAL


                 EXEC CICS LINK PROGRAM('LGAPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC

                 IF CA-RETURN-CODE > 0
                   Exec CICS Syncpoint Rollback End-Exec
                   GO TO NO-ADD
                 END-IF

                 Move CA-CUSTOMER-NUM To ENP1CNOI
                 Move CA-POLICY-NUM   To ENP1PNOI
                 Move ' '             To ENP1OPTI
                 Move 'New Motor Policy Inserted'
                   To  ERP1FLDO
                 EXEC CICS SEND MAP ('SSMAPP1')
                           FROM(SSMAPP1O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT


             WHEN '3'
                 Move '01DMOT'   To CA-REQUEST-ID
                 Move ENP1CNOO   To CA-CUSTOMER-NUM
                 Move ENP1PNOO   To CA-POLICY-NUM
                 EXEC CICS LINK PROGRAM('LGDPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC
                 IF CA-RETURN-CODE > 0
                   Exec CICS Syncpoint Rollback End-Exec
                   GO TO NO-DELETE
                 END-IF

                 Move Spaces            To  ENP1IDAI
                 Move Spaces            To  ENP1EDAI
                 Move Spaces            To  ENP1YEAI
                 Move Spaces            To  ENP1CMKI
                 Move Spaces            To  ENP1CMOI
                 Move Spaces            To  ENP1COLI
                 Move Spaces            To  ENP1VINI
                 Move Spaces            To  ENP1CMKI
                 Move Spaces            To  ENP1CMOI
                 Move Spaces            To  ENP1PREI
                 Move Spaces            To  ENP1PAYI
                 Move Spaces            To  ENP1ACTI
                 Move Spaces            To  ENP1ROUI
                 Move Spaces            To  ENP1CCTI
                 Move Spaces            To  ENP1CCNI
                 Move Spaces            To  ENP1CCPI
                 Move Spaces            To  ENP1CCVI


                 Move 'Motor Policy Deleted'
                   To  ERP1FLDO
                 EXEC CICS SEND MAP ('SSMAPP1')
                           FROM(SSMAPP1O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT


             WHEN '4'
                 Move '01IMOT'   To CA-REQUEST-ID
                 Move ENP1CNOO   To CA-CUSTOMER-NUM
                 Move ENP1PNOO   To CA-POLICY-NUM
                 EXEC CICS LINK PROGRAM('LGIPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC
                 IF CA-RETURN-CODE > 0
                   GO TO NO-DATA
                 END-IF


                 Move CA-ISSUE-DATE          To  ENP1IDAI
                 Move CA-EXPIRY-DATE         To  ENP1EDAI
                 Move CA-M-CAR-YEAR          To  ENP1YEAI
                 Move CA-M-MAKE              To  ENP1CMKI
                 Move CA-M-MODEL             To  ENP1CMOI
                 Move CA-M-COLOUR            To  ENP1COLI
                 Move CA-M-CAR-VIN           To  ENP1VINI
                 Move CA-M-PREMIUM           To  ENP1PREI
                 Move CA-M-AUTO-PAY          To  ENP1PAYI
                 Move CA-M-CHECK-ACCT-NBR    To  ENP1ACTI
                 Move CA-M-BANK-ROUTE-CODE   To  ENP1ROUI
                 Move CA-M-CREDIT-CARD-TYP   To  ENP1CCTI
                 Move CA-M-CREDIT-CARD-NBR   To  ENP1CCNI
                 Move CA-M-CREDIT-CARD-PIN   To  ENP1CCPI
                 Move CA-M-CREDIT-CARD-VAL   To  ENP1CCVI


                 EXEC CICS SEND MAP ('SSMAPP1')
                           FROM(SSMAPP1O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 EXEC CICS RECEIVE MAP('SSMAPP1')
                           INTO(SSMAPP1I)
                           MAPSET('SSMAP') END-EXEC

                 Move '01UMOT'          To CA-REQUEST-ID
                 Move ENP1CNOI          To CA-CUSTOMER-NUM
                 Move 0                 To CA-PAYMENT
                 Move 0                 To CA-BROKERID
                 Move '        '        To CA-BROKERSREF
                 Move ENP1IDAI          To CA-ISSUE-DATE
                 Move ENP1EDAI          To CA-EXPIRY-DATE
                 Move ENP1YEAI          To CA-M-CAR-YEAR
                 Move ENP1CMKI          To CA-M-MAKE
                 Move ENP1CMOI          To CA-M-MODEL
                 Move ENP1COLI          To CA-M-COLOUR
                 Move ENP1VINI          To CA-M-CAR-VIN
                 Move ENP1PREI          To CA-M-PREMIUM
                 Move ENP1PAYI          To CA-M-AUTO-PAY
                 Move ENP1ACTI          To CA-M-CHECK-ACCT-NBR
                 Move ENP1ROUI          To CA-M-BANK-ROUTE-CODE
                 Move ENP1CCTI          To CA-M-CREDIT-CARD-TYP
                 Move ENP1CCNI          To CA-M-CREDIT-CARD-NBR
                 Move ENP1CCPI          To CA-M-CREDIT-CARD-PIN
                 Move ENP1CCVI          To CA-M-CREDIT-CARD-VAL


                 EXEC CICS LINK PROGRAM('LGUPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC
                 IF CA-RETURN-CODE > 0
                   GO TO NO-UPD
                 END-IF

                 Move CA-CUSTOMER-NUM To ENP1CNOI
                 Move CA-POLICY-NUM   To ENP1PNOI
                 Move ' '             To ENP1OPTI
                 Move 'Motor Policy Updated'
                   To  ERP1FLDO
                 EXEC CICS SEND MAP ('SSMAPP1')
                           FROM(SSMAPP1O)
                           MAPSET ('SSMAP')
                 END-EXEC

                 GO TO ENDIT-STARTIT

             WHEN OTHER

                 Move 'Please enter a valid option'
                   To  ERP1FLDO
                 Move -1 To ENP1OPTL

                 EXEC CICS SEND MAP ('SSMAPP1')
                           FROM(SSMAPP1O)
                           MAPSET ('SSMAP')
                           CURSOR
                 END-EXEC
                 GO TO ENDIT-STARTIT

           END-EVALUATE.


      *    Send message to terminal and return

           EXEC CICS RETURN
           END-EXEC.

       ENDIT-STARTIT.
           MOVE  DFHBMASK      To ENP1DATA.
           EXEC CICS RETURN
                TRANSID('SSP1')
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

           Initialize SSMAPP1I.
           EXEC CICS SEND MAP ('SSMAPP1')
                     MAPSET ('SSMAP')
                     MAPONLY
           END-EXEC

           EXEC CICS RETURN
                TRANSID('SSP1')
                COMMAREA(COMM-AREA)
                END-EXEC.

      *****PWB ****************************
       SCROLL-PROCESS.

           IF EIBAID  = DFHPF7
               Move '07'          To CA-POLICY-PFKEY
           END-IF

           IF EIBAID  = DFHPF8
               Move '08'          To CA-POLICY-PFKEY
           END-IF.


      *****MAY NOT NEED THIS SSP1 OPERATES DIFFERENT VS SSC1
      *****EXEC CICS LINK Program(LGIPDB01)
      *****    Commarea(COMM-AREA)
      *****    LENGTH(32500)
      *****END-EXEC.

      *****PWB ****************************


       ER-INVALID-PFKEY.
           Move 'Invalid PFKEY Selection'      To  ERP1FLDO
           MOVE  -1                            To  ENP1OPTL
           Go To ERROR-OUT.


       ER-POLICY.
           Move 'Policy Number must be numeric'    To  ERP1FLDO
           Go To ERROR-OUT.

       ER-CUSTOMER.
           Move 'Cust Number must be numeric'      To  ERP1FLDO
           Go To ERROR-OUT.

       ER-DATE-FORMAT.
           Move 'DATE format is YYYY-MM-DD'        To  ERP1FLDO.
           Go To ERROR-OUT.

       ER-DATE-NUMERIC.
           Move 'DATE YYYY,MM,DD must be numeric'  To  ERP1FLDO.
           Go To ERROR-OUT.

       ER-DATE-MONTH-RANGE.
           Move 'DATE MM (month) range 01-12'      To  ERP1FLDO.
           Go To ERROR-OUT.

       ER-DATE-DAY-RANGE.
           Move 'DATE DD (day) range 01-31'        To  ERP1FLDO.
           Go To ERROR-OUT.


       ER-DATE-31-DAYS.
           Move '31 DAYS not valid for month'      To  ERP1FLDO.
           Go To ERROR-OUT.


       ER-EXPIRATION-VS-ISSUE.
           Move 'Expiration Date must be > Issue Date'
                                                   To  ERP1FLDO.
           MOVE -1                                 To  ENP1EDAL.
           Go To ERROR-OUT.


       ER-CAR-YEAR.
           Move WMF-MAX-YEAR                    To  MSG-CAR-YEAR-MAX.
           Move MSG-CAR-YEAR-TEXT-GROUP         To  ERP1FLDO.
           MOVE -1                              To  ENP1YEAL.
           Go To ERROR-OUT.


       ER-PREMIUM.
           Move 'Policy Premium must be numeric'   To  ERP1FLDO.
           MOVE -1                                 To  ENP1PREL.
           Go To ERROR-OUT.


       ER-AUTO-PAY.
           Move 'Auto Pay must be Y or N'          To  ERP1FLDO.
           MOVE -1                                 To  ENP1PAYL.
           Go To ERROR-OUT.


       ER-AUTO-PAY1.
           Move 'No checking or credit fields are allowed'
                                                   To  ERP1FLDO.
           MOVE -1                                 To  ENP1PAYL.
           Go To ERROR-OUT.


       ER-AUTO-PAY2.
           Move 'Checking OR Credit allowed but not both'
                                                   To  ERP1FLDO.
           MOVE -1                                 To  ENP1PAYL.
           Go To ERROR-OUT.


       ER-AUTO-PAY3.
           Move 'Either checking or credit is required'
                                                   To  ERP1FLDO.
           MOVE -1                                 To  ENP1PAYL.
           Go To ERROR-OUT.


       ER-FROM-CHECKING.
           Move 'Checking must be numeric'         To  ERP1FLDO.
           MOVE -1                                 To  ENP1ACTL.
           Go To ERROR-OUT.


       ER-BANK-ROUTING.
           Move 'Bank Routing must be numeric'     To  ERP1FLDO.
           MOVE -1                                 To  ENP1ROUL.
           Go To ERROR-OUT.


       ER-CREDIT-CARD-TYPE.
           Move 'Credit card type is invalid'      To  ERP1FLDO.
           MOVE -1                                 To  ENP1CCTL.
           Go To ERROR-OUT.


       ER-CREDIT-CARD-NUMBER.
           Move 'Credit card number must be numeric'
                                                   To  ERP1FLDO.
           MOVE -1                                 To  ENP1CCNL.
           Go To ERROR-OUT.


       ER-CREDIT-CARD-PIN-NUMBER.
           Move 'Credit card PIN Number must be numeric'
                                                   To  ERP1FLDO.
           MOVE -1                                 To  ENP1CCPL.
           Go To ERROR-OUT.


       ER-VALID-THRU-MMYY-FORMAT.
           Move 'Valid Thru format is MM/YY'       To  ERP1FLDO.
           MOVE -1                                 To  ENP1CCVL.
           Go To ERROR-OUT.


       ER-VALID-THRU-NUMERIC.
           Move 'Valid Thru MM, YY must be numeric'
                                                   To  ERP1FLDO.
           MOVE -1                                 To  ENP1CCVL.
           Go To ERROR-OUT.


       ER-VALID-THRU-MM-MONTH-RANGE.
           Move 'DATE MM (month) range 01-12'      To  ERP1FLDO.
           MOVE -1                                 To  ENP1CCVL.
           Go To ERROR-OUT.


       NO-ADD.
           Evaluate CA-RETURN-CODE
             When 70
               Move 'Customer does not exist'    To  ERP1FLDO
               Go To ERROR-OUT
             When Other
               Move 'Error Adding Motor Policy'  To  ERP1FLDO
               Go To ERROR-OUT
           End-Evaluate.


       NO-UPD.
           Move 'Error Updating Motor Policy'    To  ERP1FLDO
           Go To ERROR-OUT.

       NO-DELETE.
           Move 'Error Deleting Motor Policy'    To  ERP1FLDO
           Move -1 To  ENP1OPTL
           Go To ERROR-OUT.


       NO-DATA.
           Move 'No data was returned.'          To ERP1FLDO
           MOVE -1                               To  ENP1PNOL.


           IF EIBAID NOT = DFHENTER
               MOVE '0000000000'                 To ENP1PNOO
               MOVE '0000000000'                 To ENP1CNOO.


           Initialize                           ENP1IDAI
           Initialize                           ENP1EDAI
           Initialize                           ENP1YEAI
           Initialize                           ENP1CMKI
           Initialize                           ENP1CMOI
           Initialize                           ENP1COLI
           Initialize                           ENP1VINI
           Initialize                           ENP1PREI
           Initialize                           ENP1PAYI
           Initialize                           ENP1ACTI
           Initialize                           ENP1ROUI
           Initialize                           ENP1CCTI
           Initialize                           ENP1CCNI
           Initialize                           ENP1CCPI
           Initialize                           ENP1CCVI

           Go To ERROR-OUT.


       ERROR-OUT.
           EXEC CICS SEND MAP ('SSMAPP1')
                     FROM(SSMAPP1O)
                     MAPSET ('SSMAP')
                     CURSOR
           END-EXEC.

           Initialize SSMAPP1I.
           Initialize SSMAPP1O.
           Initialize COMM-AREA.
           Initialize CA-MOTOR.

           MOVE DFHBMFSE       To ENP1PNOA.

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

           IF ENP1ACTI = LOW-VALUES  MOVE SPACES TO ENP1ACTI.
           IF ENP1ROUI = LOW-VALUES  MOVE SPACES TO ENP1ROUI.
           IF ENP1CCTI = LOW-VALUES  MOVE SPACES TO ENP1CCTI.
           IF ENP1CCNI = LOW-VALUES  MOVE SPACES TO ENP1CCNI.
           IF ENP1CCPI = LOW-VALUES  MOVE SPACES TO ENP1CCPI.
           IF ENP1CCVI = LOW-VALUES  MOVE SPACES TO ENP1CCVI.


      ********************************************
      *    ISSUE DATE                            *
      ********************************************

           MOVE ENP1IDAI               TO WMF-DATE.


           IF WMF-DASH1 = '-'  AND
              WMF-DASH2 = '-'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP1IDAL
              GO TO ER-DATE-FORMAT.


           IF WMF-YEAR  NUMERIC    AND
              WMF-MONTH NUMERIC    AND
              WMF-DAY   NUMERIC
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP1IDAL
              GO TO ER-DATE-NUMERIC.


           IF (WMF-MONTH-R > 0) AND (WMF-MONTH-R < 13)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP1IDAL
              GO TO ER-VALID-THRU-MM-MONTH-RANGE.


           IF (WMF-DAY-R > 0) AND (WMF-DAY-R < 32)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP1IDAL
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
                  MOVE -1             TO ENP1IDAL
                  GO TO ER-DATE-31-DAYS
           ELSE
                  NEXT SENTENCE.


      ********************************************
      *    EXPIRATION DATE                       *
      ********************************************

           MOVE ENP1EDAI               TO WMF-DATE.


           IF WMF-DASH1 = '-'  AND
              WMF-DASH2 = '-'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP1EDAL
              GO TO ER-DATE-FORMAT.


           IF WMF-YEAR  NUMERIC    AND
              WMF-MONTH NUMERIC    AND
              WMF-DAY   NUMERIC
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP1EDAL
              GO TO ER-DATE-NUMERIC.


           IF (WMF-MONTH-R > 0) AND (WMF-MONTH-R < 13)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP1EDAL
              GO TO ER-DATE-MONTH-RANGE.


           IF (WMF-DAY-R > 0) AND (WMF-DAY-R < 32)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP1EDAL
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
                  MOVE -1             TO ENP1EDAL
                  GO TO ER-DATE-31-DAYS
           ELSE
                  NEXT SENTENCE.


      ********************************************
      * EXPIRATION DATE MUST BE > ISSUE DATE     *
      ********************************************

           IF (WS-EDIT-ERRORS = 'N')
               IF (ENP1EDAI > ENP1IDAI)
                   NEXT SENTENCE
               ELSE
                   GO TO ER-EXPIRATION-VS-ISSUE
           ELSE
                   NEXT SENTENCE.


      ********************************************
      * CAR YEAR - NUMERIC AND < CURR YEAR +2    *
      ********************************************

           MOVE WS-CURRENT-YEAR    TO WMF-MAX-YEAR.
           ADD +1                  TO WMF-MAX-YEAR.

           IF ENP1YEAI NUMERIC
               MOVE ENP1YEAI           TO WMF-CAR-YEAR
               IF WMF-CAR-YEAR  > WMF-MAX-YEAR
                  MOVE 'Y'             TO WS-EDIT-ERRORS
                  GO TO ER-CAR-YEAR
               ELSE
                  NEXT SENTENCE
           ELSE
                  MOVE 'Y'             TO WS-EDIT-ERRORS
                  GO TO ER-CAR-YEAR.


      *****************************************************************
      *    POLICY PREMIUM -- VALUE MUST BE NUMERIC                    *
      *****************************************************************


           MOVE +6  TO WMF-NUM-LTH
           MOVE ENP1PREI               TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE 'Y'                TO WS-EDIT-ERRORS
              GO TO ER-PREMIUM.

           MOVE WMF-NUM-OUTPUT        TO WMF-PREMIUM.
           MOVE WMF-PREMIUM           TO ENP1PREO.



      *****************************************************************
      *    AUTO PAY       -- VALUE MUST BE   Y  OR  N                 *
      *****************************************************************

           MOVE ENP1PAYI               TO WCC-LOWERCASE.

           MOVE FUNCTION Upper-case(WCC-LOWERCASE) TO WCC-UPPERCASE.
           MOVE WCC-UPPERCASE  TO ENP1PAYI.


           IF ENP1PAYI =     'Y' OR
              ENP1PAYI =     'N'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              GO TO ER-AUTO-PAY.


      *****************************************************************
      *    AUTO PAY = N   -- checking, routing, card type, number,    *
      *                   -- pin, valid thru, NOT ALLOWED             *
      *****************************************************************

           IF ENP1PAYI = 'N'
               IF (ENP1ACTI > SPACES)        OR
                  (ENP1ROUI > SPACES)        OR
                  (ENP1CCTI > SPACES)        OR
                  (ENP1CCNI > SPACES)        OR
                  (ENP1CCPI > SPACES)        OR
                  (ENP1CCVI > SPACES)
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

           IF ENP1PAYI = 'Y'
               IF ((ENP1ACTI > SPACES)       OR
                   (ENP1ROUI > SPACES))      AND
                  ((ENP1CCTI > SPACES)       OR
                   (ENP1CCNI > SPACES)       OR
                   (ENP1CCPI > SPACES)       OR
                   (ENP1CCVI > SPACES))
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

           IF ENP1PAYI = 'Y'
               IF ((ENP1ACTI = SPACES)       AND
                   (ENP1ROUI = SPACES))      AND
                  ((ENP1CCTI = SPACES)       AND
                   (ENP1CCNI = SPACES)       AND
                   (ENP1CCPI = SPACES)       AND
                   (ENP1CCVI = SPACES))
                   MOVE 'Y'                  TO WS-EDIT-ERRORS
                   GO TO ER-AUTO-PAY3
               ELSE
                   NEXT SENTENCE
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    FROM CHECKING   -- VALUE MUST BE NUMERIC                    *
      *****************************************************************

           IF (ENP1ACTI > SPACES)  OR
              (ENP1ROUI > SPACES)

               IF ENP1ACTI  NUMERIC
                   NEXT SENTENCE
               ELSE
                   MOVE 'Y'             TO WS-EDIT-ERRORS
                   GO TO ER-FROM-CHECKING
           ELSE
                   NEXT SENTENCE.



      **        MOVE +12  TO WMF-NUM-LTH
      **        MOVE ENP1ACTI TO WMF-NUM-INPUT
      **
      **        PERFORM  P70500-EDIT-NUMERIC-FIELD
      **            THRU P70500-EDIT-NUMERIC-FIELD-EXIT
      **
      **        IF WMF-NUM-ERROR           >  ZEROES
      **           MOVE 'Y'                TO WS-EDIT-ERRORS
      **           GO TO ER-FROM-CHECKING
      **        ELSE
      **           MOVE WMF-NUM-OUTPUT        TO WMF-FROM-CHECKING
      **           MOVE WMF-FROM-CHECKING     TO ENP1ACTO
      **   ELSE
      **           NEXT SENTENCE.


      *****************************************************************
      *    BANK ROUTING    -- VALUE MUST BE NUMERIC                    *
      *****************************************************************

           IF (ENP1ACTI > SPACES)  OR
              (ENP1ROUI > SPACES)

               IF ENP1ROUI  NUMERIC
                   NEXT SENTENCE
               ELSE
                   MOVE 'Y'             TO WS-EDIT-ERRORS
                   GO TO ER-BANK-ROUTING
           ELSE
                   NEXT SENTENCE.


      **       MOVE +9  TO WMF-NUM-LTH
      **       MOVE ENP1ROUI               TO WMF-NUM-INPUT
      **
      **       PERFORM  P70500-EDIT-NUMERIC-FIELD
      **           THRU P70500-EDIT-NUMERIC-FIELD-EXIT
      **
      **       IF WMF-NUM-ERROR           >  ZEROES
      **           MOVE 'Y'                TO WS-EDIT-ERRORS
      **           GO TO ER-BANK-ROUTING
      **       ELSE
      **           MOVE WMF-NUM-OUTPUT        TO WMF-BANK-ROUTING
      **           MOVE WMF-BANK-ROUTING      TO ENP1ROUO
      **   ELSE
      **           NEXT SENTENCE.

      *****************************************************************
      *    CREDIT CARD TYPE                                           *
      *****************************************************************

           IF (ENP1ACTI = SPACES) AND
              (ENP1ROUI = SPACES) AND
              (ENP1PAYI = 'Y')
               IF ENP1CCTI = 'AMEX'           OR
                  ENP1CCTI = 'VISA'           OR
                  ENP1CCTI = 'MC'             OR
                  ENP1CCTI = 'DISCOVER'
                      NEXT SENTENCE
               ELSE
                      MOVE 'Y'            TO WS-EDIT-ERRORS
                      GO TO ER-CREDIT-CARD-TYPE
           ELSE
                NEXT SENTENCE.


      *****************************************************************
      *    CREDIT CARD NUMBER                                         *
      *****************************************************************

           IF (ENP1ACTI = SPACES)  AND
              (ENP1ROUI = SPACES)  AND
              (ENP1PAYI = 'Y')
               IF ENP1CCNI  NUMERIC
                   NEXT SENTENCE
               ELSE
                   MOVE 'Y'             TO WS-EDIT-ERRORS
                   GO TO ER-CREDIT-CARD-NUMBER
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    PIN NUMBER                                                 *
      *****************************************************************

           IF (ENP1ACTI = SPACES)  AND
              (ENP1ROUI = SPACES) AND
              (ENP1PAYI = 'Y')
               IF ENP1CCPI  NUMERIC
                   NEXT SENTENCE
               ELSE
                   MOVE 'Y'             TO WS-EDIT-ERRORS
                   GO TO ER-CREDIT-CARD-PIN-NUMBER
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    CREDIT CARD VALID THRU                                     *
      *****************************************************************

           IF (ENP1ACTI = SPACES)  AND
              (ENP1ROUI = SPACES)  AND
              (ENP1PAYI = 'Y')
               MOVE ENP1CCVI               TO WMF-DATE-MMYY
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

