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
      *                    House Policy Menu                           *
      *                                                                *
      * Menu for House Policy Transactions                             *
      *                                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGTESTP3.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77 MSGEND                       PIC X(24) VALUE
                                        'Transaction ended      '.

       77 WS-EDIT-ERRORS               PIC X(01) VALUE 'N'.


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

           05  WMF-BEDROOMS            PIC 9(3).
           05  WMF-HOUSE-VALUE         PIC 9(8).
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

           Initialize SSMAPP3I.
           Initialize SSMAPP3O.
           Initialize COMM-AREA.


            MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE.


           MOVE '0000000000'   To ENP3PNOO.
           MOVE  DFHBMFSE      To ENP3PNOA.
           MOVE '0000000000'   To ENP3CNOO.
           MOVE '000000000'    To ENP3VALO.
           MOVE '000'          To ENP3BEDO.
           MOVE  DFHBMASK      To ENP3DATA.
           MOVE  DFHBMASB      To ERP3FLDA.
           MOVE  DFHBMASB      To ENP3CDTA.
           MOVE  '_'           To ENP3OPTO.

           MOVE WS-CURRENT-YEAR    To WMF-YEAR
           MOVE WS-CURRENT-MONTH   To WMF-MONTH
           MOVE WS-CURRENT-DAY     To WMF-DAY
           MOVE '-'                To WMF-DASH1
                                      WMF-DASH2
           MOVE WMF-DATE           To ENP3CDTO


      * Display Main Menu
           EXEC CICS SEND MAP ('SSMAPP3')
                     MAPSET ('SSMAP')
                     ERASE
                     END-EXEC.


       A-GAIN.

           MOVE  DFHBMASK      To ENP3DATA.

           EXEC CICS HANDLE AID
                     CLEAR(CLEARIT)
                     PF3(ENDIT) END-EXEC.
           EXEC CICS HANDLE CONDITION
                     MAPFAIL(ENDIT)
                     END-EXEC.


           EXEC CICS RECEIVE MAP('SSMAPP3')
                     INTO(SSMAPP3I)
                     MAPSET('SSMAP') END-EXEC.



           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE.

           MOVE WS-CURRENT-YEAR    To WMF-YEAR
           MOVE WS-CURRENT-MONTH   To WMF-MONTH
           MOVE WS-CURRENT-DAY     To WMF-DAY
           MOVE '-'                To WMF-DASH1
                                      WMF-DASH2
           MOVE WMF-DATE           To ENP3CDTO


      ****************************************************************
      *    VALIDATE ATTENTION IDENTIFIER USAGE                       *
      ****************************************************************

           IF EIBAID  = DFHENTER  OR  DFHCLEAR  OR  DFHPF3 OR
                        DFHPF7    OR  DFHPF8 OR  DFHPF24
               NEXT SENTENCE
           ELSE
               GO TO ER-INVALID-PFKEY.


      *****************************************************************
      *    SCREEN OPTION EDIT                                         *
      *****************************************************************

           IF ENP3OPTO = '1' OR '2' OR '3' OR '4'
                 NEXT SENTENCE
           ELSE
                 Move 'Please enter a valid option'
                             To  ERP3FLDO
                 Move -1 To  ENP3OPTL
                 EXEC CICS SEND MAP ('SSMAPP3')
                           FROM(SSMAPP3O)
                               MAPSET ('SSMAP')
                               CURSOR
                 END-EXEC
                 GO TO ENDIT-STARTIT.


      *****************************************************************
      *    SCREEN OPTION VS PFKEY EDIT, PF24 ONLY USED FOR OPTION 3   *
      *****************************************************************

           IF ENP3OPTO NOT = '3'
               IF EIBAID = DFHPF24
                    Move 'Invalid PFKEY Selection' To  ERP3FLDO
                    Move -1 To  ENP3OPTL

                    EXEC CICS SEND MAP ('SSMAPP3')
                         FROM(SSMAPP3O)
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
           MOVE ENP3PNOO               TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP3PNOL
              MOVE DFHBMFSE           TO ENP3PNOA
              GO TO ER-POLICY.


           MOVE WMF-NUM-OUTPUT        TO WMF-NUM-OUTPUT-910
           MOVE WMF-NUM-OUTPUT-910    TO ENP3PNOO
                                         CA-POLICY-NUM


      *****************************************************************
      *    SCREEN CUSTOMER NUMBER EDIT -- VALUE MUST BE NUMERIC       *
      *****************************************************************

           MOVE +10                    TO WMF-NUM-LTH
           MOVE ENP3CNOO               TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP3CNOL
              MOVE DFHBMFSE           TO ENP3CNOA
              GO TO ER-CUSTOMER.


           MOVE WMF-NUM-OUTPUT        TO WMF-NUM-OUTPUT-910
           MOVE WMF-NUM-OUTPUT-910    TO ENP3CNOO
                                         CA-CUSTOMER-NUM


      *****************************************************************
      *    SCREEN OPTION 3 (DELETE) REQUIRES AUTHORIZATION            *
      *****************************************************************

           IF (ENP3OPTO = '3')
               IF (EIBAID = DFHPF24)
                   NEXT SENTENCE
               ELSE
                   Move 'DELETE REQUIRES AUTHORIZATION'
                           To  ERP3FLDO
                   Move -1 To  ENP3OPTL

                   EXEC CICS SEND MAP ('SSMAPP3')
                             FROM(SSMAPP3O)
                             MAPSET ('SSMAP')
                             CURSOR
                   END-EXEC
                   GO TO ENDIT-STARTIT
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    CHECK FOR SCROLLING PF7-BACKWARD, PF8-FORWARD              *
      *****************************************************************

      *****PWB****************************
           IF EIBAID  = DFHPF7  OR  DFHPF8
               Perform SCROLL-PROCESS
               Move '1'  To ENP3OPTO
               Move ENP3PNOO   To CA-POLICY-NUM
               Move ENP3CNOO   To CA-CUSTOMER-NUM
           END-IF
      *****PWB****************************


      *****************************************************************
      *    PROCESS USER OPTION SELECTION                              *
      *****************************************************************

           EVALUATE ENP3OPTO

             WHEN '1'
                 Move '01IHOU'   To CA-REQUEST-ID
                 Move ENP3CNOO   To CA-CUSTOMER-NUM
                 Move ENP3PNOO   To CA-POLICY-NUM

                 EXEC CICS LINK PROGRAM('LGIPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC


                 IF CA-POLICY-NUM  = 0
                    MOVE 0       To CA-CUSTOMER-NUM
                    GO TO NO-DATA
                 END-IF


                 IF CA-POLICY-NUM  =  CA-POLICY-BEGIN
                    MOVE 'Start of data'     To  ENP3DATO
                 END-IF


                 IF CA-POLICY-NUM  =  CA-POLICY-END
                    MOVE 'End of data'       To  ENP3DATO
                 END-IF


                 IF CA-RETURN-CODE > 0
                   GO TO NO-DATA
                 END-IF


                 Move CA-CUSTOMER-NUM            To  ENP3CNOO
                 Move CA-POLICY-NUM              To  ENP3PNOO

                 Move CA-ISSUE-DATE              To  ENP3IDAI
                 Move CA-EXPIRY-DATE             To  ENP3EDAI
                 Move CA-H-PROPERTY-TYPE         To  ENP3TYPI
                 Move CA-H-BEDROOMS              To  ENP3BEDI
                 Move CA-H-VALUE                 To  ENP3VALI
      *****      Move CA-H-HOUSE-NAME            To  ENP3HNMI
      *****      Move CA-H-HOUSE-NUMBER          To  ENP3HNOI
                 Move CA-H-STREET-ADDRESS        To  ENP3STRI
                 Move CA-H-CITY                  To  ENP3CITI
                 Move CA-H-STATE                 To  ENP3STAI
                 Move CA-H-COUNTRY-CODE          To  ENP3COUI
                 Move CA-H-POSTCODE              To  ENP3HPCI
                 Move CA-H-AUTO-PAY              To  ENP3PAYI
                 Move CA-H-CHECK-ACCT-NBR        To  ENP3ACTI
                 Move CA-H-BANK-ROUTE-CODE       To  ENP3ROUI
                 Move CA-H-CREDIT-CARD-TYP       To  ENP3CCTI
                 Move CA-H-CREDIT-CARD-NBR       To  ENP3CCNI
                 Move CA-H-CREDIT-CARD-PIN       To  ENP3CCPI
                 Move CA-H-CREDIT-CARD-VAL       To  ENP3CCVI

                 EXEC CICS SEND MAP ('SSMAPP3')
                           FROM(SSMAPP3O)
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



                 Move '01AHOU'          To CA-REQUEST-ID
                 Move ENP3CNOI          To CA-CUSTOMER-NUM
                 Move 0                 To CA-PAYMENT
                 Move 0                 To CA-BROKERID
                 Move '        '        To CA-BROKERSREF
                 Move ENP3IDAI          To CA-ISSUE-DATE
                 Move ENP3EDAI          To CA-EXPIRY-DATE
                 Move ENP3TYPI          To CA-H-PROPERTY-TYPE
                 Move ENP3BEDI          To CA-H-BEDROOMS
                 Move ENP3VALI          To CA-H-VALUE
      *****      Move ENP3HNMI          To CA-H-HOUSE-NAME
      *****      Move ENP3HNOI          To CA-H-HOUSE-NUMBER
                 Move ENP3STRI          To CA-H-STREET-ADDRESS
                 Move ENP3CITI          To CA-H-CITY
                 Move ENP3STAI          To CA-H-STATE
                 Move ENP3COUI          To CA-H-COUNTRY-CODE
                 Move ENP3HPCI          To CA-H-POSTCODE
                 Move ENP3PAYI          To CA-H-AUTO-PAY
                 Move ENP3ACTI          To CA-H-CHECK-ACCT-NBR
                 Move ENP3ROUI          To CA-H-BANK-ROUTE-CODE
                 Move ENP3CCTI          To CA-H-CREDIT-CARD-TYP
                 Move ENP3CCNI          To CA-H-CREDIT-CARD-NBR
                 Move ENP3CCPI          To CA-H-CREDIT-CARD-PIN
                 Move ENP3CCVI          To CA-H-CREDIT-CARD-VAL

                 EXEC CICS LINK PROGRAM('LGAPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC


                 IF CA-RETURN-CODE > 0
                   Exec CICS Syncpoint Rollback End-Exec
                   GO TO NO-ADD
                 END-IF


                 Move CA-CUSTOMER-NUM To ENP3CNOI
                 Move CA-POLICY-NUM   To ENP3PNOI
                 Move ' '             To ENP3OPTI
                 Move 'New House Policy Inserted'
                   To  ERP3FLDO
                 EXEC CICS SEND MAP ('SSMAPP3')
                           FROM(SSMAPP3O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT



             WHEN '3'
                 Move '01DHOU'   To CA-REQUEST-ID
                 Move ENP3CNOO   To CA-CUSTOMER-NUM
                 Move ENP3PNOO   To CA-POLICY-NUM

                 EXEC CICS LINK PROGRAM('LGDPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC

                 IF CA-RETURN-CODE > 0
                   Exec CICS Syncpoint Rollback End-Exec
                   GO TO NO-DELETE
                 END-IF


                 Move Spaces             To  ENP3IDAI
                 Move Spaces             To  ENP3EDAI
                 Move Spaces             To  ENP3TYPI
                 Move Spaces             To  ENP3BEDI
                 Move Spaces             To  ENP3VALI
                 Move Spaces             To  ENP3STRI
                 Move Spaces             To  ENP3CITI
                 Move Spaces             To  ENP3STAI
                 Move Spaces             To  ENP3COUI
                 Move Spaces             To  ENP3HPCI
                 Move Spaces             To  ENP3PAYI
                 Move Spaces             To  ENP3ACTI
                 Move Spaces             To  ENP3ROUI
                 Move Spaces             To  ENP3CCTI
                 Move Spaces             To  ENP3CCNI
                 Move Spaces             To  ENP3CCPI
                 Move Spaces             To  ENP3CCVI


                 Move ' '             To ENP3OPTI
                 Move 'House Policy Deleted'
                   To  ERP3FLDO

                 EXEC CICS SEND MAP ('SSMAPP3')
                           FROM(SSMAPP3O)
                           MAPSET ('SSMAP')
                 END-EXEC

                 GO TO ENDIT-STARTIT


             WHEN '4'
                 Move '01IHOU'   To CA-REQUEST-ID
                 Move ENP3CNOO   To CA-CUSTOMER-NUM
                 Move ENP3PNOO   To CA-POLICY-NUM
                 EXEC CICS LINK PROGRAM('LGIPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC
                 IF CA-RETURN-CODE > 0
                   GO TO NO-DATA
                 END-IF


                 Move CA-ISSUE-DATE              To  ENP3IDAI
                 Move CA-EXPIRY-DATE             To  ENP3EDAI
                 Move CA-H-PROPERTY-TYPE         To  ENP3TYPI
                 Move CA-H-BEDROOMS              To  ENP3BEDI
                 Move CA-H-VALUE                 To  ENP3VALI
      *****      Move CA-H-HOUSE-NAME            To  ENP3HNMI
      *****      Move CA-H-HOUSE-NUMBER          To  ENP3HNOI
                 Move CA-H-STREET-ADDRESS        To  ENP3STRI
                 Move CA-H-CITY                  To  ENP3CITI
                 Move CA-H-STATE                 To  ENP3STAI
                 Move CA-H-COUNTRY-CODE          To  ENP3COUI
                 Move CA-H-POSTCODE              To  ENP3HPCI
                 Move CA-H-AUTO-PAY              To  ENP3PAYI
                 Move CA-H-CHECK-ACCT-NBR        To  ENP3ACTI
                 Move CA-H-BANK-ROUTE-CODE       To  ENP3ROUI
                 Move CA-H-CREDIT-CARD-TYP       To  ENP3CCTI
                 Move CA-H-CREDIT-CARD-NBR       To  ENP3CCNI
                 Move CA-H-CREDIT-CARD-PIN       To  ENP3CCPI
                 Move CA-H-CREDIT-CARD-VAL       To  ENP3CCVI

                 EXEC CICS SEND MAP ('SSMAPP3')
                           FROM(SSMAPP3O)
                           MAPSET ('SSMAP')
                 END-EXEC

                 EXEC CICS RECEIVE MAP('SSMAPP3')
                           INTO(SSMAPP3I)
                           MAPSET('SSMAP') END-EXEC


                 Move '01UHOU'          To CA-REQUEST-ID
                 Move ENP3CNOI          To CA-CUSTOMER-NUM
                 Move 0                 To CA-PAYMENT
                 Move 0                 To CA-BROKERID
                 Move '        '        To CA-BROKERSREF
                 Move ENP3IDAI          To CA-ISSUE-DATE
                 Move ENP3EDAI          To CA-EXPIRY-DATE
                 Move ENP3TYPI          To CA-H-PROPERTY-TYPE
                 Move ENP3BEDI          To CA-H-BEDROOMS
                 Move ENP3VALI          To CA-H-VALUE
      *****      Move ENP3HNMI          To CA-H-HOUSE-NAME
      *****      Move ENP3HNOI          To CA-H-HOUSE-NUMBER
                 Move ENP3STRI          To CA-H-STREET-ADDRESS
                 Move ENP3CITI          To CA-H-CITY
                 Move ENP3STAI          To CA-H-STATE
                 Move ENP3COUI          To CA-H-COUNTRY-CODE
                 Move ENP3HPCI          To CA-H-POSTCODE
                 Move ENP3PAYI          To CA-H-AUTO-PAY
                 Move ENP3ACTI          To CA-H-CHECK-ACCT-NBR
                 Move ENP3ROUI          To CA-H-BANK-ROUTE-CODE
                 Move ENP3CCTI          To CA-H-CREDIT-CARD-TYP
                 Move ENP3CCNI          To CA-H-CREDIT-CARD-NBR
                 Move ENP3CCPI          To CA-H-CREDIT-CARD-PIN
                 Move ENP3CCVI          To CA-H-CREDIT-CARD-VAL


                 EXEC CICS LINK PROGRAM('LGUPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC

                 IF CA-RETURN-CODE > 0
                   GO TO NO-UPD
                 END-IF

                 Move CA-CUSTOMER-NUM To ENP3CNOI
                 Move CA-POLICY-NUM   To ENP3PNOI
                 Move ' '             To ENP3OPTI
                 Move 'House Policy Updated'
                   To  ERP3FLDO

                 EXEC CICS SEND MAP ('SSMAPP3')
                           FROM(SSMAPP3O)
                           MAPSET ('SSMAP')
                 END-EXEC

                 GO TO ENDIT-STARTIT


             WHEN OTHER

                 Move 'Please enter a valid option'
                   To  ERP3FLDO
                 Move -1 To ENP3OPTL

                 EXEC CICS SEND MAP ('SSMAPP3')
                           FROM(SSMAPP3O)
                           MAPSET ('SSMAP')
                           CURSOR
                 END-EXEC
                 GO TO ENDIT-STARTIT

           END-EVALUATE.


      *    Send message to terminal and return

           EXEC CICS RETURN
           END-EXEC.

       ENDIT-STARTIT.
           MOVE  DFHBMASK      To ENP3DATA.
           EXEC CICS RETURN
                TRANSID('SSP3')
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

           Initialize SSMAPP3I.
           EXEC CICS SEND MAP ('SSMAPP3')
                     MAPSET ('SSMAP')
                     MAPONLY
           END-EXEC

           EXEC CICS RETURN
                TRANSID('SSP3')
                COMMAREA(COMM-AREA)
                END-EXEC.

      *****PWB ****************************

       SCROLL-PROCESS.

           IF EIBAID  = DFHPF7
               Move '07'  To CA-POLICY-PFKEY
           END-IF

           IF EIBAID  = DFHPF8
               Move '08'  To CA-POLICY-PFKEY
           END-IF.

      *****PWB ****************************


       ER-INVALID-PFKEY.
           Move 'Invalid PFKEY Selection'          To  ERP3FLDO.
           MOVE  -1                                To  ENP3OPTL.
           Go To ERROR-OUT.


       ER-POLICY.
           Move 'Policy Number must be numeric'    To  ERP3FLDO.
           Go To ERROR-OUT.

       ER-CUSTOMER.
           Move 'Cust Number must be numeric'      To  ERP3FLDO.
           Go To ERROR-OUT.


       ER-DATE-FORMAT.
           Move 'DATE format is YYYY-MM-DD'        To  ERP3FLDO.
           Go To ERROR-OUT.

       ER-DATE-NUMERIC.
           Move 'DATE YYYY,MM,DD must be numeric'  To  ERP3FLDO.
           Go To ERROR-OUT.

       ER-DATE-MONTH-RANGE.
           Move 'DATE MM (month) range 01-12'      To  ERP3FLDO.
           Go To ERROR-OUT.

       ER-DATE-DAY-RANGE.
           Move 'DATE DD (day) range 01-31'        To  ERP3FLDO.
           Go To ERROR-OUT.

       ER-DATE-31-DAYS.
           Move '31 DAYS not valid for month'      To  ERP3FLDO.
           Go To ERROR-OUT.


       ER-EXPIRATION-VS-ISSUE.
           Move 'Expiration Date must be > Issue Date'
                                                   To  ERP3FLDO.
           MOVE -1                                 To  ENP3EDAL.
           Go To ERROR-OUT.


       ER-BEDROOMS-NUMERIC.
           Move 'Bedrooms must be numeric'         To  ERP3FLDO.
           MOVE -1                                 To  ENP3BEDL.
           Go To ERROR-OUT.


       ER-HOUSE-VALUE.
           Move 'House Value must be numeric'      To  ERP3FLDO.
           MOVE -1                                 To  ENP3VALL.
           Go To ERROR-OUT.


       ER-AUTO-PAY.
           Move 'Auto Pay must be Y or N'          To  ERP3FLDO.
           MOVE -1                                 To  ENP3PAYL.
           Go To ERROR-OUT.


       ER-AUTO-PAY1.
           Move 'No checking or credit fields are allowed'
                                                   To  ERP3FLDO.
           MOVE -1                                 To  ENP3PAYL.
           Go To ERROR-OUT.


       ER-AUTO-PAY2.
           Move 'Checking OR Credit allowed but not both'
                                                   To  ERP3FLDO.
           MOVE -1                                 To  ENP3PAYL.
           Go To ERROR-OUT.

       ER-AUTO-PAY3.
           Move 'Either checking or credit is required'
                                                   To  ERP3FLDO.
           MOVE -1                                 To  ENP3PAYL.
           Go To ERROR-OUT.


       ER-FROM-CHECKING.
           Move 'Checking must be numeric'         To  ERP3FLDO.
           MOVE -1                                 To  ENP3ACTL.
           Go To ERROR-OUT.


       ER-BANK-ROUTING.
           Move 'Bank Routing must be numeric'     To  ERP3FLDO.
           MOVE -1                                 To  ENP3ROUL.
           Go To ERROR-OUT.


       ER-CREDIT-CARD-TYPE.
           Move 'Credit card type is invalid'      To  ERP3FLDO.
           MOVE -1                                 To  ENP3CCTL.
           Go To ERROR-OUT.


       ER-CREDIT-CARD-NUMBER.
           Move 'Credit card number must be numeric'
                                                   To  ERP3FLDO.
           MOVE -1                                 To  ENP3CCNL.
           Go To ERROR-OUT.


       ER-CREDIT-CARD-PIN-NUMBER.
           Move 'Credit card PIN Number must be numeric'
                                                   To  ERP3FLDO.
           MOVE -1                                 To  ENP3CCPL.
           Go To ERROR-OUT.


       ER-VALID-THRU-MMYY-FORMAT.
           Move 'Valid Thru format is MM/YY'       To  ERP3FLDO.
           MOVE -1                                 To  ENP3CCVL.
           Go To ERROR-OUT.


       ER-VALID-THRU-NUMERIC.
           Move 'Valid Thru MM, YY must be numeric'
                                                   To  ERP3FLDO.
           MOVE -1                                 To  ENP3CCVL.
           Go To ERROR-OUT.


       ER-VALID-THRU-MM-MONTH-RANGE.
           Move 'DATE MM (month) range 01-12'      To  ERP3FLDO.
           MOVE -1                                 To  ENP3CCVL.
           Go To ERROR-OUT.


       NO-ADD.
           Evaluate CA-RETURN-CODE
             When 70
               Move 'Customer does not exist'      To  ERP3FLDO
               Go To ERROR-OUT
             When Other
               Move 'Error Adding House Policy'    To  ERP3FLDO
               Go To ERROR-OUT
           End-Evaluate.


       NO-UPD.
           Move 'Error Updating House Policy'      To  ERP3FLDO
           Go To ERROR-OUT.


       NO-DELETE.
           Move 'Error Deleting House Policy'      To  ERP3FLDO
           Move -1 To  ENP3OPTL
           Go To ERROR-OUT.


       NO-DATA.
           Move 'No data was returned.'            To  ERP3FLDO
           MOVE -1                                 To  ENP3PNOL.


           IF EIBAID NOT = DFHENTER
              MOVE '0000000000'   To ENP3CNOO
              MOVE '0000000000'   To ENP3PNOO.


           Initialize                ENP3IDAI
           Initialize                ENP3EDAI
           Initialize                ENP3TYPI
           Initialize                ENP3BEDI
           Initialize                ENP3VALI
           Initialize                ENP3STRI
           Initialize                ENP3CITI
           Initialize                ENP3STAI
           Initialize                ENP3COUI
           Initialize                ENP3HPCI
           Initialize                ENP3PAYI
           Initialize                ENP3ACTI
           Initialize                ENP3ROUI
           Initialize                ENP3CCTI
           Initialize                ENP3CCNI
           Initialize                ENP3CCPI
           Initialize                ENP3CCVI

           Go To ERROR-OUT.


       ERROR-OUT.
           EXEC CICS SEND MAP ('SSMAPP3')
                     FROM(SSMAPP3O)
                     MAPSET ('SSMAP')
                     CURSOR
           END-EXEC.

           Initialize SSMAPP3I.
           Initialize SSMAPP3O.
           Initialize COMM-AREA.

           MOVE DFHBMFSE       To ENP3PNOA.

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

           IF ENP3ACTI = LOW-VALUES  MOVE SPACES TO ENP3ACTI.
           IF ENP3ROUI = LOW-VALUES  MOVE SPACES TO ENP3ROUI.
           IF ENP3CCTI = LOW-VALUES  MOVE SPACES TO ENP3CCTI.
           IF ENP3CCNI = LOW-VALUES  MOVE SPACES TO ENP3CCNI.
           IF ENP3CCPI = LOW-VALUES  MOVE SPACES TO ENP3CCPI.
           IF ENP3CCVI = LOW-VALUES  MOVE SPACES TO ENP3CCVI.


      ********************************************
      *    ISSUE DATE                            *
      ********************************************

           MOVE ENP3IDAI               TO WMF-DATE.


           IF WMF-DASH1 = '-'  AND
              WMF-DASH2 = '-'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP3IDAL
              GO TO ER-DATE-FORMAT.


           IF WMF-YEAR  NUMERIC    AND
              WMF-MONTH NUMERIC    AND
              WMF-DAY   NUMERIC
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP3IDAL
              GO TO ER-DATE-NUMERIC.


           IF (WMF-MONTH-R > 0) AND (WMF-MONTH-R < 13)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP3IDAL
              GO TO ER-VALID-THRU-MM-MONTH-RANGE.


           IF (WMF-DAY-R > 0) AND (WMF-DAY-R < 32)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP3IDAL
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
                  MOVE -1             TO ENP3IDAL
                  GO TO ER-DATE-31-DAYS
           ELSE
                  NEXT SENTENCE.


      ********************************************
      *    EXPIRATION DATE                       *
      ********************************************

           MOVE ENP3EDAI               TO WMF-DATE.


           IF WMF-DASH1 = '-'  AND
              WMF-DASH2 = '-'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP3EDAL
              GO TO ER-DATE-FORMAT.


           IF WMF-YEAR  NUMERIC    AND
              WMF-MONTH NUMERIC    AND
              WMF-DAY   NUMERIC
                   NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP3EDAL
              GO TO ER-DATE-NUMERIC.


           IF (WMF-MONTH-R > 0) AND (WMF-MONTH-R < 13)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP3EDAL
              GO TO ER-DATE-MONTH-RANGE.


           IF (WMF-DAY-R > 0) AND (WMF-DAY-R < 32)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP3EDAL
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
                  MOVE -1             TO ENP3EDAL
                  GO TO ER-DATE-31-DAYS
           ELSE
                  NEXT SENTENCE.



      ********************************************
      * EXPIRATION DATE MUST BE > ISSUE DATE     *
      ********************************************

           IF (WS-EDIT-ERRORS = 'N')
               IF (ENP3EDAI > ENP3IDAI)
                   NEXT SENTENCE
               ELSE
                   GO TO ER-EXPIRATION-VS-ISSUE
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    BEDROOMS    -- VALUE MUST BE NUMERIC                       *
      *****************************************************************

           MOVE +3                     TO WMF-NUM-LTH
           MOVE ENP3BEDI               TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP3BEDL
              MOVE DFHBMFSE           TO ENP3BEDA
              GO TO ER-BEDROOMS-NUMERIC.


           MOVE WMF-NUM-OUTPUT        TO WMF-BEDROOMS
           MOVE WMF-BEDROOMS          TO ENP3BEDO
                                         CA-CUSTOMER-NUM.


      *****************************************************************
      *    HOUSE VALUE -- MUST BE NUMERIC                             *
      *****************************************************************

           MOVE +8                     TO WMF-NUM-LTH
           MOVE ENP3VALI               TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP3VALL
              MOVE DFHBMFSE           TO ENP3VALA
              GO TO ER-HOUSE-VALUE.


           MOVE WMF-NUM-OUTPUT        TO WMF-HOUSE-VALUE
           MOVE WMF-HOUSE-VALUE       TO ENP3VALO
                                         CA-H-VALUE.


      *****************************************************************
      *    AUTO PAY       -- VALUE MUST BE   Y  OR  N                 *
      *****************************************************************

           IF ENP3PAYI =     'Y' OR
              ENP3PAYI =     'N'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              GO TO ER-AUTO-PAY.


      *****************************************************************
      *    AUTO PAY = N   -- checking, routing, card type, number,    *
      *                   -- pin, valid thru, NOT ALLOWED             *
      *****************************************************************

            IF ENP3PAYI = 'N'
                IF (ENP3ACTI > SPACES)        OR
                   (ENP3ROUI > SPACES)        OR
                   (ENP3CCTI > SPACES)        OR
                   (ENP3CCNI > SPACES)        OR
                   (ENP3CCPI > SPACES)        OR
                   (ENP3CCVI > SPACES)
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

            IF ENP3PAYI = 'Y'
                IF ((ENP3ACTI > SPACES)       OR
                    (ENP3ROUI > SPACES))      AND
                   ((ENP3CCTI > SPACES)       OR
                    (ENP3CCNI > SPACES)       OR
                    (ENP3CCPI > SPACES)       OR
                    (ENP3CCVI > SPACES))
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

            IF ENP3PAYI = 'Y'
                IF ((ENP3ACTI = SPACES)       AND
                    (ENP3ROUI = SPACES))      AND
                   ((ENP3CCTI = SPACES)       AND
                    (ENP3CCNI = SPACES)       AND
                    (ENP3CCPI = SPACES)       AND
                    (ENP3CCVI = SPACES))
                    MOVE 'Y'                  TO WS-EDIT-ERRORS
                    GO TO ER-AUTO-PAY3
                ELSE
                    NEXT SENTENCE
            ELSE
                    NEXT SENTENCE.


      *****************************************************************
      *    FROM CHECKING   -- VALUE MUST BE NUMERIC                   *
      *****************************************************************

            IF (ENP3ACTI > SPACES)  OR
               (ENP3ROUI > SPACES)

                IF ENP3ACTI  NUMERIC
                    NEXT SENTENCE
                ELSE
                    MOVE 'Y'             TO WS-EDIT-ERRORS
                    GO TO ER-FROM-CHECKING
            ELSE
                    NEXT SENTENCE.



      *****************************************************************
      *    BANK ROUTING    -- VALUE MUST BE NUMERIC *
      *****************************************************************

            IF (ENP3ACTI > SPACES)  OR
               (ENP3ROUI > SPACES)

                IF ENP3ROUI  NUMERIC
                    NEXT SENTENCE
                ELSE
                    MOVE 'Y'             TO WS-EDIT-ERRORS
                    GO TO ER-BANK-ROUTING
            ELSE
                    NEXT SENTENCE.



      *****************************************************************
      *    CREDIT CARD TYPE                                           *
      *****************************************************************

            IF (ENP3ACTI = SPACES) AND
               (ENP3ROUI = SPACES) AND
               (ENP3PAYI = 'Y')
                IF ENP3CCTI = 'AMEX'           OR
                   ENP3CCTI = 'VISA'           OR
                   ENP3CCTI = 'MC'             OR
                   ENP3CCTI = 'DISC'
                       NEXT SENTENCE
                ELSE
                       MOVE 'Y'            TO WS-EDIT-ERRORS
                       GO TO ER-CREDIT-CARD-TYPE
            ELSE
                 NEXT SENTENCE.


      *****************************************************************
      *    CREDIT CARD NUMBER                                         *
      *****************************************************************

            IF (ENP3ACTI = SPACES)  AND
               (ENP3ROUI = SPACES)  AND
               (ENP3PAYI = 'Y')
                IF ENP3CCNI  NUMERIC
                    NEXT SENTENCE
                ELSE
                    MOVE 'Y'             TO WS-EDIT-ERRORS
                    GO TO ER-CREDIT-CARD-NUMBER
            ELSE
                    NEXT SENTENCE.



      *****************************************************************
      *    PIN NUMBER                                                 *
      *****************************************************************

            IF (ENP3ACTI = SPACES)  AND
               (ENP3ROUI = SPACES)  AND
               (ENP3PAYI = 'Y')
                IF ENP3CCPI  NUMERIC
                    NEXT SENTENCE
                ELSE
                    MOVE 'Y'             TO WS-EDIT-ERRORS
                    GO TO ER-CREDIT-CARD-PIN-NUMBER
            ELSE
                    NEXT SENTENCE.


      *****************************************************************
      *    CREDIT CARD VALID THRU                                     *
      *****************************************************************

            IF (ENP3ACTI = SPACES)  AND
               (ENP3ROUI = SPACES)  AND
               (ENP3PAYI = 'Y')
                MOVE ENP3CCVI               TO WMF-DATE-MMYY
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
