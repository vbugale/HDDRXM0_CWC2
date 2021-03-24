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
      *               Commercial Policy Menu                           *
      *                                                                *
      * Menu for Commercial Policy Transactions                        *
      *                                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGTESTP4.
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


           05  WMF-FIRE-PERIL          PIC 9(4).
           05  WMF-FIRE-PREMIUM        PIC 9(8).
           05  WMF-CRIME-PERIL         PIC 9(4).
           05  WMF-CRIME-PREMIUM       PIC 9(8).
           05  WMF-FLOOD-PERIL         PIC 9(4).
           05  WMF-FLOOD-PREMIUM       PIC 9(8).
           05  WMF-WEATHER-PERIL       PIC 9(4).
           05  WMF-WEATHER-PREMIUM     PIC 9(8).
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

           Initialize SSMAPP4I.
           Initialize SSMAPP4O.
           Initialize COMM-AREA.

           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE.

           MOVE '0000000000'   To ENP4PNOO.
           MOVE  DFHBMFSE      To ENP4PNOA.
           MOVE '0000000000'   To ENP4CNOO.
           MOVE  DFHBMASK      To ENP4DATA.
           MOVE  DFHBMASB      To ERP4FLDA.
           MOVE  DFHBMASB      To ENP4CDTA.
           MOVE  '_'           To ENP4OPTO.

           MOVE SPACES         To ENP4FPEO.
           MOVE SPACES         To ENP4FPRO.
           MOVE SPACES         To ENP4CPEO.
           MOVE SPACES         To ENP4CPRO.
           MOVE SPACES         To ENP4XPEO.
           MOVE SPACES         To ENP4XPRO.
           MOVE SPACES         To ENP4WPEO.
           MOVE SPACES         To ENP4WPRO.
           MOVE SPACES         To ENP4STAO.


           MOVE WS-CURRENT-YEAR    To WMF-YEAR
           MOVE WS-CURRENT-MONTH   To WMF-MONTH
           MOVE WS-CURRENT-DAY     To WMF-DAY
           MOVE '-'                To WMF-DASH1
                                      WMF-DASH2
           MOVE WMF-DATE           To ENP4CDTO


      * Display Main Menu
           EXEC CICS SEND MAP ('SSMAPP4')
                     MAPSET ('SSMAP')
                     ERASE
                     END-EXEC.

       A-GAIN.

           MOVE  DFHBMASK      To ENP4DATA.

           EXEC CICS HANDLE AID
                     CLEAR(CLEARIT)
                     PF3(ENDIT) END-EXEC.
           EXEC CICS HANDLE CONDITION
                     MAPFAIL(ENDIT)
                     END-EXEC.

           EXEC CICS RECEIVE MAP('SSMAPP4')
                     INTO(SSMAPP4I)
                     MAPSET('SSMAP') END-EXEC.


           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE.

           MOVE WS-CURRENT-YEAR    To WMF-YEAR
           MOVE WS-CURRENT-MONTH   To WMF-MONTH
           MOVE WS-CURRENT-DAY     To WMF-DAY
           MOVE '-'                To WMF-DASH1
                                      WMF-DASH2
           MOVE WMF-DATE           To ENP4CDTO


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

           IF ENP4OPTO = '1' OR '2' OR '3' OR '4'
                 NEXT SENTENCE
           ELSE
                 Move 'Please enter a valid option'
                             To  ERP4FLDO
                 Move -1 To  ENP4OPTL
                 EXEC CICS SEND MAP ('SSMAPP4')
                           FROM(SSMAPP4O)
                               MAPSET ('SSMAP')
                               CURSOR
                 END-EXEC
                 GO TO ENDIT-STARTIT.


      *****************************************************************
      *    SCREEN OPTION VS PFKEY EDIT, PF24 ONLY USED FOR OPTION 3   *
      *****************************************************************

           IF ENP4OPTO NOT = '3'
               IF EIBAID = DFHPF24
                    Move 'Invalid PFKEY Selection' To  ERP4FLDO
                    Move -1 To  ENP4OPTL

                    EXEC CICS SEND MAP ('SSMAPP4')
                         FROM(SSMAPP4O)
                         MAPSET ('SSMAP')
                         CURSOR
                    END-EXEC
                    GO TO ENDIT-STARTIT
               ELSE
                    NEXT SENTENCE
           ELSE
                    NEXT SENTENCE.


      ***************************************************************
      *    SCREEN POLICY   NUMBER EDIT -- VALUE MUST BE NUMERIC     *
      ***************************************************************

           MOVE +10                    TO WMF-NUM-LTH
           MOVE ENP4PNOO               TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP4PNOL
              MOVE DFHBMFSE           TO ENP4PNOA
              GO TO ER-POLICY.


           MOVE WMF-NUM-OUTPUT        TO WMF-NUM-OUTPUT-910
           MOVE WMF-NUM-OUTPUT-910    TO ENP4PNOO
                                         CA-POLICY-NUM


      ****************************************************************
      *    SCREEN CUSTOMER NUMBER EDIT -- VALUE MUST BE NUMERIC      *
      ****************************************************************

           MOVE +10                    TO WMF-NUM-LTH
           MOVE ENP4CNOO               TO WMF-NUM-INPUT

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP4CNOL
              MOVE DFHBMFSE           TO ENP4CNOA
              GO TO ER-CUSTOMER.


           MOVE WMF-NUM-OUTPUT        TO WMF-NUM-OUTPUT-910
           MOVE WMF-NUM-OUTPUT-910    TO ENP4CNOO
                                         CA-CUSTOMER-NUM


      *****************************************************************
      *    SCREEN OPTION 3 (DELETE) REQUIRES AUTHORIZATION            *
      *****************************************************************

           IF (ENP4OPTO = '3')
               IF (EIBAID = DFHPF24)
                   NEXT SENTENCE
               ELSE
                   Move 'DELETE REQUIRES AUTHORIZATION'
                           To  ERP4FLDO
                   Move -1 To  ENP4OPTL

                   EXEC CICS SEND MAP ('SSMAPP4')
                             FROM(SSMAPP4O)
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
               Move '1'  To ENP4OPTO
               Move ENP4PNOO   To CA-POLICY-NUM
               Move ENP4CNOO   To CA-CUSTOMER-NUM
           END-IF
      *****PWB****************************


      *****************************************************************
      *    PROCESS USER OPTION SELECTION                              *
      *****************************************************************

           EVALUATE ENP4OPTO

             WHEN '1'
                 If (
                     ENP4CNOO Not = Spaces      AND
                     ENP4CNOO Not = Low-Values  AND
                     ENP4CNOO Not = 0
                                                   )
                                                    AND
                    (
                     ENP4PNOO Not = Spaces      AND
                     ENP4PNOO Not = Low-Values  AND
                     ENP4PNOO Not = 0
                                                   )
                        Move '01ICOM'   To CA-REQUEST-ID
                        Move ENP4CNOO   To CA-CUSTOMER-NUM
                        Move ENP4PNOO   To CA-POLICY-NUM
                 End-If

                 If (
                     ENP4CNOO     = Spaces      OR
                     ENP4CNOO     = Low-Values  OR
                     ENP4CNOO     = 0
                                                   )
                                                    AND
                    (
                     ENP4PNOO Not = Spaces      AND
                     ENP4PNOO Not = Low-Values  AND
                     ENP4PNOO Not = 0
                                                   )
                        Move '02ICOM'   To CA-REQUEST-ID
                        Move ENP4PNOO   To CA-POLICY-NUM
                 End-If

                 If (
                     ENP4CNOO Not = Spaces      AND
                     ENP4CNOO Not = Low-Values  AND
                     ENP4CNOO Not = 0
                                                   )
                                                    AND
                    (
                     ENP4PNOO     = Spaces      Or
                     ENP4PNOO     = Low-Values  Or
                     ENP4PNOO     = 0
                                                   )
                        Move '03ICOM'   To CA-REQUEST-ID
                        Move ENP4CNOO   To CA-CUSTOMER-NUM
                 End-If

      *******    If (
      *******        ENP4HPCO NOT = Spaces      AND
      *******        ENP4HPCO NOT = Low-Values  AND
      *******        ENP4HPCO NOT = 0
      *******                                      )
      *******           Move '05ICOM'   To CA-REQUEST-ID
      *******           Move ENP4HPCO   To CA-B-PostCode
      *******    End-If



                 EXEC CICS LINK PROGRAM('LGIPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC


                 IF CA-POLICY-NUM  = 0
                    MOVE 0       To CA-CUSTOMER-NUM
                    GO TO NO-DATA
                 END-IF


                 IF CA-POLICY-NUM  =  CA-POLICY-BEGIN
                    MOVE 'Start of data'     To  ENP4DATO
                 END-IF


                 IF CA-POLICY-NUM  =  CA-POLICY-END
                    MOVE 'End of data'       To  ENP4DATO
                 END-IF


                 IF CA-RETURN-CODE > 0
                   GO TO NO-DATA
                 END-IF



                 Move CA-CUSTOMER-NUM      To  ENP4CNOO
                 Move CA-POLICY-NUM        To  ENP4PNOO

      ***PWB***  Move CA-CUSTOMER-NUM      To  ENP4CNOI
                 Move CA-ISSUE-DATE        To  ENP4IDAI
                 Move CA-EXPIRY-DATE       To  ENP4EDAI
                 Move CA-B-STREET-ADDRESS  To  ENP4ADDI
                 Move CA-B-CITY            To  ENP4CITI
                 Move CA-B-STATE           To  ENP4STAI
                 Move CA-B-COUNTRY-CODE    To  ENP4COUI
                 Move CA-B-Postcode        To  ENP4HPCI
      *****      Move CA-B-Latitude        To  ENP4LATI
      *****      Move CA-B-Longitude       To  ENP4LONI
      *****      Move CA-B-Customer        To  ENP4CUSI
                 Move CA-B-PropType        To  ENP4PTYI
                 Move CA-B-FirePeril       To  ENP4FPEI
                 Move CA-B-FirePremium     To  ENP4FPRI
                 Move CA-B-CrimePeril      To  ENP4CPEI
                 Move CA-B-CrimePremium    To  ENP4CPRI
                 Move CA-B-FloodPeril      To  ENP4XPEI
                 Move CA-B-FloodPremium    To  ENP4XPRI
                 Move CA-B-WeatherPeril    To  ENP4WPEI
                 Move CA-B-WeatherPremium  To  ENP4WPRI
                 Move CA-B-AUTO-PAY        To  ENP4PAYI
                 Move CA-B-CHECK-ACCT-NBR  To  ENP4ACTI
                 Move CA-B-BANK-ROUTE-CODE To  ENP4ROUI
                 Move CA-B-CREDIT-CARD-TYP To  ENP4CCTI
                 Move CA-B-CREDIT-CARD-NBR To  ENP4CCNI
                 Move CA-B-CREDIT-CARD-PIN To  ENP4CCPI
                 Move CA-B-CREDIT-CARD-VAL To  ENP4CCVI
      *****      Move CA-B-Status          To  ENP4STAI
      *****      Move CA-B-RejectReason    To  ENP4REJI


                 EXEC CICS SEND MAP ('SSMAPP4')
                           FROM(SSMAPP4O)
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


                 Move '01ACOM'             To  CA-REQUEST-ID
                 Move ENP4CNOO             To  CA-CUSTOMER-NUM
                 Move ENP4IDAO             To  CA-ISSUE-DATE
                 Move ENP4EDAO             To  CA-EXPIRY-DATE
                 Move ENP4ADDO             To  CA-B-STREET-ADDRESS
                 Move ENP4CITO             To  CA-B-CITY
                 Move ENP4STAO             To  CA-B-STATE
                 Move ENP4COUO             To  CA-B-COUNTRY-CODE
      *****      Move ENP4ADDO             To  CA-B-Address
                 Move ENP4HPCO             To  CA-B-Postcode
      *****      Move ENP4LATO             To  CA-B-Latitude
      *****      Move ENP4LONO             To  CA-B-Longitude
      *****      Move ENP4CUSO             To  CA-B-Customer
                 Move ENP4PTYO             To  CA-B-PropType
                 Move ENP4FPEO             To  CA-B-FirePeril
                 Move ENP4FPRO             To  CA-B-FirePremium
                 Move ENP4CPEO             To  CA-B-CrimePeril
                 Move ENP4CPRO             To  CA-B-CrimePremium
                 Move ENP4XPEO             To  CA-B-FloodPeril
                 Move ENP4XPRO             To  CA-B-FloodPremium
                 Move ENP4WPEO             To  CA-B-WeatherPeril
                 Move ENP4WPRO             To  CA-B-WeatherPremium
                 Move ENP4PAYO             To  CA-B-AUTO-PAY
                 Move ENP4ACTO             To  CA-B-CHECK-ACCT-NBR
                 Move ENP4ROUO             To  CA-B-BANK-ROUTE-CODE
                 Move ENP4CCTO             To  CA-B-CREDIT-CARD-TYP
                 Move ENP4CCNO             To  CA-B-CREDIT-CARD-NBR
                 Move ENP4CCPO             To  CA-B-CREDIT-CARD-PIN
                 Move ENP4CCVO             To  CA-B-CREDIT-CARD-VAL
      *****      Move ENP4STAO             To  CA-B-Status
      *****      Move ENP4REJO             To  CA-B-RejectReason

                 EXEC CICS LINK PROGRAM('LGAPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC

                 IF CA-RETURN-CODE > 0
                   Exec CICS Syncpoint Rollback End-Exec
                   GO TO NO-ADD
                 END-IF

                 Move CA-CUSTOMER-NUM To ENP4CNOI
                 Move CA-POLICY-NUM   To ENP4PNOI
                 Move ' '             To ENP4OPTI
                 Move 'New Commercial Policy Inserted'
                   To  ERP4FLDO

                 EXEC CICS SEND MAP ('SSMAPP4')
                           FROM(SSMAPP4O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT


             WHEN '3'
                 Move '01DCOM'   To CA-REQUEST-ID
                 Move ENP4CNOO   To CA-CUSTOMER-NUM
                 Move ENP4PNOO   To CA-POLICY-NUM

                 EXEC CICS LINK PROGRAM('LGDPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC

                 IF CA-RETURN-CODE > 0
                   Exec CICS Syncpoint Rollback End-Exec
                   GO TO NO-DELETE
                 END-IF

                 Move SPACES               To  ENP4EDAI
                 Move SPACES               To  ENP4ADDI
                 Move SPACES               To  ENP4CITI
                 Move SPACES               To  ENP4STAI
                 Move SPACES               To  ENP4COUI
                 Move SPACES               To  ENP4HPCI
      *****      Move CA-B-Latitude        To  ENP4LATI
      *****      Move CA-B-Longitude       To  ENP4LONI
      *****      Move SPACES               To  ENP4CUSI
                 Move SPACES               To  ENP4PTYI
                 Move SPACES               To  ENP4FPEI
                 Move SPACES               To  ENP4FPRI
                 Move SPACES               To  ENP4CPEI
                 Move SPACES               To  ENP4CPRI
                 Move SPACES               To  ENP4XPEI
                 Move SPACES               To  ENP4XPRI
                 Move SPACES               To  ENP4WPEI
                 Move SPACES               To  ENP4WPRI
                 Move SPACES               To  ENP4PAYI
                 Move SPACES               To  ENP4ACTI
                 Move SPACES               To  ENP4ROUI
                 Move SPACES               To  ENP4CCTI
                 Move SPACES               To  ENP4CCNI
                 Move SPACES               To  ENP4CCPI
                 Move SPACES               To  ENP4CCVI
      *****      Move SPACES               To  ENP4STAI
      *****      Move SPACES               To  ENP4REJI

                 Move ' '             To ENP4OPTI
                 Move 'Commercial Policy Deleted'
                   To  ERP4FLDO

                 EXEC CICS SEND MAP ('SSMAPP4')
                           FROM(SSMAPP4O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT


             WHEN OTHER

                 Move 'Please enter a valid option'
                   To  ERP4FLDO
                 Move -1 To ENP4OPTL

                 EXEC CICS SEND MAP ('SSMAPP4')
                           FROM(SSMAPP4O)
                           MAPSET ('SSMAP')
                           CURSOR
                 END-EXEC
                 GO TO ENDIT-STARTIT

           END-EVALUATE.


      *    Send message to terminal and return

           EXEC CICS RETURN
           END-EXEC.

       ENDIT-STARTIT.
           MOVE  DFHBMASK      To ENP4DATA.
           EXEC CICS RETURN
                TRANSID('SSP4')
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

           Initialize SSMAPP4I.
           EXEC CICS SEND MAP ('SSMAPP4')
                     MAPSET ('SSMAP')
                     MAPONLY
           END-EXEC

           EXEC CICS RETURN
                TRANSID('SSP4')
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
      ******PWB ****************************


       ER-INVALID-PFKEY.
           Move 'Invalid PFKEY Selection'          To  ERP4FLDO
           MOVE  -1                                To  ENP4OPTL
           Go To ERROR-OUT.


       ER-POLICY.
           Move 'Policy Number must be numeric'    To  ERP4FLDO
           Go To ERROR-OUT.

       ER-CUSTOMER.
           Move 'Cust Number must be numeric'      To  ERP4FLDO
           Go To ERROR-OUT.

       ER-DATE-FORMAT.
           Move 'DATE format is YYYY-MM-DD'        To  ERP4FLDO.
           Go To ERROR-OUT.

       ER-DATE-NUMERIC.
           Move 'DATE YYYY,MM,DD must be numeric'  To  ERP4FLDO.
           Go To ERROR-OUT.

       ER-DATE-MONTH-RANGE.
           Move 'DATE MM (month) range 01-12'      To  ERP4FLDO.
           Go To ERROR-OUT.

       ER-DATE-DAY-RANGE.
           Move 'DATE DD (day) range 01-31'        To  ERP4FLDO.
           Go To ERROR-OUT.


       ER-DATE-31-DAYS.
           Move '31 DAYS not valid for month'      To  ERP4FLDO.
           Go To ERROR-OUT.


       ER-EXPIRATION-VS-ISSUE.
           Move 'Expiration Date must be > Issue Date'
                                                   To  ERP4FLDO.
           MOVE -1                                 To  ENP4EDAL.
           Go To ERROR-OUT.


       ER-FIRE-PERIL.
           Move 'Fire Peril must be numeric'       To  ERP4FLDO.
           MOVE -1                                 To  ENP4FPEL.
           Go To ERROR-OUT.


       ER-FIRE-PREMIUM.
           Move 'Fire Prem must be numeric'        To  ERP4FLDO.
           MOVE -1                                 To  ENP4FPRL.
           Go To ERROR-OUT.


       ER-CRIME-PERIL.
           Move 'Crime Peril must be numeric'      To  ERP4FLDO.
           MOVE -1                                 To  ENP4CPEL.
           Go To ERROR-OUT.


       ER-CRIME-PREMIUM.
           Move 'Crime Prem must be numeric'       To  ERP4FLDO.
           MOVE -1                                 To  ENP4CPRL.
           Go To ERROR-OUT.


       ER-FLOOD-PERIL.
           Move 'Flood Peril must be numeric'      To  ERP4FLDO.
           MOVE -1                                 To  ENP4XPEL.
           Go To ERROR-OUT.


       ER-FLOOD-PREMIUM.
           Move 'Flood Prem must be numeric'       To  ERP4FLDO.
           MOVE -1                                 To  ENP4XPRL.
           Go To ERROR-OUT.


       ER-WEATHER-PERIL.
           Move 'Weather Peril must be numeric'    To  ERP4FLDO.
           MOVE -1                                 To  ENP4WPEL.
           Go To ERROR-OUT.


       ER-WEATHER-PREMIUM.
           Move 'Weather Prem must be numeric'     To  ERP4FLDO.
           MOVE -1                                 To  ENP4WPRL.
           Go To ERROR-OUT.


       ER-AUTO-PAY.
           Move 'Auto Pay must be Y or N'          To  ERP4FLDO.
           MOVE -1                                 To  ENP4PAYL.
           Go To ERROR-OUT.


       ER-AUTO-PAY1.
           Move 'No checking or credit fields are allowed'
                                                   To  ERP4FLDO.
           MOVE -1                                 To  ENP4PAYL.
           Go To ERROR-OUT.


       ER-AUTO-PAY2.
           Move 'Checking OR Credit allowed but not both'
                                                   To  ERP4FLDO.
           MOVE -1                                 To  ENP4PAYL.
           Go To ERROR-OUT.


       ER-AUTO-PAY3.
           Move 'Either checking or credit is required'
                                                   To  ERP4FLDO.
           MOVE -1                                 To  ENP4PAYL.
           Go To ERROR-OUT.


       ER-FROM-CHECKING.
           Move 'Checking must be numeric'         To  ERP4FLDO.
           MOVE -1                                 To  ENP4ACTL.
           Go To ERROR-OUT.


       ER-BANK-ROUTING.
           Move 'Bank Routing must be numeric'     To  ERP4FLDO.
           MOVE -1                                 To  ENP4ROUL.
           Go To ERROR-OUT.


       ER-CREDIT-CARD-TYPE.
           Move 'Credit card type is invalid'      To  ERP4FLDO.
           MOVE -1                                 To  ENP4CCTL.
           Go To ERROR-OUT.


       ER-CREDIT-CARD-NUMBER.
           Move 'Credit card number must be numeric'
                                                   To  ERP4FLDO.
           MOVE -1                                 To  ENP4CCNL.
           Go To ERROR-OUT.


       ER-CREDIT-CARD-PIN-NUMBER.
           Move 'Credit card PIN Number must be numeric'
                                                   To  ERP4FLDO.
           MOVE -1                                 To  ENP4CCPL.
           Go To ERROR-OUT.


       ER-VALID-THRU-MMYY-FORMAT.
           Move 'Valid Thru format is MM/YY'       To  ERP4FLDO.
           MOVE -1                                 To  ENP4CCVL.
           Go To ERROR-OUT.


       ER-VALID-THRU-NUMERIC.
           Move 'Valid Thru MM, YY must be numeric'
                                                   To  ERP4FLDO.
           MOVE -1                                 To  ENP4CCVL.
           Go To ERROR-OUT.


       ER-VALID-THRU-MM-MONTH-RANGE.
           Move 'DATE MM (month) range 01-12'      To  ERP4FLDO.
           MOVE -1                                 To  ENP4CCVL.
           Go To ERROR-OUT.


       NO-ADD.
           Evaluate CA-RETURN-CODE
             When 70
               Move 'Customer does not exist'        To  ERP4FLDO
               Go To ERROR-OUT
             When Other
               Move 'Error Adding Commercial Policy' To  ERP4FLDO
               Go To ERROR-OUT
           End-Evaluate.


       NO-UPD.
           Move 'Error Updating Commercial Policy'   To  ERP4FLDO
           Go To ERROR-OUT.


       NO-DELETE.
           Move 'Error Deleting Commercial Policy'   To  ERP4FLDO
           Move -1 To  ENP4OPTL
           Go To ERROR-OUT.


       NO-DATA.
           Move 'No data was returned.'              To  ERP4FLDO
           MOVE -1                                   To  ENP4PNOL.


           IF EIBAID NOT = DFHENTER
              MOVE '0000000000'   To ENP4CNOO
              MOVE '0000000000'   To ENP4PNOO.


           Initialize             ENP4IDAO
           Initialize             ENP4EDAO
           Initialize             ENP4ADDO
           Initialize             ENP4CITO
           Initialize             ENP4STAO
           Initialize             ENP4COUO
           Initialize             ENP4HPCO
           Initialize             ENP4PTYO
           Initialize             ENP4FPEO
           Initialize             ENP4FPRO
           Initialize             ENP4CPEO
           Initialize             ENP4CPRO
           Initialize             ENP4XPEO
           Initialize             ENP4XPRO
           Initialize             ENP4WPEO
           Initialize             ENP4WPRO
           Initialize             ENP4PAYO
           Initialize             ENP4ACTO
           Initialize             ENP4ROUO
           Initialize             ENP4CCTO
           Initialize             ENP4CCNO
           Initialize             ENP4CCPO
           Initialize             ENP4CCVO

           Go To ERROR-OUT.


       ERROR-OUT.
           EXEC CICS SEND MAP ('SSMAPP4')
                     FROM(SSMAPP4O)
                     MAPSET ('SSMAP')
                     CURSOR
           END-EXEC.

           Initialize SSMAPP4I.
           Initialize SSMAPP4O.
           Initialize COMM-AREA.

           MOVE DFHBMFSE       To ENP4PNOA.

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

           IF ENP4ACTI = LOW-VALUES  MOVE SPACES TO ENP4ACTI.
           IF ENP4ROUI = LOW-VALUES  MOVE SPACES TO ENP4ROUI.
           IF ENP4CCTI = LOW-VALUES  MOVE SPACES TO ENP4CCTI.
           IF ENP4CCNI = LOW-VALUES  MOVE SPACES TO ENP4CCNI.
           IF ENP4CCPI = LOW-VALUES  MOVE SPACES TO ENP4CCPI.
           IF ENP4CCVI = LOW-VALUES  MOVE SPACES TO ENP4CCVI.



      ********************************************
      *    ISSUE DATE                            *
      ********************************************

                MOVE ENP4IDAI               TO WMF-DATE.


                IF WMF-DASH1 = '-'  AND
                   WMF-DASH2 = '-'
                   NEXT SENTENCE
                ELSE
                   MOVE 'Y'                 TO WS-EDIT-ERRORS
                   MOVE -1                  TO ENP4IDAL
                   GO TO ER-DATE-FORMAT.


                IF WMF-YEAR  NUMERIC    AND
                   WMF-MONTH NUMERIC    AND
                   WMF-DAY   NUMERIC
                   NEXT SENTENCE
                ELSE
                   MOVE 'Y'                 TO WS-EDIT-ERRORS
                   MOVE -1                  TO ENP4IDAL
                   GO TO ER-DATE-NUMERIC.


                IF (WMF-MONTH-R > 0) AND (WMF-MONTH-R < 13)
                    NEXT SENTENCE
                ELSE
                   MOVE 'Y'                 TO WS-EDIT-ERRORS
                   MOVE -1                  TO ENP4IDAL
                   GO TO ER-VALID-THRU-MM-MONTH-RANGE.


                IF (WMF-DAY-R > 0) AND (WMF-DAY-R < 32)
                      NEXT SENTENCE
                ELSE
                   MOVE 'Y'                 TO WS-EDIT-ERRORS
                   MOVE -1                  TO ENP4IDAL
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
                       MOVE -1             TO ENP4IDAL
                       GO TO ER-DATE-31-DAYS
                ELSE
                       NEXT SENTENCE.


      ********************************************
      *    EXPIRATION DATE                       *
      ********************************************

           MOVE ENP4EDAI               TO WMF-DATE.


           IF WMF-DASH1 = '-'  AND
              WMF-DASH2 = '-'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP4EDAL
              GO TO ER-DATE-FORMAT.


           IF WMF-YEAR  NUMERIC    AND
              WMF-MONTH NUMERIC    AND
              WMF-DAY   NUMERIC
                   NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP4EDAL
              GO TO ER-DATE-NUMERIC.


           IF (WMF-MONTH-R > 0) AND (WMF-MONTH-R < 13)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP4EDAL
              GO TO ER-DATE-MONTH-RANGE.


           IF (WMF-DAY-R > 0) AND (WMF-DAY-R < 32)
               NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              MOVE -1                  TO ENP4EDAL
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
                  MOVE -1             TO ENP4EDAL
                  GO TO ER-DATE-31-DAYS
           ELSE
                  NEXT SENTENCE.


      ********************************************
      * EXPIRATION DATE MUST BE > ISSUE DATE     *
      ********************************************

           IF (WS-EDIT-ERRORS = 'N')
               IF (ENP4EDAI > ENP4IDAI)
                   NEXT SENTENCE
               ELSE
                   GO TO ER-EXPIRATION-VS-ISSUE
           ELSE
               NEXT SENTENCE.



      *****************************************************************
      *    FIRE PERIL, FIRE PREMIUM -- VALUE MUST BE NUMERIC          *
      *****************************************************************

      ***** FIRE PERIL *****

           MOVE +4                     TO WMF-NUM-LTH
           MOVE ENP4FPEI               TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP4FPEL
              MOVE DFHBMFSE           TO ENP4FPEA
              GO TO ER-FIRE-PERIL.


           MOVE WMF-NUM-OUTPUT        TO WMF-FIRE-PERIL
           MOVE WMF-FIRE-PERIL        TO ENP4FPEO
                                         CA-B-FirePeril.


      ***** FIRE PREMIUM *****

           MOVE +8                     TO WMF-NUM-LTH
           MOVE ENP4FPRI               TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP4FPRL
              MOVE DFHBMFSE           TO ENP4FPRA
              GO TO ER-FIRE-PREMIUM.


           MOVE WMF-NUM-OUTPUT        TO WMF-FIRE-PREMIUM
           MOVE WMF-FIRE-PREMIUM      TO ENP4FPRO
                                         CA-B-FirePremium.


      *****************************************************************
      *    CRIME PERIL, CRIME PREMIUM -- VALUE MUST BE NUMERIC        *
      *****************************************************************

      ***** CRIME PERIL *****

           MOVE +4                     TO WMF-NUM-LTH
           MOVE ENP4CPEI               TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP4CPEL
              MOVE DFHBMFSE           TO ENP4CPEA
              GO TO ER-CRIME-PERIL.


           MOVE WMF-NUM-OUTPUT        TO WMF-CRIME-PERIL
           MOVE WMF-CRIME-PERIL       TO ENP4CPEO
                                         CA-B-CrimePeril.


      ***** CRIME PREMIUM *****

           MOVE +8                     TO WMF-NUM-LTH
           MOVE ENP4CPRI               TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP4CPRL
              MOVE DFHBMFSE           TO ENP4CPRA
              GO TO ER-CRIME-PREMIUM.


           MOVE WMF-NUM-OUTPUT        TO WMF-CRIME-PREMIUM
           MOVE WMF-CRIME-PREMIUM     TO ENP4CPRO
                                         CA-B-CrimePremium.



      *****************************************************************
      *    FLOOD PERIL, FLOOD PREMIUM -- VALUE MUST BE NUMERIC        *
      *****************************************************************

      ***** FLOOD PERIL *****

           MOVE +4                     TO WMF-NUM-LTH
           MOVE ENP4XPEI               TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP4XPEL
              MOVE DFHBMFSE           TO ENP4XPEA
              GO TO ER-FLOOD-PERIL.


           MOVE WMF-NUM-OUTPUT        TO WMF-FLOOD-PERIL
           MOVE WMF-FLOOD-PERIL       TO ENP4XPEO
                                         CA-B-FloodPeril.


      ***** FLOOD PREMIUM *****

           MOVE +8                     TO WMF-NUM-LTH
           MOVE ENP4XPRI               TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP4XPRL
              MOVE DFHBMFSE           TO ENP4XPRA
              GO TO ER-FLOOD-PREMIUM.


           MOVE WMF-NUM-OUTPUT        TO WMF-FLOOD-PREMIUM
           MOVE WMF-FLOOD-PREMIUM     TO ENP4XPRO
                                         CA-B-FloodPremium.




      *****************************************************************
      * WEATHER  PERIL, WEATHER PREMIUM -- VALUE MUST BE NUMERIC      *
      *****************************************************************

      ***** WEATHER PERIL *****

           MOVE +4                     TO WMF-NUM-LTH
           MOVE ENP4WPEI               TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP4WPEL
              MOVE DFHBMFSE           TO ENP4WPEA
              GO TO ER-WEATHER-PERIL.


           MOVE WMF-NUM-OUTPUT        TO WMF-WEATHER-PERIL
           MOVE WMF-WEATHER-PERIL     TO ENP4WPEO
                                         CA-B-WeatherPeril.


      ***** WEATHER PREMIUM *****

           MOVE +8                     TO WMF-NUM-LTH
           MOVE ENP4WPRI               TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR           >  ZEROES
              MOVE -1                 TO ENP4WPRL
              MOVE DFHBMFSE           TO ENP4WPRA
              GO TO ER-WEATHER-PREMIUM.


           MOVE WMF-NUM-OUTPUT        TO WMF-WEATHER-PREMIUM
           MOVE WMF-WEATHER-PREMIUM   TO ENP4WPRO
                                         CA-B-WeatherPremium.


      *****************************************************************
      *    AUTO PAY       -- VALUE MUST BE   Y  OR  N                 *
      *****************************************************************

           IF ENP4PAYI =     'Y' OR
              ENP4PAYI =     'N'
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO WS-EDIT-ERRORS
              GO TO ER-AUTO-PAY.



      *****************************************************************
      *    AUTO PAY = N   -- checking, routing, card type, number,    *
      *                   -- pin, valid thru, NOT ALLOWED             *
      *****************************************************************

           IF ENP4PAYI = 'N'
               IF (ENP4ACTI > SPACES)        OR
                  (ENP4ROUI > SPACES)        OR
                  (ENP4CCTI > SPACES)        OR
                  (ENP4CCNI > SPACES)        OR
                  (ENP4CCPI > SPACES)        OR
                  (ENP4CCVI > SPACES)
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

           IF ENP4PAYI = 'Y'
               IF ((ENP4ACTI > SPACES)       OR
                   (ENP4ROUI > SPACES))      AND
                  ((ENP4CCTI > SPACES)       OR
                   (ENP4CCNI > SPACES)       OR
                   (ENP4CCPI > SPACES)       OR
                   (ENP4CCVI > SPACES))
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

           IF ENP4PAYI = 'Y'
               IF ((ENP4ACTI = SPACES)       AND
                   (ENP4ROUI = SPACES))      AND
                  ((ENP4CCTI = SPACES)       AND
                   (ENP4CCNI = SPACES)       AND
                   (ENP4CCPI = SPACES)       AND
                   (ENP4CCVI = SPACES))
                   MOVE 'Y'                  TO WS-EDIT-ERRORS
                   GO TO ER-AUTO-PAY3
               ELSE
                   NEXT SENTENCE
           ELSE
                   NEXT SENTENCE.



      *****************************************************************
      *    FROM CHECKING   -- VALUE MUST BE NUMERIC                   *
      *****************************************************************

           IF (ENP4ACTI > SPACES)  OR
              (ENP4ROUI > SPACES)

               IF ENP4ACTI  NUMERIC
                   NEXT SENTENCE
               ELSE
                   MOVE 'Y'             TO WS-EDIT-ERRORS
                   GO TO ER-FROM-CHECKING
           ELSE
                    NEXT SENTENCE.



      *****************************************************************
      *    BANK ROUTING    -- VALUE MUST BE NUMERIC                   *
      *****************************************************************

           IF (ENP4ACTI > SPACES)  OR
              (ENP4ROUI > SPACES)

               IF ENP4ROUI  NUMERIC
                   NEXT SENTENCE
               ELSE
                   MOVE 'Y'             TO WS-EDIT-ERRORS
                   GO TO ER-BANK-ROUTING
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    CREDIT CARD TYPE                                           *
      *****************************************************************

            IF (ENP4ACTI = SPACES) AND
               (ENP4ROUI = SPACES) AND
               (ENP4PAYI = 'Y')
                IF ENP4CCTI = 'AMEX'           OR
                   ENP4CCTI = 'VISA'           OR
                   ENP4CCTI = 'MC'             OR
                   ENP4CCTI = 'DISC'
                       NEXT SENTENCE
                ELSE
                       MOVE 'Y'            TO WS-EDIT-ERRORS
                       GO TO ER-CREDIT-CARD-TYPE
            ELSE
                 NEXT SENTENCE.



      *****************************************************************
      *    CREDIT CARD NUMBER                                         *
      *****************************************************************

            IF (ENP4ACTI = SPACES)  AND
               (ENP4ROUI = SPACES)  AND
               (ENP4PAYI = 'Y')
                IF ENP4CCNI  NUMERIC
                    NEXT SENTENCE
                ELSE
                    MOVE 'Y'             TO WS-EDIT-ERRORS
                    GO TO ER-CREDIT-CARD-NUMBER
            ELSE
                    NEXT SENTENCE.



      *****************************************************************
      *    PIN NUMBER                                                 *
      *****************************************************************

            IF (ENP4ACTI = SPACES)  AND
               (ENP4ROUI = SPACES)  AND
               (ENP4PAYI = 'Y')
                IF ENP4CCPI  NUMERIC
                    NEXT SENTENCE
                ELSE
                    MOVE 'Y'             TO WS-EDIT-ERRORS
                    GO TO ER-CREDIT-CARD-PIN-NUMBER
            ELSE
                    NEXT SENTENCE.



      *****************************************************************
      *    CREDIT CARD VALID THRU                                     *
      *****************************************************************

            IF (ENP4ACTI = SPACES)  AND
               (ENP4ROUI = SPACES)  AND
               (ENP4PAYI = 'Y')
                MOVE ENP4CCVI               TO WMF-DATE-MMYY
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


