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
      *               COPYBOOK for COMMAREA structure                  *
      *                                                                *
      *   This commarea can be used for all functions                  *
      *                                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
           03 CA-REQUEST-ID            PIC X(6).
           03 CA-RETURN-CODE           PIC 9(2).
           03 CA-CUSTOMER-NUM          PIC 9(10).
           03 CA-REQUEST-SPECIFIC      PIC X(32482).
      *    Fields used in INQ All and ADD customer
           03 CA-CUSTOMER-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
              05 CA-FIRST-NAME         PIC X(10).
              05 CA-LAST-NAME          PIC X(20).
              05 CA-DOB                PIC X(10).
              05 CA-HOUSE-NAME         PIC X(20).
              05 CA-HOUSE-NUM          PIC X(4).
              05 CA-POSTCODE           PIC X(8).
              05 CA-NUM-POLICIES       PIC 9(3).
              05 CA-PHONE-MOBILE       PIC X(20).
              05 CA-PHONE-HOME         PIC X(20).
              05 CA-EMAIL-ADDRESS      PIC X(100).
              05 CA-NATIONAL-ID-NBR    PIC X(20).
              05 CA-NIN-TYPE           PIC X(2).
              05 CA-STREET-ADDRESS     PIC X(30).
              05 CA-CITY               PIC X(20).
              05 CA-STATE              PIC X(2).
              05 CA-COUNTRY-CODE       PIC X(3).
              05 CA-SALES-TERRITORY    PIC X(5).
      ****PWB*** ADDED PFKEY, REDUCED CA-POLICY-DATA BY 2
              05 CA-CUSTOMER-PFKEY     PIC X(2).
      ****PWB*** ADDED CUSTOMER-BEGIN-END REDUCED CA-POLICY-DATA BY 20
              05 CA-CUSTOMER-BEGIN     PIC 9(10).
              05 CA-CUSTOMER-END       PIC 9(10).
              05 CA-POLICY-DATA        PIC X(32163).
      *    Fields used in Customer security call
           03 CA-CUSTSECR-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
              05 CA-CUSTSECR-PASS      PIC X(32).
              05 CA-CUSTSECR-COUNT     PIC X(4).
              05 CA-CUSTSECR-STATE     PIC X.
              05 CA-CUSTSECR-DATA      PIC X(32445).
      *    Fields used in INQ, UPD, ADD & DELETE policy
           03 CA-POLICY-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
              05 CA-POLICY-NUM         PIC 9(10).
      *       Common policy details
              05 CA-POLICY-COMMON.
                 07 CA-ISSUE-DATE      PIC X(10).
                 07 CA-EXPIRY-DATE     PIC X(10).
                 07 CA-LASTCHANGED     PIC X(26).
                 07 CA-BROKERID        PIC 9(10).
                 07 CA-BROKERSREF      PIC X(10).
                 07 CA-PAYMENT         PIC 9(6).
      ****PWB*** ADDED PFKEY, REDUCED CA-POLICY-SPECIFIC BY 2
              05 CA-POLICY-PFKEY       PIC X(2).
      ****PWB*** ADDED POLICY-BEGIN-END REDUCED CA-POLICY-DATA BY 20
              05 CA-POLICY-BEGIN       PIC 9(10).
              05 CA-POLICY-END         PIC 9(10).
              05 CA-POLICY-SPECIFIC    PIC X(32378).
      *       Endowment policy description
              05 CA-ENDOWMENT REDEFINES CA-POLICY-SPECIFIC.
                 07 CA-E-WITH-PROFITS    PIC X.
                 07 CA-E-EQUITIES        PIC X.
                 07 CA-E-MANAGED-FUND    PIC X.
                 07 CA-E-FUND-NAME       PIC X(10).
                 07 CA-E-TERM            PIC 99.
                 07 CA-E-SUM-ASSURED     PIC 9(6).
                 07 CA-E-LIFE-ASSURED    PIC X(31).
                 07 CA-E-AUTO-PAY        PIC X(1).
                 07 CA-E-CHECK-ACCT-NBR  PIC X(12).
                 07 CA-E-BANK-ROUTE-CODE PIC X(9).
                 07 CA-E-CREDIT-CARD-TYP PIC X(8).
                 07 CA-E-CREDIT-CARD-NBR PIC X(16).
                 07 CA-E-CREDIT-CARD-PIN PIC X(4).
                 07 CA-E-CREDIT-CARD-VAL PIC X(5).
                 07 CA-E-PADDING-DATA    PIC X(32293).
      *       House policy description
              05 CA-HOUSE REDEFINES CA-POLICY-SPECIFIC.
                 07 CA-H-PROPERTY-TYPE   PIC X(15).
                 07 CA-H-BEDROOMS        PIC 9(3).
                 07 CA-H-VALUE           PIC 9(8).
                 07 CA-H-HOUSE-NAME      PIC X(20).
                 07 CA-H-HOUSE-NUMBER    PIC X(4).
                 07 CA-H-POSTCODE        PIC X(8).
                 07 CA-H-STREET-ADDRESS  PIC X(30).
                 07 CA-H-CITY            PIC X(20).
                 07 CA-H-STATE           PIC X(2).
                 07 CA-H-COUNTRY-CODE    PIC X(3).
                 07 CA-H-AUTO-PAY        PIC X(1).
                 07 CA-H-CHECK-ACCT-NBR  PIC X(12).
                 07 CA-H-BANK-ROUTE-CODE PIC X(9).
                 07 CA-H-CREDIT-CARD-TYP PIC X(8).
                 07 CA-H-CREDIT-CARD-NBR PIC X(16).
                 07 CA-H-CREDIT-CARD-PIN PIC X(4).
                 07 CA-H-CREDIT-CARD-VAL PIC X(5).
                 07 CA-H-FILLER          PIC X(32232).
      *       Motor policy description
              05 CA-MOTOR REDEFINES CA-POLICY-SPECIFIC.
                 07 CA-M-MAKE            PIC X(15).
                 07 CA-M-MODEL           PIC X(15).
                 07 CA-M-VALUE           PIC 9(6).
                 07 CA-M-REGNUMBER       PIC X(7).
                 07 CA-M-COLOUR          PIC X(8).
                 07 CA-M-CC              PIC 9(4).
                 07 CA-M-MANUFACTURED    PIC X(10).
                 07 CA-M-PREMIUM         PIC 9(6).
                 07 CA-M-ACCIDENTS       PIC 9(6).
                 07 CA-M-CAR-YEAR        PIC X(4).
                 07 CA-M-CAR-VIN         PIC X(20).
                 07 CA-M-AUTO-PAY        PIC X(1).
                 07 CA-M-CHECK-ACCT-NBR  PIC X(12).
                 07 CA-M-BANK-ROUTE-CODE PIC X(9).
                 07 CA-M-CREDIT-CARD-TYP PIC X(8).
                 07 CA-M-CREDIT-CARD-NBR PIC X(16).
                 07 CA-M-CREDIT-CARD-PIN PIC X(4).
                 07 CA-M-CREDIT-CARD-VAL PIC X(5).
                 07 CA-M-FILLER          PIC X(32244).
      *       Commercial policy description
              05 CA-COMMERCIAL REDEFINES CA-POLICY-SPECIFIC.
                 07 CA-B-Address         PIC X(255).
                 07 CA-B-Postcode        PIC X(8).
                 07 CA-B-Latitude        PIC X(11).
                 07 CA-B-Longitude       PIC X(11).
                 07 CA-B-Customer        PIC X(255).
                 07 CA-B-PropType        PIC X(255).
                 07 CA-B-FirePeril       PIC 9(4).
                 07 CA-B-FirePremium     PIC 9(8).
                 07 CA-B-CrimePeril      PIC 9(4).
                 07 CA-B-CrimePremium    PIC 9(8).
                 07 CA-B-FloodPeril      PIC 9(4).
                 07 CA-B-FloodPremium    PIC 9(8).
                 07 CA-B-WeatherPeril    PIC 9(4).
                 07 CA-B-WeatherPremium  PIC 9(8).
                 07 CA-B-Status          PIC 9(4).
                 07 CA-B-RejectReason    PIC X(255).
                 07 CA-B-STREET-ADDRESS  PIC X(30).
                 07 CA-B-CITY            PIC X(20).
                 07 CA-B-STATE           PIC X(2).
                 07 CA-B-COUNTRY-CODE    PIC X(3).
                 07 CA-B-AUTO-PAY        PIC X(1).
                 07 CA-B-CHECK-ACCT-NBR  PIC X(12).
                 07 CA-B-BANK-ROUTE-CODE PIC X(9).
                 07 CA-B-CREDIT-CARD-TYP PIC X(8).
                 07 CA-B-CREDIT-CARD-NBR PIC X(16).
                 07 CA-B-CREDIT-CARD-PIN PIC X(4).
                 07 CA-B-CREDIT-CARD-VAL PIC X(5).
                 07 CA-B-FILLER          PIC X(31188).
      *       CLAIM policy description
              05 CA-CLAIM      REDEFINES CA-POLICY-SPECIFIC.
                 07 CA-C-Num             PIC 9(10).
                 07 CA-C-Date            PIC X(10).
                 07 CA-C-Paid            PIC 9(8).
                 07 CA-C-Value           PIC 9(8).
                 07 CA-C-Cause           PIC X(255).
                 07 CA-C-Observations    PIC X(255).
                 07 CA-C-FILLER          PIC X(31854).
