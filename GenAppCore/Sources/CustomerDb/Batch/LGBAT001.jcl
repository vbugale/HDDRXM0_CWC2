//PFHNCR0L JOB ('OXTBAS6.4DSP'),'LGBAT001 ',CLASS=A,MSGCLASS=X,
//       NOTIFY=&SYSUID,REGION=0M
/*JOBPARM  S=CW09
//STEP1   EXEC PGM=IKJEFT01,DYNAMNBR=20                                 00240004
//STEPLIB   DD DISP=SHR,DSN=DSNC10.SDSNLOAD                             00250004
//          DD DISP=SHR,DSN=DSNC10.DC01.RUNLIB.LOAD                     00260004
//          DD DISP=SHR,DSN=GENAPP.ISPW.TEST.LOAD                       00260004
//          DD DISP=SHR,DSN=CEE.SCEERUN
//SYSTSPRT  DD SYSOUT=*,DCB=BLKSIZE=2420                                00280004
//SYSPRINT  DD SYSOUT=*                                                 00290004
//ABNLTERM  DD SYSOUT=*                                                 00300004
//ABNLTRAC  DD SYSOUT=*                                                 00310004
//SYSUDUMP  DD SYSOUT=*                                                 00320004
//SYSOUT    DD SYSOUT=*                                                 00330004
//INVRPTO   DD SYSOUT=*                                                 00330004
//IPARAMS   DD DSN=CSUP1.IGENAPP.BATCH.PARM,DISP=SHR
//IORDERS   DD DSN=CSUP1.IGENAPP.BATCH.IORDERS,DISP=SHR
//IPARTS    DD DSN=CSUP1.IGENAPP.BATCH.IPARTS,DISP=SHR
//VCUSTOMR  DD DSN=CSUP1.IGENAPP.CUST,DISP=SHR
//VPOLICY   DD DSN=CSUP1.IGENAPP.POLY,DISP=SHR
//SYSTSIN   DD *
  DSN SYSTEM(DC01)
  RUN PROGRAM(LGBAT001) PLAN(GENAPP) -
  LIB('GENAPP.ISPW.TEST.LOAD')
  END
/*
//
