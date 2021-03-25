//PFHNCR0L JOB ('OXTBAS6.4DSP'),'LGBAT003 ',CLASS=A,MSGCLASS=X,         00003116
//       NOTIFY=&SYSUID,REGION=0M                                       00003216
/*JOBPARM  S=CW09                                                       00003316
//*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++           00004000
//DBPROC   EXEC PGM=DFSRRC00,                                           00005001
//         PARM='DLI,LGBAT003,LGBAT003,,,,,,,,,,,N',                    00006000
//         COND=(4,LT)                                                  00007000
//STEPLIB  DD DSN=IMS141A.SDFSRESL,DISP=SHR                             00008001
//         DD DSN=GENAPP.ISPW.TEST.LOAD,DISP=SHR                        00008105
//         DD DISP=SHR,DSN=CEE.SCEERUN                                  00008210
//DFSESL   DD DSN=IMS141A.SDFSRESL,DISP=SHR                             00008310
//DFSRESLB DD DSN=IMS141A.SDFSRESL,DISP=SHR                             00009001
//IMS      DD DSN=GENAPP.IMS141A.DBDLIB,DISP=SHR                        00010001
//         DD DSN=GENAPP.IMS141A.PSBLIB,DISP=SHR                        00020001
//CUSTMRDD DD DSN=GENAPP.CUSTMRDD,DISP=SHR                              00041013
//CUSTMXDD DD DSN=GENAPP.CUSTMXDD,DISP=SHR                              00042013
//IPARAMS  DD DSN=CSUP1.IGENAPP.BATCH.PARM,DISP=SHR                     00043016
//PRINTDD  DD SYSOUT=*                                                  00050000
//SYSPRINT DD SYSOUT=*                                                  00060000
//IEFRDER  DD DUMMY                                                     00070000
//DFSVSAMP DD DSN=GENAPP.IMS.CNTL(DFSVSAMP),DISP=SHR                    00080001
//SYSTSPRT  DD SYSOUT=*,DCB=BLKSIZE=2420                                00081010
//SYSTSMRT DD SYSOUT=*,DCB=BLKSIZE=2420                                 00090004
//ABENDAID DD SYSOUT=*                                                  00110002
//ABNLTERM DD SYSOUT=*                                                  00120002
//ABNLTRAC DD SYSOUT=*                                                  00130002
//SYSUDUMP DD SYSOUT=*                                                  00140002
//SYSOUT   DD SYSOUT=*                                                  00150002
//INVRPTO  DD SYSOUT=*                                                  00160002
//                                                                      00210006
