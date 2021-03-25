//${user_id}X JOB ('FTS'),'GITDEMO',NOTIFY=&SYSUID,
//             MSGLEVEL=(1,1),MSGCLASS=X,CLASS=A,REGION=6M
/*JOBPARM S=*
//*
//CWXTPROC PROC
//* *******************************************************************
//*
//* EXECUTE CWXTCOB, E.G. FOR DEBUGGING
//*
//*  INSTRUCTIONS:
//*  =============
//*
//*  1) CHANGE PATH PARAMETER TO THE PATH IN YOUR ISPW APPLICATION
//*
//* *******************************************************************
//*
//STEP1  EXEC PGM=CWXTCOB,PARM=00003
//STEPLIB  DD   DISP=SHR,DSN=SALESSUP.&APPL..UT&PATH..LOAD
//         DD   DISP=SHR,DSN=SALESSUP.&APPL..FT&PATH..LOAD
//         DD   DISP=SHR,DSN=SALESSUP.&APPL..DEVL.LOAD
//         DD   DISP=SHR,DSN=SALESSUP.&APPL..MAIN.LOAD
//         DD   DISP=SHR,DSN=SALESSUP.&APPL..PROD.LOAD
//         DD   DSN=CEE.SCEERUN,DISP=SHR
//EMPFILE  DD  DISP=SHR,DSN=SALESSUP.&APPL..TEST.CWXTDATA
//RPTFILE  DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//*ABNLIGNR DD  DUMMY
//SYSOUT   DD  SYSOUT=*
//         PEND
//CWXTCOB  EXEC CWXTPROC,
//         APPL=${ispw_app_value},
//         PATH=${ispw_path_num}