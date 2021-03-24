//${user_id}X JOB (MMC),'COBOL TEST',CLASS=L,
//  MSGCLASS=X,MSGLEVEL=(1,1),NOTIFY=&SYSUID
/*JOBPARM S=*
//*
//CWBWPROC PROC
//* *******************************************************************
//*
//* EXECUTE CWBWCOBX, E.G. FOR DEBUGGING
//*
//*  INSTRUCTIONS:
//*  =============
//*
//*  1) CHANGE PATH PARAMETER TO THE PATH IN YOUR ISPW APPLICATION
//*
//* *******************************************************************
//*
//CWBWCOBX EXEC PGM=CWBWCOBX,PARM='00003,200319'
//STEPLIB  DD   DISP=SHR,DSN=SALESSUP.&APPL..UT&PATH..LOAD
//         DD   DISP=SHR,DSN=SALESSUP.&APPL..FT&PATH..LOAD
//         DD   DISP=SHR,DSN=SALESSUP.&APPL..DEVL.LOAD
//         DD   DISP=SHR,DSN=SALESSUP.&APPL..MAIN.LOAD
//         DD   DISP=SHR,DSN=SALESSUP.&APPL..PROD.LOAD
//         DD   DSN=CEE.SCEERUN,DISP=SHR
//EMPFILE  DD   DSN=SALESSUP.&APPL..TEST.CWXTDATA,
//         DISP=SHR
//RPTFILE  DD   SYSOUT=X
//SYSOUT   DD   SYSOUT=X
//         PEND
//CWBWCOBX EXEC CWBWPROC,
//         APPL=${ispw_app_value},
//         PATH=${ispw_path_num}