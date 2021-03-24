       01  SSMAPC1I.
           02  FILLER PIC X(12).
           02  ENT1CDTL    COMP  PIC  S9(4).
           02  ENT1CDTF    PICTURE X.
           02  FILLER REDEFINES ENT1CDTF.
             03 ENT1CDTA    PICTURE X.
           02  ENT1CDTI  PIC X(10).
           02  ENT1OPTL    COMP  PIC  S9(4).
           02  ENT1OPTF    PICTURE X.
           02  FILLER REDEFINES ENT1OPTF.
             03 ENT1OPTA    PICTURE X.
           02  ENT1OPTI  PIC X(1).
           02  ENT1CNOL    COMP  PIC  S9(4).
           02  ENT1CNOF    PICTURE X.
           02  FILLER REDEFINES ENT1CNOF.
             03 ENT1CNOA    PICTURE X.
           02  ENT1CNOI  PIC X(10).
           02  ENT1DATL    COMP  PIC  S9(4).
           02  ENT1DATF    PICTURE X.
           02  FILLER REDEFINES ENT1DATF.
             03 ENT1DATA    PICTURE X.
           02  ENT1DATI  PIC X(13).
           02  ENT1FNAL    COMP  PIC  S9(4).
           02  ENT1FNAF    PICTURE X.
           02  FILLER REDEFINES ENT1FNAF.
             03 ENT1FNAA    PICTURE X.
           02  ENT1FNAI  PIC X(10).
           02  ENT1LNAL    COMP  PIC  S9(4).
           02  ENT1LNAF    PICTURE X.
           02  FILLER REDEFINES ENT1LNAF.
             03 ENT1LNAA    PICTURE X.
           02  ENT1LNAI  PIC X(20).
           02  ENT1DOBL    COMP  PIC  S9(4).
           02  ENT1DOBF    PICTURE X.
           02  FILLER REDEFINES ENT1DOBF.
             03 ENT1DOBA    PICTURE X.
           02  ENT1DOBI  PIC X(10).
           02  ENT1NINL    COMP  PIC  S9(4).
           02  ENT1NINF    PICTURE X.
           02  FILLER REDEFINES ENT1NINF.
             03 ENT1NINA    PICTURE X.
           02  ENT1NINI  PIC X(20).
           02  ENT1TYPL    COMP  PIC  S9(4).
           02  ENT1TYPF    PICTURE X.
           02  FILLER REDEFINES ENT1TYPF.
             03 ENT1TYPA    PICTURE X.
           02  ENT1TYPI  PIC X(2).
           02  ENT1STRL    COMP  PIC  S9(4).
           02  ENT1STRF    PICTURE X.
           02  FILLER REDEFINES ENT1STRF.
             03 ENT1STRA    PICTURE X.
           02  ENT1STRI  PIC X(30).
           02  ENT1CITL    COMP  PIC  S9(4).
           02  ENT1CITF    PICTURE X.
           02  FILLER REDEFINES ENT1CITF.
             03 ENT1CITA    PICTURE X.
           02  ENT1CITI  PIC X(20).
           02  ENT1STAL    COMP  PIC  S9(4).
           02  ENT1STAF    PICTURE X.
           02  FILLER REDEFINES ENT1STAF.
             03 ENT1STAA    PICTURE X.
           02  ENT1STAI  PIC X(2).
           02  ENT1COUL    COMP  PIC  S9(4).
           02  ENT1COUF    PICTURE X.
           02  FILLER REDEFINES ENT1COUF.
             03 ENT1COUA    PICTURE X.
           02  ENT1COUI  PIC X(3).
           02  ENT1POSL    COMP  PIC  S9(4).
           02  ENT1POSF    PICTURE X.
           02  FILLER REDEFINES ENT1POSF.
             03 ENT1POSA    PICTURE X.
           02  ENT1POSI  PIC X(8).
           02  ENT1TERL    COMP  PIC  S9(4).
           02  ENT1TERF    PICTURE X.
           02  FILLER REDEFINES ENT1TERF.
             03 ENT1TERA    PICTURE X.
           02  ENT1TERI  PIC X(5).
           02  ENT1PH1L    COMP  PIC  S9(4).
           02  ENT1PH1F    PICTURE X.
           02  FILLER REDEFINES ENT1PH1F.
             03 ENT1PH1A    PICTURE X.
           02  ENT1PH1I  PIC X(10).
           02  ENT1PH2L    COMP  PIC  S9(4).
           02  ENT1PH2F    PICTURE X.
           02  FILLER REDEFINES ENT1PH2F.
             03 ENT1PH2A    PICTURE X.
           02  ENT1PH2I  PIC X(10).
           02  ENT1EMAL    COMP  PIC  S9(4).
           02  ENT1EMAF    PICTURE X.
           02  FILLER REDEFINES ENT1EMAF.
             03 ENT1EMAA    PICTURE X.
           02  ENT1EMAI  PIC X(25).
           02  ERRFLDL    COMP  PIC  S9(4).
           02  ERRFLDF    PICTURE X.
           02  FILLER REDEFINES ERRFLDF.
             03 ERRFLDA    PICTURE X.
           02  ERRFLDI  PIC X(40).
       01  SSMAPC1O REDEFINES SSMAPC1I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  ENT1CDTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENT1OPTO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ENT1CNOO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENT1DATO  PIC X(13).
           02  FILLER PICTURE X(3).
           02  ENT1FNAO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENT1LNAO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  ENT1DOBO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENT1NINO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  ENT1TYPO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  ENT1STRO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  ENT1CITO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  ENT1STAO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  ENT1COUO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  ENT1POSO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ENT1TERO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  ENT1PH1O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENT1PH2O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENT1EMAO  PIC X(25).
           02  FILLER PICTURE X(3).
           02  ERRFLDO  PIC X(40).
       01  SSMAPP1I.
           02  FILLER PIC X(12).
           02  ENP1CDTL    COMP  PIC  S9(4).
           02  ENP1CDTF    PICTURE X.
           02  FILLER REDEFINES ENP1CDTF.
             03 ENP1CDTA    PICTURE X.
           02  ENP1CDTI  PIC X(10).
           02  ENP1OPTL    COMP  PIC  S9(4).
           02  ENP1OPTF    PICTURE X.
           02  FILLER REDEFINES ENP1OPTF.
             03 ENP1OPTA    PICTURE X.
           02  ENP1OPTI  PIC X(1).
           02  ENP1PNOL    COMP  PIC  S9(4).
           02  ENP1PNOF    PICTURE X.
           02  FILLER REDEFINES ENP1PNOF.
             03 ENP1PNOA    PICTURE X.
           02  ENP1PNOI  PIC X(10).
           02  ENP1DATL    COMP  PIC  S9(4).
           02  ENP1DATF    PICTURE X.
           02  FILLER REDEFINES ENP1DATF.
             03 ENP1DATA    PICTURE X.
           02  ENP1DATI  PIC X(13).
           02  ENP1CNOL    COMP  PIC  S9(4).
           02  ENP1CNOF    PICTURE X.
           02  FILLER REDEFINES ENP1CNOF.
             03 ENP1CNOA    PICTURE X.
           02  ENP1CNOI  PIC X(10).
           02  ENP1IDAL    COMP  PIC  S9(4).
           02  ENP1IDAF    PICTURE X.
           02  FILLER REDEFINES ENP1IDAF.
             03 ENP1IDAA    PICTURE X.
           02  ENP1IDAI  PIC X(10).
           02  ENP1EDAL    COMP  PIC  S9(4).
           02  ENP1EDAF    PICTURE X.
           02  FILLER REDEFINES ENP1EDAF.
             03 ENP1EDAA    PICTURE X.
           02  ENP1EDAI  PIC X(10).
           02  ENP1YEAL    COMP  PIC  S9(4).
           02  ENP1YEAF    PICTURE X.
           02  FILLER REDEFINES ENP1YEAF.
             03 ENP1YEAA    PICTURE X.
           02  ENP1YEAI  PIC X(4).
           02  ENP1CMKL    COMP  PIC  S9(4).
           02  ENP1CMKF    PICTURE X.
           02  FILLER REDEFINES ENP1CMKF.
             03 ENP1CMKA    PICTURE X.
           02  ENP1CMKI  PIC X(20).
           02  ENP1CMOL    COMP  PIC  S9(4).
           02  ENP1CMOF    PICTURE X.
           02  FILLER REDEFINES ENP1CMOF.
             03 ENP1CMOA    PICTURE X.
           02  ENP1CMOI  PIC X(20).
           02  ENP1COLL    COMP  PIC  S9(4).
           02  ENP1COLF    PICTURE X.
           02  FILLER REDEFINES ENP1COLF.
             03 ENP1COLA    PICTURE X.
           02  ENP1COLI  PIC X(8).
           02  ENP1VINL    COMP  PIC  S9(4).
           02  ENP1VINF    PICTURE X.
           02  FILLER REDEFINES ENP1VINF.
             03 ENP1VINA    PICTURE X.
           02  ENP1VINI  PIC X(20).
           02  ENP1PREL    COMP  PIC  S9(4).
           02  ENP1PREF    PICTURE X.
           02  FILLER REDEFINES ENP1PREF.
             03 ENP1PREA    PICTURE X.
           02  ENP1PREI  PIC X(6).
           02  ENP1PAYL    COMP  PIC  S9(4).
           02  ENP1PAYF    PICTURE X.
           02  FILLER REDEFINES ENP1PAYF.
             03 ENP1PAYA    PICTURE X.
           02  ENP1PAYI  PIC X(1).
           02  ENP1ACTL    COMP  PIC  S9(4).
           02  ENP1ACTF    PICTURE X.
           02  FILLER REDEFINES ENP1ACTF.
             03 ENP1ACTA    PICTURE X.
           02  ENP1ACTI  PIC X(12).
           02  ENP1ROUL    COMP  PIC  S9(4).
           02  ENP1ROUF    PICTURE X.
           02  FILLER REDEFINES ENP1ROUF.
             03 ENP1ROUA    PICTURE X.
           02  ENP1ROUI  PIC X(9).
           02  ENP1CCTL    COMP  PIC  S9(4).
           02  ENP1CCTF    PICTURE X.
           02  FILLER REDEFINES ENP1CCTF.
             03 ENP1CCTA    PICTURE X.
           02  ENP1CCTI  PIC X(8).
           02  ENP1CCNL    COMP  PIC  S9(4).
           02  ENP1CCNF    PICTURE X.
           02  FILLER REDEFINES ENP1CCNF.
             03 ENP1CCNA    PICTURE X.
           02  ENP1CCNI  PIC X(16).
           02  ENP1CCPL    COMP  PIC  S9(4).
           02  ENP1CCPF    PICTURE X.
           02  FILLER REDEFINES ENP1CCPF.
             03 ENP1CCPA    PICTURE X.
           02  ENP1CCPI  PIC X(4).
           02  ENP1CCVL    COMP  PIC  S9(4).
           02  ENP1CCVF    PICTURE X.
           02  FILLER REDEFINES ENP1CCVF.
             03 ENP1CCVA    PICTURE X.
           02  ENP1CCVI  PIC X(5).
           02  ERP1FLDL    COMP  PIC  S9(4).
           02  ERP1FLDF    PICTURE X.
           02  FILLER REDEFINES ERP1FLDF.
             03 ERP1FLDA    PICTURE X.
           02  ERP1FLDI  PIC X(40).
       01  SSMAPP1O REDEFINES SSMAPP1I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  ENP1CDTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP1OPTO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ENP1PNOO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP1DATO  PIC X(13).
           02  FILLER PICTURE X(3).
           02  ENP1CNOO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP1IDAO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP1EDAO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP1YEAO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ENP1CMKO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  ENP1CMOO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  ENP1COLO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ENP1VINO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  ENP1PREO  PIC X(6).
           02  FILLER PICTURE X(3).
           02  ENP1PAYO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ENP1ACTO  PIC X(12).
           02  FILLER PICTURE X(3).
           02  ENP1ROUO  PIC X(9).
           02  FILLER PICTURE X(3).
           02  ENP1CCTO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ENP1CCNO  PIC X(16).
           02  FILLER PICTURE X(3).
           02  ENP1CCPO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ENP1CCVO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  ERP1FLDO  PIC X(40).
       01  SSMAPP2I.
           02  FILLER PIC X(12).
           02  ENP2CDTL    COMP  PIC  S9(4).
           02  ENP2CDTF    PICTURE X.
           02  FILLER REDEFINES ENP2CDTF.
             03 ENP2CDTA    PICTURE X.
           02  ENP2CDTI  PIC X(10).
           02  ENP2OPTL    COMP  PIC  S9(4).
           02  ENP2OPTF    PICTURE X.
           02  FILLER REDEFINES ENP2OPTF.
             03 ENP2OPTA    PICTURE X.
           02  ENP2OPTI  PIC X(1).
           02  ENP2PNOL    COMP  PIC  S9(4).
           02  ENP2PNOF    PICTURE X.
           02  FILLER REDEFINES ENP2PNOF.
             03 ENP2PNOA    PICTURE X.
           02  ENP2PNOI  PIC X(10).
           02  ENP2DATL    COMP  PIC  S9(4).
           02  ENP2DATF    PICTURE X.
           02  FILLER REDEFINES ENP2DATF.
             03 ENP2DATA    PICTURE X.
           02  ENP2DATI  PIC X(13).
           02  ENP2CNOL    COMP  PIC  S9(4).
           02  ENP2CNOF    PICTURE X.
           02  FILLER REDEFINES ENP2CNOF.
             03 ENP2CNOA    PICTURE X.
           02  ENP2CNOI  PIC X(10).
           02  ENP2IDAL    COMP  PIC  S9(4).
           02  ENP2IDAF    PICTURE X.
           02  FILLER REDEFINES ENP2IDAF.
             03 ENP2IDAA    PICTURE X.
           02  ENP2IDAI  PIC X(10).
           02  ENP2EDAL    COMP  PIC  S9(4).
           02  ENP2EDAF    PICTURE X.
           02  FILLER REDEFINES ENP2EDAF.
             03 ENP2EDAA    PICTURE X.
           02  ENP2EDAI  PIC X(10).
           02  ENP2FNML    COMP  PIC  S9(4).
           02  ENP2FNMF    PICTURE X.
           02  FILLER REDEFINES ENP2FNMF.
             03 ENP2FNMA    PICTURE X.
           02  ENP2FNMI  PIC X(10).
           02  ENP2TERL    COMP  PIC  S9(4).
           02  ENP2TERF    PICTURE X.
           02  FILLER REDEFINES ENP2TERF.
             03 ENP2TERA    PICTURE X.
           02  ENP2TERI  PIC X(2).
           02  ENP2SUML    COMP  PIC  S9(4).
           02  ENP2SUMF    PICTURE X.
           02  FILLER REDEFINES ENP2SUMF.
             03 ENP2SUMA    PICTURE X.
           02  ENP2SUMI  PIC X(6).
           02  ENP2LIFL    COMP  PIC  S9(4).
           02  ENP2LIFF    PICTURE X.
           02  FILLER REDEFINES ENP2LIFF.
             03 ENP2LIFA    PICTURE X.
           02  ENP2LIFI  PIC X(12).
           02  ENP2WPRL    COMP  PIC  S9(4).
           02  ENP2WPRF    PICTURE X.
           02  FILLER REDEFINES ENP2WPRF.
             03 ENP2WPRA    PICTURE X.
           02  ENP2WPRI  PIC X(1).
           02  ENP2EQUL    COMP  PIC  S9(4).
           02  ENP2EQUF    PICTURE X.
           02  FILLER REDEFINES ENP2EQUF.
             03 ENP2EQUA    PICTURE X.
           02  ENP2EQUI  PIC X(1).
           02  ENP2MANL    COMP  PIC  S9(4).
           02  ENP2MANF    PICTURE X.
           02  FILLER REDEFINES ENP2MANF.
             03 ENP2MANA    PICTURE X.
           02  ENP2MANI  PIC X(1).
           02  ENP2PAYL    COMP  PIC  S9(4).
           02  ENP2PAYF    PICTURE X.
           02  FILLER REDEFINES ENP2PAYF.
             03 ENP2PAYA    PICTURE X.
           02  ENP2PAYI  PIC X(1).
           02  ENP2ACTL    COMP  PIC  S9(4).
           02  ENP2ACTF    PICTURE X.
           02  FILLER REDEFINES ENP2ACTF.
             03 ENP2ACTA    PICTURE X.
           02  ENP2ACTI  PIC X(12).
           02  ENP2ROUL    COMP  PIC  S9(4).
           02  ENP2ROUF    PICTURE X.
           02  FILLER REDEFINES ENP2ROUF.
             03 ENP2ROUA    PICTURE X.
           02  ENP2ROUI  PIC X(9).
           02  ENP2CCTL    COMP  PIC  S9(4).
           02  ENP2CCTF    PICTURE X.
           02  FILLER REDEFINES ENP2CCTF.
             03 ENP2CCTA    PICTURE X.
           02  ENP2CCTI  PIC X(8).
           02  ENP2CCNL    COMP  PIC  S9(4).
           02  ENP2CCNF    PICTURE X.
           02  FILLER REDEFINES ENP2CCNF.
             03 ENP2CCNA    PICTURE X.
           02  ENP2CCNI  PIC X(16).
           02  ENP2CCPL    COMP  PIC  S9(4).
           02  ENP2CCPF    PICTURE X.
           02  FILLER REDEFINES ENP2CCPF.
             03 ENP2CCPA    PICTURE X.
           02  ENP2CCPI  PIC X(4).
           02  ENP2CCVL    COMP  PIC  S9(4).
           02  ENP2CCVF    PICTURE X.
           02  FILLER REDEFINES ENP2CCVF.
             03 ENP2CCVA    PICTURE X.
           02  ENP2CCVI  PIC X(5).
           02  ERP2FLDL    COMP  PIC  S9(4).
           02  ERP2FLDF    PICTURE X.
           02  FILLER REDEFINES ERP2FLDF.
             03 ERP2FLDA    PICTURE X.
           02  ERP2FLDI  PIC X(40).
       01  SSMAPP2O REDEFINES SSMAPP2I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  ENP2CDTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP2OPTO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ENP2PNOO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP2DATO  PIC X(13).
           02  FILLER PICTURE X(3).
           02  ENP2CNOO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP2IDAO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP2EDAO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP2FNMO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP2TERO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  ENP2SUMO  PIC X(6).
           02  FILLER PICTURE X(3).
           02  ENP2LIFO  PIC X(12).
           02  FILLER PICTURE X(3).
           02  ENP2WPRO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ENP2EQUO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ENP2MANO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ENP2PAYO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ENP2ACTO  PIC X(12).
           02  FILLER PICTURE X(3).
           02  ENP2ROUO  PIC X(9).
           02  FILLER PICTURE X(3).
           02  ENP2CCTO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ENP2CCNO  PIC X(16).
           02  FILLER PICTURE X(3).
           02  ENP2CCPO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ENP2CCVO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  ERP2FLDO  PIC X(40).
       01  SSMAPP3I.
           02  FILLER PIC X(12).
           02  ENP3CDTL    COMP  PIC  S9(4).
           02  ENP3CDTF    PICTURE X.
           02  FILLER REDEFINES ENP3CDTF.
             03 ENP3CDTA    PICTURE X.
           02  ENP3CDTI  PIC X(10).
           02  ENP3OPTL    COMP  PIC  S9(4).
           02  ENP3OPTF    PICTURE X.
           02  FILLER REDEFINES ENP3OPTF.
             03 ENP3OPTA    PICTURE X.
           02  ENP3OPTI  PIC X(1).
           02  ENP3PNOL    COMP  PIC  S9(4).
           02  ENP3PNOF    PICTURE X.
           02  FILLER REDEFINES ENP3PNOF.
             03 ENP3PNOA    PICTURE X.
           02  ENP3PNOI  PIC X(10).
           02  ENP3DATL    COMP  PIC  S9(4).
           02  ENP3DATF    PICTURE X.
           02  FILLER REDEFINES ENP3DATF.
             03 ENP3DATA    PICTURE X.
           02  ENP3DATI  PIC X(13).
           02  ENP3CNOL    COMP  PIC  S9(4).
           02  ENP3CNOF    PICTURE X.
           02  FILLER REDEFINES ENP3CNOF.
             03 ENP3CNOA    PICTURE X.
           02  ENP3CNOI  PIC X(10).
           02  ENP3IDAL    COMP  PIC  S9(4).
           02  ENP3IDAF    PICTURE X.
           02  FILLER REDEFINES ENP3IDAF.
             03 ENP3IDAA    PICTURE X.
           02  ENP3IDAI  PIC X(10).
           02  ENP3EDAL    COMP  PIC  S9(4).
           02  ENP3EDAF    PICTURE X.
           02  FILLER REDEFINES ENP3EDAF.
             03 ENP3EDAA    PICTURE X.
           02  ENP3EDAI  PIC X(10).
           02  ENP3TYPL    COMP  PIC  S9(4).
           02  ENP3TYPF    PICTURE X.
           02  FILLER REDEFINES ENP3TYPF.
             03 ENP3TYPA    PICTURE X.
           02  ENP3TYPI  PIC X(15).
           02  ENP3BEDL    COMP  PIC  S9(4).
           02  ENP3BEDF    PICTURE X.
           02  FILLER REDEFINES ENP3BEDF.
             03 ENP3BEDA    PICTURE X.
           02  ENP3BEDI  PIC X(3).
           02  ENP3VALL    COMP  PIC  S9(4).
           02  ENP3VALF    PICTURE X.
           02  FILLER REDEFINES ENP3VALF.
             03 ENP3VALA    PICTURE X.
           02  ENP3VALI  PIC X(8).
           02  ENP3STRL    COMP  PIC  S9(4).
           02  ENP3STRF    PICTURE X.
           02  FILLER REDEFINES ENP3STRF.
             03 ENP3STRA    PICTURE X.
           02  ENP3STRI  PIC X(25).
           02  ENP3CITL    COMP  PIC  S9(4).
           02  ENP3CITF    PICTURE X.
           02  FILLER REDEFINES ENP3CITF.
             03 ENP3CITA    PICTURE X.
           02  ENP3CITI  PIC X(20).
           02  ENP3STAL    COMP  PIC  S9(4).
           02  ENP3STAF    PICTURE X.
           02  FILLER REDEFINES ENP3STAF.
             03 ENP3STAA    PICTURE X.
           02  ENP3STAI  PIC X(2).
           02  ENP3COUL    COMP  PIC  S9(4).
           02  ENP3COUF    PICTURE X.
           02  FILLER REDEFINES ENP3COUF.
             03 ENP3COUA    PICTURE X.
           02  ENP3COUI  PIC X(3).
           02  ENP3HPCL    COMP  PIC  S9(4).
           02  ENP3HPCF    PICTURE X.
           02  FILLER REDEFINES ENP3HPCF.
             03 ENP3HPCA    PICTURE X.
           02  ENP3HPCI  PIC X(8).
           02  ENP3PAYL    COMP  PIC  S9(4).
           02  ENP3PAYF    PICTURE X.
           02  FILLER REDEFINES ENP3PAYF.
             03 ENP3PAYA    PICTURE X.
           02  ENP3PAYI  PIC X(1).
           02  ENP3ACTL    COMP  PIC  S9(4).
           02  ENP3ACTF    PICTURE X.
           02  FILLER REDEFINES ENP3ACTF.
             03 ENP3ACTA    PICTURE X.
           02  ENP3ACTI  PIC X(12).
           02  ENP3ROUL    COMP  PIC  S9(4).
           02  ENP3ROUF    PICTURE X.
           02  FILLER REDEFINES ENP3ROUF.
             03 ENP3ROUA    PICTURE X.
           02  ENP3ROUI  PIC X(9).
           02  ENP3CCTL    COMP  PIC  S9(4).
           02  ENP3CCTF    PICTURE X.
           02  FILLER REDEFINES ENP3CCTF.
             03 ENP3CCTA    PICTURE X.
           02  ENP3CCTI  PIC X(8).
           02  ENP3CCNL    COMP  PIC  S9(4).
           02  ENP3CCNF    PICTURE X.
           02  FILLER REDEFINES ENP3CCNF.
             03 ENP3CCNA    PICTURE X.
           02  ENP3CCNI  PIC X(16).
           02  ENP3CCPL    COMP  PIC  S9(4).
           02  ENP3CCPF    PICTURE X.
           02  FILLER REDEFINES ENP3CCPF.
             03 ENP3CCPA    PICTURE X.
           02  ENP3CCPI  PIC X(4).
           02  ENP3CCVL    COMP  PIC  S9(4).
           02  ENP3CCVF    PICTURE X.
           02  FILLER REDEFINES ENP3CCVF.
             03 ENP3CCVA    PICTURE X.
           02  ENP3CCVI  PIC X(5).
           02  ERP3FLDL    COMP  PIC  S9(4).
           02  ERP3FLDF    PICTURE X.
           02  FILLER REDEFINES ERP3FLDF.
             03 ERP3FLDA    PICTURE X.
           02  ERP3FLDI  PIC X(40).
       01  SSMAPP3O REDEFINES SSMAPP3I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  ENP3CDTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP3OPTO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ENP3PNOO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP3DATO  PIC X(13).
           02  FILLER PICTURE X(3).
           02  ENP3CNOO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP3IDAO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP3EDAO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP3TYPO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  ENP3BEDO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  ENP3VALO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ENP3STRO  PIC X(25).
           02  FILLER PICTURE X(3).
           02  ENP3CITO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  ENP3STAO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  ENP3COUO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  ENP3HPCO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ENP3PAYO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ENP3ACTO  PIC X(12).
           02  FILLER PICTURE X(3).
           02  ENP3ROUO  PIC X(9).
           02  FILLER PICTURE X(3).
           02  ENP3CCTO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ENP3CCNO  PIC X(16).
           02  FILLER PICTURE X(3).
           02  ENP3CCPO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ENP3CCVO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  ERP3FLDO  PIC X(40).
       01  SSMAPP4I.
           02  FILLER PIC X(12).
           02  ENP4CDTL    COMP  PIC  S9(4).
           02  ENP4CDTF    PICTURE X.
           02  FILLER REDEFINES ENP4CDTF.
             03 ENP4CDTA    PICTURE X.
           02  ENP4CDTI  PIC X(10).
           02  ENP4OPTL    COMP  PIC  S9(4).
           02  ENP4OPTF    PICTURE X.
           02  FILLER REDEFINES ENP4OPTF.
             03 ENP4OPTA    PICTURE X.
           02  ENP4OPTI  PIC X(1).
           02  ENP4PNOL    COMP  PIC  S9(4).
           02  ENP4PNOF    PICTURE X.
           02  FILLER REDEFINES ENP4PNOF.
             03 ENP4PNOA    PICTURE X.
           02  ENP4PNOI  PIC X(10).
           02  ENP4DATL    COMP  PIC  S9(4).
           02  ENP4DATF    PICTURE X.
           02  FILLER REDEFINES ENP4DATF.
             03 ENP4DATA    PICTURE X.
           02  ENP4DATI  PIC X(13).
           02  ENP4CNOL    COMP  PIC  S9(4).
           02  ENP4CNOF    PICTURE X.
           02  FILLER REDEFINES ENP4CNOF.
             03 ENP4CNOA    PICTURE X.
           02  ENP4CNOI  PIC X(10).
           02  ENP4IDAL    COMP  PIC  S9(4).
           02  ENP4IDAF    PICTURE X.
           02  FILLER REDEFINES ENP4IDAF.
             03 ENP4IDAA    PICTURE X.
           02  ENP4IDAI  PIC X(10).
           02  ENP4EDAL    COMP  PIC  S9(4).
           02  ENP4EDAF    PICTURE X.
           02  FILLER REDEFINES ENP4EDAF.
             03 ENP4EDAA    PICTURE X.
           02  ENP4EDAI  PIC X(10).
           02  ENP4ADDL    COMP  PIC  S9(4).
           02  ENP4ADDF    PICTURE X.
           02  FILLER REDEFINES ENP4ADDF.
             03 ENP4ADDA    PICTURE X.
           02  ENP4ADDI  PIC X(25).
           02  ENP4CITL    COMP  PIC  S9(4).
           02  ENP4CITF    PICTURE X.
           02  FILLER REDEFINES ENP4CITF.
             03 ENP4CITA    PICTURE X.
           02  ENP4CITI  PIC X(20).
           02  ENP4STAL    COMP  PIC  S9(4).
           02  ENP4STAF    PICTURE X.
           02  FILLER REDEFINES ENP4STAF.
             03 ENP4STAA    PICTURE X.
           02  ENP4STAI  PIC X(2).
           02  ENP4COUL    COMP  PIC  S9(4).
           02  ENP4COUF    PICTURE X.
           02  FILLER REDEFINES ENP4COUF.
             03 ENP4COUA    PICTURE X.
           02  ENP4COUI  PIC X(3).
           02  ENP4HPCL    COMP  PIC  S9(4).
           02  ENP4HPCF    PICTURE X.
           02  FILLER REDEFINES ENP4HPCF.
             03 ENP4HPCA    PICTURE X.
           02  ENP4HPCI  PIC X(8).
           02  ENP4PTYL    COMP  PIC  S9(4).
           02  ENP4PTYF    PICTURE X.
           02  FILLER REDEFINES ENP4PTYF.
             03 ENP4PTYA    PICTURE X.
           02  ENP4PTYI  PIC X(25).
           02  ENP4FPEL    COMP  PIC  S9(4).
           02  ENP4FPEF    PICTURE X.
           02  FILLER REDEFINES ENP4FPEF.
             03 ENP4FPEA    PICTURE X.
           02  ENP4FPEI  PIC X(4).
           02  ENP4FPRL    COMP  PIC  S9(4).
           02  ENP4FPRF    PICTURE X.
           02  FILLER REDEFINES ENP4FPRF.
             03 ENP4FPRA    PICTURE X.
           02  ENP4FPRI  PIC X(8).
           02  ENP4CPEL    COMP  PIC  S9(4).
           02  ENP4CPEF    PICTURE X.
           02  FILLER REDEFINES ENP4CPEF.
             03 ENP4CPEA    PICTURE X.
           02  ENP4CPEI  PIC X(4).
           02  ENP4CPRL    COMP  PIC  S9(4).
           02  ENP4CPRF    PICTURE X.
           02  FILLER REDEFINES ENP4CPRF.
             03 ENP4CPRA    PICTURE X.
           02  ENP4CPRI  PIC X(8).
           02  ENP4XPEL    COMP  PIC  S9(4).
           02  ENP4XPEF    PICTURE X.
           02  FILLER REDEFINES ENP4XPEF.
             03 ENP4XPEA    PICTURE X.
           02  ENP4XPEI  PIC X(4).
           02  ENP4XPRL    COMP  PIC  S9(4).
           02  ENP4XPRF    PICTURE X.
           02  FILLER REDEFINES ENP4XPRF.
             03 ENP4XPRA    PICTURE X.
           02  ENP4XPRI  PIC X(8).
           02  ENP4WPEL    COMP  PIC  S9(4).
           02  ENP4WPEF    PICTURE X.
           02  FILLER REDEFINES ENP4WPEF.
             03 ENP4WPEA    PICTURE X.
           02  ENP4WPEI  PIC X(4).
           02  ENP4WPRL    COMP  PIC  S9(4).
           02  ENP4WPRF    PICTURE X.
           02  FILLER REDEFINES ENP4WPRF.
             03 ENP4WPRA    PICTURE X.
           02  ENP4WPRI  PIC X(8).
           02  ENP4PAYL    COMP  PIC  S9(4).
           02  ENP4PAYF    PICTURE X.
           02  FILLER REDEFINES ENP4PAYF.
             03 ENP4PAYA    PICTURE X.
           02  ENP4PAYI  PIC X(1).
           02  ENP4ACTL    COMP  PIC  S9(4).
           02  ENP4ACTF    PICTURE X.
           02  FILLER REDEFINES ENP4ACTF.
             03 ENP4ACTA    PICTURE X.
           02  ENP4ACTI  PIC X(12).
           02  ENP4ROUL    COMP  PIC  S9(4).
           02  ENP4ROUF    PICTURE X.
           02  FILLER REDEFINES ENP4ROUF.
             03 ENP4ROUA    PICTURE X.
           02  ENP4ROUI  PIC X(9).
           02  ENP4CCTL    COMP  PIC  S9(4).
           02  ENP4CCTF    PICTURE X.
           02  FILLER REDEFINES ENP4CCTF.
             03 ENP4CCTA    PICTURE X.
           02  ENP4CCTI  PIC X(8).
           02  ENP4CCNL    COMP  PIC  S9(4).
           02  ENP4CCNF    PICTURE X.
           02  FILLER REDEFINES ENP4CCNF.
             03 ENP4CCNA    PICTURE X.
           02  ENP4CCNI  PIC X(16).
           02  ENP4CCPL    COMP  PIC  S9(4).
           02  ENP4CCPF    PICTURE X.
           02  FILLER REDEFINES ENP4CCPF.
             03 ENP4CCPA    PICTURE X.
           02  ENP4CCPI  PIC X(4).
           02  ENP4CCVL    COMP  PIC  S9(4).
           02  ENP4CCVF    PICTURE X.
           02  FILLER REDEFINES ENP4CCVF.
             03 ENP4CCVA    PICTURE X.
           02  ENP4CCVI  PIC X(5).
           02  ERP4FLDL    COMP  PIC  S9(4).
           02  ERP4FLDF    PICTURE X.
           02  FILLER REDEFINES ERP4FLDF.
             03 ERP4FLDA    PICTURE X.
           02  ERP4FLDI  PIC X(40).
       01  SSMAPP4O REDEFINES SSMAPP4I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  ENP4CDTO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP4OPTO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ENP4PNOO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP4DATO  PIC X(13).
           02  FILLER PICTURE X(3).
           02  ENP4CNOO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP4IDAO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP4EDAO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  ENP4ADDO  PIC X(25).
           02  FILLER PICTURE X(3).
           02  ENP4CITO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  ENP4STAO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  ENP4COUO  PIC X(3).
           02  FILLER PICTURE X(3).
           02  ENP4HPCO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ENP4PTYO  PIC X(25).
           02  FILLER PICTURE X(3).
           02  ENP4FPEO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ENP4FPRO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ENP4CPEO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ENP4CPRO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ENP4XPEO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ENP4XPRO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ENP4WPEO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ENP4WPRO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ENP4PAYO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ENP4ACTO  PIC X(12).
           02  FILLER PICTURE X(3).
           02  ENP4ROUO  PIC X(9).
           02  FILLER PICTURE X(3).
           02  ENP4CCTO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ENP4CCNO  PIC X(16).
           02  FILLER PICTURE X(3).
           02  ENP4CCPO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ENP4CCVO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  ERP4FLDO  PIC X(40).
