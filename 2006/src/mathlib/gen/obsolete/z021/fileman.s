*
* $Id: fileman.s,v 1.1.1.1 1996/04/01 15:03:22 mclareni Exp $
*
* $Log: fileman.s,v $
* Revision 1.1.1.1  1996/04/01 15:03:22  mclareni
* Mathlib gen
*
*
          IDENT  FILEMAN
          SYSCOM
          IPARAMS
          SST
          TITLE  P.F. / FORTRAN FTN FUNCTIONS....
 ENTFUNC  MACRO  NAM
          SPACE   3
          ENTRY  NAM
 X_NAM    VFD    42/0L_NAM,18/NAM
 NAM      JP    *+400000B
          SA2    X_NAM
          BX6    X2
          SX7    A0
          SA6    NAME
          SA7    TEMPA0
          ENDM
* PF PARAMETER IN PFUNC : PP USED BY THE ACTION : NAM TO DO EFFECTIVE
*    THE REQUEST POSTED IN RA +1 (ONLY THIRD CHARACTER USED IN PF ;
*    SEE ARGPF LOCATION )
 PFUNC    MACRO  P1,NAM,COD,PF
 PP       MICRO  3,1,*_PF*
          ENTFUNC NAM
          RJ     PAKARG
          RJ     P1
          SX2    1R"PP"
          SX3    COD
          RJ     POST        REQUEST IN RA+1
          RJ     RAZTAB
          SA1    TEMPA0
          SA0    X1
          EQ     NAM
          ENDM
 ENTKEY   MACRO  KEY,CODKEY
 KY       MICRO  1,2,*_KEY*
          ENTRY  KEY
 KEY      DATA   0
          SX2    CODKEY
          SB6    "KY"AD
          SB7    KEY
          EQ     LOAD
          ENDM
          PFUNC    STORLFN,PURGEF,40B,PFP
          PFUNC  CLONR,CATLGF,20B,PFC
          PFUNC    STORLFN,ATTACHF,10B,PFA
          PFUNC  CLONR,EXTENDF,30B,PFE
          PFUNC  CLONR,ALTERF,34B,PFE
          PFUNC STORLFN,RENAMEF,50B,PFR
          ENTFUNC  RETURNF
          SA0  A1
          RJ     STORLFN
          SA1  A0
+RJ=XCLOSUN
-         VFD  30/NAME
          SA1    RETAD
          ZR     X1,RET2
          SX7    B0
          SA7    X1
          EQ     RET3
 RET2     SX1    ADMSRT
          RJ     MESG
 RET3     RJ     RAZTAB
          SA1  TEMPA0
          SA0  X1
          EQ     RETURNF
CLONR     BSSZ  1
          SA0  A1
          RJ  STORLFN
          SA1  A0
+RJ=XCLOSNR
-         VFD  30/NAME
          SA1  TEMPA0
          SA0  X1
          EQ  CLONR
          ENTFUNC  DEVICE
#if !defined(CERNLIB_F4)
          MX7    1
          BX1    X7+X1
#endif
          RJ     =XGETFIT.
          LT     B0,B0,NAME
          SA2    X1
          MX0    42
          BX6    X0*X2
          SX7    A2
          SA6    REQPR
          SA7    LFNAD
          SPACE  1
*---- LA ROUTINE XXREQF SE TROUVE DANS LE DECK REQQ
          RJ     =XXXREQF
          SPACE  1
          SX6    B0
          SB1    REQPR
          SA6    SNAD              REMISE A ZERO DE SN DANS FDB
          SB2    REQPR+8
RAZREQ    SA6    B1                REMISE A ZERO DE LA TABLE DE REQUEST
          SB1    B1+1
          GE,B2,B1 RAZREQ
          SA2    RETCOD             RETURN CODE
          BX6    X2
          SA1    RETAD
          ZR     X1,DEVC2
          SA6    X1
          EQ     DEVICE
DEVC2     ZR,X6  DEVICE
          MESSAGE  REQABT,,RECALL
          ABORT
 REQABT   DIS    ,*INVALID REQ.PARAMS(DEVICE)*
          ENTRY  DEVTYP
 DEVTYP   DATA   0
          SA4    STAR
          SA1    X1
          LX1    48
          SX3    X1-1R*
          ZR     X3,DEV1
          SX4    B0
 DEV1     MX0    12
          BX2    X1*X0
          SB2    DEVTAB
 NXTYP    SA1    B2
          ZR     X1,DEVTYP
          BX5    X1*X0
          IX3    X2-X5
          ZR     X3,DEV2
          SB2    B2+1
          EQ     NXTYP
 DEV2     BX4    X1+X4
          BX6    -X0*X4
          SA5    REQPR+1
          BX6    X6+X5
          SA6    A5
          EQ     DEVTYP
DEVTAB    VFD    12/2CPF,24/600B   OV BIT + PF BIT
          VFD    6/4,18/0          RC BIT
          VFD    12/2CA*,48/0
          VFD    60/0
 STAR     VFD    12/0,24/20B,24/0
          ENTRY  SN
SN        EQ     *+1S17
          SA4    REQPR+1
          MX6    1                 SET SN BIT :
          LX6    18                  - BIT 17 (FIRST BIT=0) OF REQPR+1
          BX6    X6+X4
          SA6    A4
          SA1    X1
          MX0    42
          BX6    X1*X0
          SX5    40B
          BX7    X6+X5
          SA6    REQPR+3     SETNAME LEFT JUSTIFIED IN REQPR+3
          SA7    SNAD        SETNAME IN SNAD (FDB)
          EQ     SN
          ENTRY  PW
 PW       DATA   0
          SB1    B0
          SB2    4
          SX2    20B
          MX0    54
 TURN     SA3    A1+B1
          ZR     X3,PW              END OF ARGUMENT LIST
          SA3    X3                 X3=PW ARGUMT.
          ZR     X3,TURN
          SX4    X2+B1
          LX3    6
          BX6    X0*X3              CLEAR ARGUMT. LOWER
          IX7    X6+X4              SET   PW CODE
          SA7    B1+PWAD            STORE
          SB1    B1+1
          LE    B1,B2,TURN
          EQ     PW
          ENTRY  RC
 RC       DATA   0
          SX7    X1
          SA7    RETAD
          EQ     RC
*
*    TRAITEMENT DE  REAL TIME REQUEST  RT(IRET)
*
          ENTRY  RT
 RT       DATA   0
          SX6    1
          SA6    RTMAD
          RJ     RC
          EQ     RT
          ENTKEY   RP,02B
          ENTKEY   CY,03B
          ENTKEY   TK,04B
          ENTKEY   CN,05B
          ENTKEY   MD,06B
          ENTKEY   EX,07B
          ENTKEY   RD,10B
          ENTKEY   MR,11B
          ENTKEY  XR,13B
          ENTKEY   ID,14B
          ENTKEY LC,31B
          ENTKEY ST,32B
          ENTKEY RW,33B
          ENTKEY RB,43B
          ENTKEY UV,53B
 LOAD     BSS    0
          SA1    X1
          MX0    54
          LX1    6
          BX6    X1*X0
          BX6    X6+X2
          SA6    B6
 LDEXIT   JP     B7
 STORLFN  DATA   0
#if !defined(CERNLIB_F4)
          MX7    1
          BX1    X7+X1
#endif
 +        RJ     =XGETFIT.
 -        LT     B0,B0,NAME
          SA2    X1
          MX0    42
          BX6    X0*X2
          SX7    A2
          SA6    LFNFDB
          SA7    LFNAD
          EQ     STORLFN
 ARGPF    VFD    18/2LPF,2/1,40/0
 POST     DATA   0
          SA1    ARGPF
          LX2    42
          BX2    X2+X1
          MX0    52
          SA1    LFNFDB
          BX6    X0*X1
          SX5    A1
          SA4    RTMAD
          ZR     X4,POST1
          SX1    200B
          EQ     POST2
 POST1    SA4    RETAD
          NZ     X4,WRC
          SX1    100B
 POST2    IX3    X3+X1
 WRC      IX7    X3+X6
          BX6    X5+X2
          SA7    A1
          RJ     =XSYS=
          SA2    RETAD
          ZR     X2,POST
          SA3    LFNFDB
          MX0    51
          AX3    9
          BX6    -X0*X3
          SA6    X2
          EQ     POST
 RAZTAB   DATA   0
          SX7    B0
          SA7    REQPR+3           CLEAR SETNAME IN REQUEST TABLE
          SA7    A7-2              CLEAR SN BIT IN REQPR+1
          SB1    PFNAD
          SB2    ADZERO
          SA7  RCY
RAZ       SA1  B1
          MX0  54
          BX6  -X0*X1
          SB3  X6-03
          NE   B3,B0,NOCYW
          BX6  X1
          SA6  RCY
NOCYW     SA7  B1
          SB1    B1+1
          LE     B1,B2,RAZ
          SA7    RTMAD
          SA7    A7+1
          EQ     RAZTAB
 PAKARG   DATA   0
          SB6  1
          SA2  A1+B6
          SB5  B0
          MX7  0
          ZR   X2,PACK
          SB2  X2
          SB5    B0
          SB4    3
          SX5    77B
 STORPFN  SA2    B2+B5
          SB3    B0
          SX7    B0
          SX0    77B
 NXTCHAR  LX0    54
          SB3    B3+6
          BX6    X0*X2
          ZR     X6,PACK
          BX7    X7+X6
          IX4    X0-X5
          NZ     X4,NXTCHAR
 NXTWRD   SA7    B5+PFNAD
          SB5    B5+B6
          LE     B5,B4,STORPFN
          EQ     PACK2
 PACK     SA7    B5+PFNAD
 PACK2    SB4    ADZERO
          MX0    54
          SB3    NULAD
          SB2    PPAD
          SX7    B0
 BCLPAC   SA4    B2
          ZR     X4,SUIT
          BX6    X4
          SA6    B3
          SA7    A4
          SB3    B3+B6
 SUIT     SB2    B2+B6
          LT     B2,B4,BCLPAC
          EQ     PAKARG
 MESG     DATA   0
          SA2    LFNAD
          MX0    42
          SA2    X2                X2 = LFN
          BX6    X0*X2             CLEAR LFN LOWER 18 BITS
          SA6    X1+2              STORE LFN IN MESSAGE
          MX6    1
          LX6    23D
 +        RJ     =XMSG=
 -        LT     B0,B0,NAME
          EQ     MESG
 ADMSRT   DIS    2,FILE RETURNED,LFN =
 FILERT   DIS    1,
          USE    /REQPR/
REQPR     BSSZ   9           REQUEST TABLE
RETCOD    BSSZ   1
          USE    *
 PFNAD    BSSZ   4
 LFNFDB   BSSZ   1           FDB
 NULAD    BSSZ   1
 PPAD     BSSZ   1
 RPAD     BSSZ   1
 CYAD     BSSZ   1
 TKAD     BSSZ   1
 CNAD     BSSZ   1
 MDAD     BSSZ   1
 EXAD     BSSZ   1
 SDAD     BSSZ   1
 IDAD     BSSZ   1
 RDAD     BSSZ   1
 MRAD     DATA   0
 RNAD     DATA   0
XRAD      BSSZ 1
LCAD      BSSZ 1
STAD      BSSZ 1
RWAD      BSSZ 1
 PWAD     BSSZ    5
RBAD      BSSZ 1
UVAD      BSSZ 1
SNAD      BSSZ 1
 ADZERO   DATA   0
 RTMAD    BSSZ   1
 RETAD    DATA   0
 LFNAD    DATA   0
 TEMPA0   BSS    1
 NAME     DATA   0
          USE /FILEM./
RCY       BSSZ  1
          USE  *
          END
