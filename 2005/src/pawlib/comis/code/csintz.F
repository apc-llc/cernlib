*
* $Id: csintz.F,v 1.1.1.1 1996/02/26 17:16:23 mclareni Exp $
*
* $Log: csintz.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:23  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_IBM)
*CMZ :  1.18/00 17/02/94  16.31.56  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSINTZ(IPARCE,IJMPT)
***----------------------------------------
*    it is needed for ibm only due to possible recurtion
***----------------------------------------
      INTEGER CSLEQS,CSLNES,CSLLTS,CSLLES,CSLGES,CSLGTS
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/csbuf.inc"
#include "comis/csrec.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/cssysd.inc"
#include "comis/csdpvs.inc"
       REAL Q(99)
       EQUIVALENCE (IQ,Q)
#include "comis/cslun.inc"
#include "comis/csfres.inc"
#include "comis/cskeys.inc"
#include "comis/csopen.inc"
#include "comis/csfmt.inc"
#include "comis/csichv.inc"
#if defined(CERNLIB_PAW)
#include "comis/cskucs.inc"
#endif
         INTEGER  CSIOED,CSKCAL,CSKRDW,CSKIOV,CSKIOA
      EQUIVALENCE (IDOE1,RDOE1)
      INTEGER IEQD(6),IEQD1(4),IEQD2(2),IDPRS(2)
      EQUIVALENCE (IEQD,D),(IEQD1,D1),(IEQD2,D2),(IDPRS,DCSRES)
      CHARACTER *256 CHIOV, CHIBUF
      SAVE CHIOV, CHIBUF
      PARAMETER ( KLCMLX=2 )
      MIWORD(J)=IA(J-JTOPA)
      JCHIBF=MJSCHA(CHIBUF)
      JFMT=MJSCHA(FMT)
      JCHIOV=MJSCHA(CHIOV)
      LCHIOV=LEN(CHIOV)
      JDOE1=LOCF(IDOE1)
****      JTOPA=LOCF(IA(1))-1
      GO TO 999
 400  JTCH=IA(ITA-2)
      NCHS=NCHS+IA(ITA-3)+IA(ITA-1)
      ITA=ITA-3
      IA(ITA)=I
 998  IPC=IPC+1
*
 999  CONTINUE
       GO TO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
     1 16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
     2 33,34,35,36,37,38,39,40,41,42,43,44,45,
     3 46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
     4 61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,
     5 78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,
     6 94,95,96,97,98,99,100,101,102,103,104,105,106,107,
     7 108,109,110,111,112,113,114,115
     A ,116,117,118,119,120,121,122,123,124,125,126,127,
     B  128,129,130,131,132,133,134,135,136,137,138,139,
     C  140,141,142,143,144,145,146,147,148,149,150,151,
     D  152,153,154,155,156,157,158,159,160,161,162
     1 ,163,164,165,166,167,168,169,170,171,172,173,
     2  174,175,176,177,178,179,180,181,182,183,184,185,186,187,
     3  188,189,190,191,192,193,194,195,196,197,198,199,200,201

     8                    ),KD(IPC)
      GO TO 700
*     ---------
*    I.OR.I
 1    I=0
      ITA=ITA-1
      IF(IA(ITA).NE.0.OR.IA(ITA+1).NE.0)I=1
      IA(ITA)=I
      GO TO 998
*    R.OR.R
 2    I=0
      ITA=ITA-1
      IF(RA(ITA).NE.0. .OR. RA(ITA+1).NE.0.)I=1
      IA(ITA)=I
      GO TO 998
*    I.AND.I
  3   I=0
      ITA=ITA-1
      IF(IA(ITA).NE.0.AND.IA(ITA+1).NE.0)I=1
      IA(ITA)=I
      GO TO 998
*    R.AND.R
 4    I=0
      ITA=ITA-1
      IF(RA(ITA).NE.0. .AND. RA(ITA+1).NE.0.)I=1
      IA(ITA)=I
      GO TO 998
* .NOT.I
  5   I=0
      IF(IA(ITA).EQ.0)I=1
      IA(ITA)=I
      GO TO 998
* .NOT.R
  6   I=0
      IF(RA(ITA).EQ.0.)I=1
      IA(ITA)=I
      GO TO 998
* I.EQ.I
 7    I=0
      ITA=ITA-1
      IF(IA(ITA).EQ.IA(ITA+1))I=1
      IA(ITA)=I
      GO TO 998
*R.EQ.R
 8    I=0
      ITA=ITA-1
      IF(RA(ITA).EQ.RA(ITA+1))I=1
      IA(ITA)=I
      GO TO 998
*CH.EQ.CH
 9    I=CSLEQS(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1))
      GO TO 400
*I.NE.I
  10  I=0
      ITA=ITA-1
      IF(IA(ITA).NE.IA(ITA+1))I=1
      IA(ITA)=I
      GO TO 998
*R.NE.R
  11  I=0
      ITA=ITA-1
      IF(RA(ITA).NE.RA(ITA+1))I=1
      IA(ITA)=I
      GO TO 998
*CH.NE.CH
 12   I=CSLNES(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1))
      GO TO 400
*I.LT.I
  13  I=0
      ITA=ITA-1
      IF(IA(ITA).LT.IA(ITA+1))I=1
      IA(ITA)=I
      GO TO 998
*R.LT.R
  14  I=0
      ITA=ITA-1
      IF(RA(ITA).LT.RA(ITA+1))I=1
      IA(ITA)=I
      GO TO 998
*CH.LT.CH
  15  I=CSLLTS(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1))
      GO TO 400
*I.LE.I
  16  I=0
      ITA=ITA-1
      IF (IA(ITA).LE.IA(ITA+1))I=1
      IA(ITA)=I
      GO TO 998
*R.LE.R
  17  I=0
      ITA=ITA-1
      IF(RA(ITA).LE.RA(ITA+1))I=1
      IA(ITA)=I
      GO TO 998
*CH.LE.CH
  18  I=CSLLES(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1))
      GO TO 400
*I.GE.I
  19  I=0
      ITA=ITA-1
      IF(IA(ITA).GE.IA(ITA+1))I=1
      IA(ITA)=I
      GO TO 998
*R.GE.R
  20  I=0
      ITA=ITA-1
      IF(RA(ITA).GE.RA(ITA+1))I=1
      IA(ITA)=I
      GO TO 998
*CH.GE.CH
  21  I=CSLGES(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1))
      GO TO 400
*I.GT.I
  22  I=0
      ITA=ITA-1
      IF(IA(ITA).GT.IA(ITA+1))I=1
      IA(ITA)=I
      GO TO 998
*R.GT.R
  23  I=0
      ITA=ITA-1
      IF(RA(ITA).GT.RA(ITA+1))I=1
      IA(ITA)=I
      GO TO 998
*CH.GT.CH
  24  I=CSLGTS(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1))
      GO TO 400
*-I
  25  IA(ITA)=-IA(ITA)
      GO TO 998
*-R
  26  RA(ITA)=-RA(ITA)
      GO TO 998
*REAL(I)
  27  RA(ITA)=IA(ITA)
      GO TO 998
*INT(R)
  28  IA(ITA)=RA(ITA)
      GO TO 998
* REAL2(I)
  29  RA(ITA-1)=IA(ITA-1)
      GO TO 998
* INT2(R)
  30  IA(ITA-1)=RA(ITA-1)
      GO TO 998
*I+I
  31  ITA=ITA-1
      IA(ITA)=IA(ITA)+IA(ITA+1)
      GO TO 998
*R+R
  32  ITA=ITA-1
      RA(ITA)=RA(ITA)+RA(ITA+1)
      GO TO 998
*I-I
  33  ITA=ITA-1
      IA(ITA)=IA(ITA)-IA(ITA+1)
      GO TO 998
*R-R
  34  ITA=ITA-1
      RA(ITA)=RA(ITA)-RA(ITA+1)
      GO TO 998
*I*I
  35  ITA=ITA-1
      IA(ITA)=IA(ITA)*IA(ITA+1)
      GO TO 998
*R*R
  36  ITA=ITA-1
      RA(ITA)=RA(ITA)*RA(ITA+1)
      GO TO 998
*I/I
  37  ITA=ITA-1
      IA(ITA)=IA(ITA)/IA(ITA+1)
      GO TO 998
*R/R
  38  ITA=ITA-1
      RA(ITA)=RA(ITA)/RA(ITA+1)
      GO TO 998
*I**I
  39  ITA=ITA-1
      IA(ITA)=IA(ITA)**IA(ITA+1)
      GO TO 998
*R**I
  40  ITA=ITA-1
      RA(ITA)=RA(ITA)**IA(ITA+1)
      GO TO 998
*R**R
  41  ITA=ITA-1
      RA(ITA)=RA(ITA)**RA(ITA+1)
      GO TO 998
*A=A
  42  J=IA(ITA-1)
      IA(J-JTOPA)=IA(ITA)
      ITA=ITA-2
      GO TO 998
*CH=CH
  43  N1=IA(ITA-3)
      N2=IA(ITA-1)
      N=MIN0(N1,N2)
      CALL CCOPYS(IA(ITA),IA(ITA-2),N)
      IF(N1.GT.N)THEN
              J=IA(ITA-2)+N
              N=N1-N
              CALL CSSETC(J,N,ICHBLN)
      ENDIF
      JTCH=IA(ITA)
      NCHS=NCHS+N2
      ITA=ITA-4
      GO TO 998
*CH//CH
  44  IA(ITA-3)=IA(ITA-3) + IA(ITA-1)
      ITA= ITA-2
      GO TO 998
*LK K
  45  ITA=ITA+1
      IA(ITA)=KD1(IPC)
 4501 IPC=IPC+2
 4502 IF(ITA.GT.LAST)GO TO 701
*                    ---------
      GO TO 999
* LAK K
  46  ITA=ITA+1
      IA(ITA)=LOCF(KD1(IPC))
      GO TO 4501
*LCK N,'TEXT'
  47  N=KD1(IPC)
      J=MJCHAR(KD2(IPC))
      ITA=ITA+2
      IA(ITA-1)=N
      IA(ITA)=JTCH
      NCHS=NCHS-N
      IF(NCHS.LT.0)GO TO 702
*                 -----------
      CALL CCOPYS(J,JTCH,N)
      JTCH=JTCH+N
 4701 I=(N-1)/NBYTPW+3
      IPC=IPC+I
      GO TO 4502
*LACK N,'TEXT'
  48  N=KD1(IPC)
      ITA=ITA+2
      IA(ITA-1)=N
      IA(ITA)=MJCHAR(KD2(IPC))
      GO TO 4701
*DCV I
  49  ID=IDP+KD1(IPC)
      IDA(ID)=IA(ITA-1)+KON1
      IDA(ID+1)=IA(ITA)
      IA(ITA-1)=LOCF(IDA(ID))
      ITA=ITA-1
      IPC=IPC+2
      GO TO 999
*LVL  I
  50  I=IDP+KD1(IPC)
      ITA=ITA+1
      IA(ITA)=IDA(I)
      GO TO 4501
*LAL  I
  51  ITA=ITA+1
      IA(ITA)=LOCF(IDA(IDP+KD1(IPC)))
      GO TO 4501
*LVG N,I
  52  J=IQ(KD1(IPC))+KD2(IPC)
      IPC=IPC+1
 5201 ITA=ITA+1
      IA(ITA)=IA(J-JTOPA)
      GO TO 4501
*LAG N,I
  53  ITA=ITA+1
      IA(ITA)=IQ(KD1(IPC))+KD2(IPC)
      IPC=IPC+3
      GO TO 4502
*LVP I
  54  J=IA(IAP+KD1(IPC))
      GO TO 5201
*LAP I
  55  ITA=ITA+1
      IA(ITA)=IA(IAP+KD1(IPC))
      GO TO 4501
*LVKA IDARR,OFFSET
  56  ID=KD1(IPC)
      LENEGI=MOD(IQ(ID),KON3)
      J=IQ(ID+1)+KD2(IPC)
      ITA=ITA+1
      IA(ITA)=IA(J-JTOPA)
      IF(LENEGI.EQ.2)THEN
        ITA=ITA+1
        IA(ITA)=IA(J-JTOPA+1)
      ENDIF
      IPC=IPC+3
      GO TO 999
*LAKA IDARR,OFFSET
  57  ID=KD1(IPC)
      ITA=ITA+1
      IA(ITA)=IQ(ID+1)+KD2(IPC)
      IPC=IPC+3
      GO TO 999
*LVCL N,I
  58  IVAL=1
 5801 J=MJCHAR(IDA(IDP+KD2(IPC)))
 5802 NB=KD1(IPC)
 5803 N2=IA(ITA)
      N1=IA(ITA-1)
 5804 N=N2-N1+1
      IF(N.LT.1.OR.N1.LT.1.OR.N2.GT.NB)GO TO 703
*                                      ---------
      J=J+N1-1
      IA(ITA-1)=N
      IF(IVAL.EQ.1)THEN
        IA(ITA)=JTCH
        NCHS=NCHS-N
        IF(NCHS.LT.0)GO TO 702
*                   ----------
        CALL CCOPYS(J,JTCH,N)
        JTCH=JTCH+N
      ELSE
         IA(ITA)=J
      ENDIF
      IPC=IPC+3
      GO TO 999
*LACL NB,I
  59  IVAL=0
      GO TO 5801
*LVCG N,NB,I
  60  IVAL=1
 6001 IPC=IPC+1
      J=(IQ(KD(IPC))+KD2(IPC))*NBYTPW
      GO TO 5802
*LACG N,NB,I
  61  IVAL=0
      GO TO 6001
*LVCP X,I
  62  IVAL=1
 6201 JD=IA(IAP+KD2(IPC))
      J=MIWORD(JD+1)
      NB=MOD(MIWORD(JD),KON3)
      IF(IA(ITA).EQ.0)IA(ITA)=NB
      GO TO 5803
*LACP X,I
  63  IVAL=0
      GO TO 6201
*IRPW   I**R
  64  ITA=ITA-1
      RA(ITA)=IA(ITA)**RA(ITA+1)
      GO TO 998
*IDPW I**D
  65  I=ITA-KDLEN
      ITA=I+1
***      CALL CCOPYA(IA(ITA),D,KDLEN)
      IEQD(1)=IA(ITA)
      IEQD(2)=IA(ITA+1)
      D1=IA(I)**D
***      CALL CCOPYA(D1,IA(I),KDLEN)
      IA(I)=IEQD1(1)
      IA(I+1)=IEQD1(2)
      GO TO 998
*RDPW R**D
  66  I=ITA-KDLEN
      ITA=I+1
***      CALL CCOPYA(IA(ITA),D,KDLEN)
      IEQD(1)=IA(ITA)
      IEQD(2)=IA(ITA+1)
      D1=RA(I)**D
***      CALL CCOPYA(D1,IA(I),KDLEN)
      IA(I)=IEQD1(1)
      IA(I+1)=IEQD1(2)
      GO TO 998
*IFUN1 NFUN,ITPAR
  67  NPAR=1
 6701 NFUN=KD1(IPC)
      ITPAR=KD2(IPC)
      IPC=IPC+3
      CALL CSINFN(NFUN,ITPAR,NPAR)
      IF(ITA.GT.LAST)GO TO 701
      IF(NFUN.LT.0)GO TO 712
      GO TO 999
*IFUN2 NFUN,ITPAR
  68  NPAR=2
      GO TO 6701
*IFUNN NPAR,NFUN,ITPAR
  69  NPAR=KD1(IPC)
      IPC=IPC+1
      GO TO 6701
*DFA NPAR,IDARR
  70  J=IA(IAP+KD1(IPC))
      I=KD2(IPC)
      IQ(I+1)=J
      IPC=IPC+3
      GO TO 999
*DFAS NPAR,IDARR
  71  I=KD2(IPC)
      IQ(I+1)=IA(IAP+KD1(IPC))
      IPC=IPC+3
 7100 NDIM=IQ(I+3)
      N=NDIM+NDIM
      ICON=0
      ILEN=1
      I1=ITA-N+1
      I2=I+4
      DO 7101 K=1,NDIM
        L=IA(I1)
        M=IA(I1+1)-L+1
        IF(M.LE.0)GO TO 706
*                    ------
        ICON=ICON+ILEN*L
        ILEN=ILEN*M
        IQ(I2)=ILEN
        I1=I1+2
 7101 I2=I2+1
      IQ(I+2)=ICON
      ITA=ITA-N
      GO TO 999
*DFCA NPAR,IDARR,LENEL
  72  I=KD2(IPC)
      J=IA(IAP+KD1(IPC))
      IQ(I+1)=MIWORD(J+1)
      LENEL=KD3(IPC)
      IF(LENEL.EQ.0)THEN
        IQ(I)=MIWORD(J)
      ELSE
        IQ(I)=LENEL+KON2
      ENDIF
      IPC=IPC+4
      GO TO 999
*DFCAS NPAR,IDARR,LENEL
 73   I=KD2(IPC)
      J=IA(IAP+KD1(IPC))
      IQ(I+1)=MIWORD(J+1)
      LENEL=KD3(IPC)
      IF(LENEL.EQ.0)THEN
         IQ(I)=MIWORD(J)
      ELSE
         IQ(I)=LENEL+KON2
      ENDIF
      IPC=IPC+4
      GOTO 7100
*LEA IDARR,NIND
  74  IVAL=1
      LENEGI=1
 7401 ID=KD1(IPC)
 7402 ASSIGN 7406 TO LAB1
 7403 N=KD2(IPC)
      IF(N.EQ.1)THEN
         I=IA(ITA)
         ITA=ITA-1
         J1=ID+4
      ELSE
        ITA=ITA-N
        K=ITA+1
        I=IA(K)
        J1=ID+4
 7404 N=N-1
        IF(N.LE.0)GO TO 7405
        K=K+1
        I=I+IQ(J1)*IA(K)
        J1=J1+1
        GO TO 7404
      ENDIF
 7405 IVP=I-IQ(ID+2)
      IF(NCBARR.EQ.0)THEN
        IF(IVP.LT.0.OR.IVP.GE.IQ(J1))GO TO 710
*                                      -------
      ENDIF
      GO TO LAB1
 7406 J=IQ(ID+1)+IVP*LENEGI
      IF(IVAL.EQ.0)THEN
         ITA=ITA+1
         IA(ITA)=J
      ELSE
***         CALL COPYAJ(J,JTOPA+ITA+1,LENEGI)
        K=J-JTOPA-1
        DO 7407 I=1,LENEGI
        IA(ITA+I)=IA(K+I)
 7407   CONTINUE
        ITA=ITA+LENEGI
      ENDIF
      IPC=IPC+3
      GO TO 4502
*LAA IDARR,NIND
  75  IVAL=0
      LENEGI=1
      GO TO 7401
*LHK NW,TEXT
  76  N=KD1(IPC)
      ITA=ITA+1
      IA(ITA)=KD2(IPC)
      IPC=IPC+N+2
      GO TO 999
*LAHK NW,TEXT
  77  N=KD1(IPC)
      ITA=ITA+1
      IA(ITA)=LOCF(KD2(IPC))
      IPC=IPC+N+2
      GO TO 999
*BBLK IAP,IBASE,IDP
  78  IAP=KD1(IPC)
      IBASE=KD2(IPC)
      IDP=KD3(IPC)
      IPC=IPC+4
      GO TO 999
*EBLK
  79  IF(IJMPT.EQ.1)GO TO 9301
      IJMPT=1
      GO TO 777
*LECA IDARR,NIND
  80  IVAL=1
 8001 ID=KD1(IPC)
      N2=IA(ITA)
      N1=IA(ITA-1)
      ITA=ITA-2
      NB=MOD(IQ(ID),KON3)
      IF(N2.EQ.0)N2=NB
      ASSIGN 8003 TO LAB1
      GO TO 7403
 8003 J=IQ(ID+1)+IVP*NB
      ITA=ITA+2
* GO TO LVCL
      GO TO 5804
*LACA IDARR,NIND
  81  IVAL=0
      GO TO 8001
* 82 OPEN IFIL,ISTAT,IACCESS,IFORM,IRECL,IOSTAT,LABERR
*          1    2       3      4     5     6      7
  82  IF(KD(IPC+2).EQ.0)STATOP='UNKNOWN'
      IF(KD(IPC+3).EQ.0)ACCEOP='SEQUENTIAL'
      IF(KD(IPC+4).EQ.0)THEN
        IF(ACCEOP.EQ.'SEQUENTIAL')THEN
          FORMOP='FORMATTED'
        ELSE
          FORMOP='UNFORMATTED'
        ENDIF
      ENDIF
      IF(KD(IPC+6).EQ.0)JIOSTA=0
      LABERR=KD(IPC+7)
      I=KD(IPC+1)
      I1=KD(IPC+5)
      IPC=IPC+8
      IF(I.EQ.1) CALL CSFLCH(FILEOP)
      IF(I.EQ.0 .AND. I1.EQ.0)THEN
        OPEN(LUNOP,STATUS=STATOP,ACCESS=ACCEOP,FORM=FORMOP,
     +       IOSTAT=IOSTA , ERR=8201)
      ELSEIF(I.EQ.0 .AND. I1.EQ.1)THEN
        OPEN(LUNOP,STATUS=STATOP,ACCESS=ACCEOP,FORM=FORMOP,
     +       RECL=LRECOP,
     +       IOSTAT=IOSTA , ERR=8201)
      ELSEIF(I.EQ.1 .AND. I1.EQ.0)THEN
        OPEN(LUNOP,STATUS=STATOP,ACCESS=ACCEOP,FORM=FORMOP,
     +       FILE=FILEOP,
     +       IOSTAT=IOSTA , ERR=8201)
      ELSEIF(I.EQ.1 .AND. I1.EQ.1)THEN
        OPEN(LUNOP,STATUS=STATOP,ACCESS=ACCEOP,FORM=FORMOP,
     +       FILE=FILEOP,
     +       RECL=LRECOP,
     +       IOSTAT=IOSTA , ERR=8201)
      ENDIF
      IF(JIOSTA.NE.0)IA(JIOSTA-JTOPA)=IOSTA
      JIOSTA=0
      GO TO 999
 8201 CONTINUE
      IF(JIOSTA.NE.0)IA(JIOSTA-JTOPA)=IOSTA
      JIOSTA=0
      IF(LABERR.EQ.0)GO TO 711
      IPC=IBASE+LABERR
      GO TO 999
* 83 STO J
  83  IA(KD1(IPC)-JTOPA)=IA(ITA)
      ITA=ITA-1
      IPC=IPC+2
      GO TO 999
*STOC N,J
  84  N1=KD1(IPC)
      N2=IA(ITA-1)
      N=MIN0(N1,N2)
      CALL CCOPYS(IA(ITA),KD2(IPC),N)
      IF(N1.GT.N)THEN
        J=KD2(IPC)+N
        N=N1-N
        CALL CSSETC(J,N,ICHBLN)
      ENDIF
      JTCH=IA(ITA)
      NCHS=NCHS+N2
      ITA=ITA-2
      IPC=IPC+3
      GO TO 999
*SETPC IPC
  85  IPC=KD1(IPC)
      GO TO 999
*AA IDARR
  86  ID=KD1(IPC)
      ITA=ITA+1
 8601 IA(ITA)=IQ(ID+1)
      GO TO 4501
*CALLS IP,NPAR,ITB,...
  87  KS=1
      GO TO 9201
*IOEND
  88  CONTINUE
      I=CSIOED(N)
      IF(JIOSTA.NE.0)IA(JIOSTA-JTOPA)=IFMTST
      IF(I.EQ.0)THEN
        IF(IFMTST.LT.0 . AND. LIOEND.NE.-1)THEN
          IPC=LIOEND
        ELSEIF(LIOERR.NE.-1)THEN
          IPC=LIOERR
        ELSE
          GO TO 704
        ENDIF
      ELSE
        IPC=IPC+1
        IF(KINTWR.EQ.1)THEN
          CALL CCOPYS(JCHIBF,JINTBF,LENIBF)
        ENDIF
      ENDIF
      GO TO 999
*ACA IDARR
  89  ID=KD1(IPC)
      ITA=ITA+1
      IA(ITA)=LOCF(IQ(ID))
      GO TO 4501
*ASSGO I,IPCL
  90  I=IDP+KD1(IPC)
      IDA(I)=KD2(IPC)+IBASE
      IPC=IPC+3
      GO TO 999
*GOI I
 91   I=IDP+KD1(IPC)
      IPC=IDA(I)
      GO TO 999
*CALL  IP,NPAR[,IHK,IKK,ITB]*NPAR
  92  KS=3
 9201 I=CSKCAL(KS)
      IF(I.LT.0)THEN
         CSINTZ=I
         GO TO 7771
      ENDIF
      GO TO 999
*RET
***  93  CALL CCOPYA(IDA(IDP),DCSRES,KDLEN)
  93  IDPRS(1)=IDA(IDP)
      IDPRS(2)=IDA(IDP+1)
 9301 CONTINUE
***      CALL CCOPYA(IA(IFP+6),NAMEST(1),3)
      NAMEST(1)=IA(IFP+6)
      NAMEST(2)=IA(IFP+7)
      NUMST=IA(IFP+8)
***
***      CALL CCOPYA(IA(IFP),IFP,6)
      IAP=IA(IFP+1)
      IBASE=IA(IFP+2)
      IDP=IA(IFP+3)
      IPC=IA(IFP+4)
      ITA=IA(IFP+5)
      IFP=IA(IFP)
***
      NPAR=KD2(IPC)
      ITA=ITA-NPAR
      IF(KD(IPC).EQ.87)THEN
        IPC=IPC+NPAR+3
      ELSE
        IPC=IPC+NPAR*3+3
      ENDIF
      GO TO 999
*GO IPL
  94  IPC=KD1(IPC)+IBASE
      GO TO 999
*GOC N,L1,...,LN
  95  I=IA(ITA)
      N=KD1(IPC)
      IF(I.LE.0.OR.I.GT.N)THEN
        IPC=IPC+N+2
      ELSE
        IPC=KD1(IPC+I)+IBASE
      ENDIF
      ITA=ITA-1
      GO TO 999
*BRZI IPC
  96  IF(IA(ITA).EQ.0)THEN
        IPC=KD1(IPC)+IBASE
      ELSE
        IPC=IPC+2
      ENDIF
      ITA=ITA-1
      GO TO 999
*BRZR IPC
  97  IF(RA(ITA).EQ.0.)THEN
        IPC=KD1(IPC)+IBASE
      ELSE
        IPC=IPC+2
      ENDIF
      ITA=ITA-1
      GO TO 999
*ENTRY NCODES,NDATA
 98   IDP=IBASE+KD1(IPC)
      IPC=IPC+3
      GO TO 999
*DOI N,S,JDOV,K,IW
  99  IDOE1=IA(ITA-2)
      I2=IA(ITA-1)
      I3=IA(ITA)
      K=MAX0(0,(I2-IDOE1+I3)/I3)
      IA(ITA-2)=K
      KD(IPC+4)=K
      KD(IPC+5)=I3
 9901 ITA=ITA-2
 9902 N=KD1(IPC)
      I=KD2(IPC)
      IF(N.EQ.0)THEN
        J=LOCF(IDA(IDP+I))
      ELSEIF(N.GT.0)THEN
        J=IQ(N)+I
      ELSE
        J=IA(IAP+I)
      ENDIF
 9903 IA(J-JTOPA)=IDOE1
      KD3(IPC)=J
      IPC=IPC+6
      GO TO 999
*DOR N,S,JDOV,K,IW
 100  RDOE1=RA(ITA-2)
      E2=RA(ITA-1)
      E3=RA(ITA)
      K=MAX0(0,INT ((E2-RDOE1+E3)/E3))
      IA(ITA-2)=K
      KD(IPC+4)=K
      Q(IPC+5)=E3
      GO TO 9901
*ODI IDOV,LABDOB
 101  I=KD1(IPC)+IBASE
      J=KD(I)
      KD1(I)=KD1(I)-1
***      CALL CSWORD(J,MIWORD(J)+KD2(I))
      IA(J-JTOPA)=IA(J-JTOPA)+KD2(I)
***
      IF(KD1(I).GT.0)THEN
        IPC=KD2(IPC)+IBASE
      ELSE
        IPC=IPC+3
      ENDIF
      GO TO 999
*ODR IDOV,LABDOB
 102  I=KD1(IPC)+IBASE
      J=KD(I)
      KD1(I)=KD1(I)-1
***      CALL CSWORD(J,CRWORD(J)+Q(I+2))
      RA(J-JTOPA)=RA(J-JTOPA)+Q(I+2)
***
      IF(KD1(I).GT.0)THEN
        IPC=KD2(IPC)+IBASE
      ELSE
        IPC=IPC+3
      ENDIF
      GO TO 999
*PAUSE NCH,TEXT
  103 NCH=KD(IPC+1)
      REC='CSPAUSE:'
      IF(NCH.GT.0)CALL CCOPYS(MJCHAR(KD(IPC+2)),JSR+8,NCH)
      CALL CSSOUT(REC(:8+NCH))
      PRINT *, ' CSINTZ PAUSE'
*     PAUSE
      IPC=IPC+2
      IF(NCH.GT.0)IPC=IPC+(NCH-1)/NBYTPW+1
      GO TO 999
*QUIT
  104 CSINTZ=2
      GO TO 777
#if !defined(CERNLIB_PAW)
*STOP
 105  STOP
#endif
#if defined(CERNLIB_PAW)
  105 CSINTZ = 2
      GO TO 777
#endif
*NUM I
 106  NUMST=KD1(IPC)
      IPC=IPC+2
      GO TO 999
*INP 0,NPAR[,IE,IT]*NPAR
 107  CALL CSKINP
      GO TO 999
*TYP 0,NPAR[,IE,IT]*NPAR
 108  CALL CSKTYP
      GO TO 999
* CONT
 109  GO TO 998
* JMPT NW,CHAR
 110  IF(IJMPT)1101,1102,1103
*               BL   PAR  JMP
 1101 IF(IPC.NE.IPARCE)GO TO 1103
       IJMPT=1
      GO TO 93
 1102 IF(IPC.NE.IPARCE)GO TO 1103
      CSINTZ=IA(ITA)
      IJMPT=1
      GO TO 777
 1103 N=KD1(IPC)
      IPC=IPC+N+2
      GO TO 999
* SVL I
 111  IDA(IDP+KD1(IPC))=IA(ITA)
      ITA=ITA-1
      IPC=IPC+2
      GO TO 999
*RETM
 112  CSINTZ=1
      GO TO 777
*EXIT
 113  CSINTZ=2
      GO TO 777
*PUSH I
 114  I=KD1(IPC)
***      CALL CCOPYA(DCSRES,IA(ITA+1),I)
      IA(ITA+1)=IDPRS(1)
      IA(ITA+2)=IDPRS(2)
      IPC=IPC+2
      ITA=ITA+I
      GO TO 4502
*LPB LBLK
 115  IPC=IPC+2
      ITA=ITA+1
      IA(ITA)=-IPC
      KD1(IPC)=IAP
      KD2(IPC)=IBASE
      KD3(IPC)=IDP
      IPC=IPC+KD(IPC-1)
      GO TO 4502
* D==D
 116  K=1
      GO TO 1160
*D<>D
 117  K=2
      GO TO 1160
*D < D
 118  K=3
      GO TO 1160
*D<=D
 119  K=4
      GO TO 1160
*D>=D
 120  K=5
      GO TO 1160
* D>D
 121  K=6
 1160 ITA=ITA-KDLEN2
***      CALL CCOPYA(IA(ITA+1),D1,KDLEN2)
      IEQD1(1)=IA(ITA+1)
      IEQD1(2)=IA(ITA+2)
      IEQD1(3)=IA(ITA+3)
      IEQD1(4)=IA(ITA+4)
      I=0
      GO TO(1161,1162,1163,1164,1165,1166,1311,1312,1313,1314,1315),K
 1161 IF(D1.EQ.D2)I=1
      GO TO 1167
 1162 IF(D1.NE.D2)I=1
      GO TO 1167
 1163 IF(D1.LT.D2)I=1
      GO TO 1167
 1164 IF(D1.LE.D2)I=1
      GO TO 1167
 1165 IF(D1.GE.D2)I=1
      GO TO 1167
 1166 IF(D1.GT.D2)I=1
 1167 ITA=ITA+1
      IA(ITA)=I
      GO TO 998
*-D
  122 I=ITA-KDLEN+1
      IEQD(1)=IA(I)
      IEQD(2)=IA(I+1)
      D=-D
      IA(I)=IEQD(1)
      IA(I+1)=IEQD(2)
      GO TO 998
*DP(I)
  123 D=IA(ITA)
      GO TO 1241
*DP(R)
  124 D=RA(ITA)
*** 1241 CALL CCOPYA(D,IA(ITA),KDLEN)
 1241 IA(ITA)=IEQD(1)
      IA(ITA+1)=IEQD(2)
      ITA=ITA-1+KDLEN
      IPC=IPC+1
      GO TO 4502
*I(DP) ...;DP --> ...;I
  125 ITA=ITA-KDLEN+1
***      CALL CCOPYA(IA(ITA),D,KDLEN)
      IEQD(1)=IA(ITA)
      IEQD(2)=IA(ITA+1)
      IA(ITA)=D
      GO TO 998
*R(DP) ...;DP --> ...;R
  126 ITA=ITA-KDLEN+1
***      CALL CCOPYA(IA(ITA),D,KDLEN)
      IEQD(1)=IA(ITA)
      IEQD(2)=IA(ITA+1)
      RA(ITA)=D
      GO TO 998
*DP2(I)   ....I;DP  --->  ...DP;DP
  127 K=0
 1270 I=ITA-KDLEN
***      CALL CCOPYA(IA(I+1),D1,KDLEN)
      IEQD1(1)=IA(I+1)
      IEQD1(2)=IA(I+2)
      IF(K.EQ.0)THEN
         D=IA(I)
      ELSE
         D=RA(I)
      ENDIF
***      CALL CCOPYA(D,IA(I),KDLEN2)
      IA(I)=IEQD(1)
      IA(I+1)=IEQD(2)
      IA(I+2)=IEQD(3)
      IA(I+3)=IEQD(4)
      ITA=I+KDLEN2-1
      IPC=IPC+1
      GO TO 4502
*DP2(R)
  128 K=1
      GO TO 1270
*D+D
  129 K=7
      GO TO 1160
*D-D
  130 K=8
      GO TO 1160
*D*D
  131 K=9
      GO TO 1160
*D/D
  132 K=10
      GO TO 1160
*D**D
  133 K=11
      GO TO 1160
 1311 D=D1+D2
      GO TO 1316
 1312 D=D1-D2
      GO TO 1316
 1313 D=D1*D2
      GO TO 1316
 1314 D=D1/D2
      GO TO 1316
 1315 D=D1**D2
*** 1316 CALL CCOPYA(D,IA(ITA+1),KDLEN)
 1316 IA(ITA+1)=IEQD(1)
      IA(ITA+2)=IEQD(2)
      ITA=ITA+KDLEN
      GO TO 998
*D**I
  134 K=0
 1340 I=ITA-KDLEN
***      CALL CCOPYA(IA(I),D1,KDLEN)
      IEQD1(1)=IA(I)
      IEQD1(2)=IA(I+1)
      IF(K.EQ.0) THEN
        D=D1**IA(ITA)
      ELSE
        D=D1**RA(ITA)
      ENDIF
***      CALL CCOPYA(D,IA(I),KDLEN)
      IA(I)=IEQD(1)
      IA(I+1)=IEQD(2)
      ITA=ITA-1
      GO TO 998
*D**R
  135 K=1
      GO TO 1340
*D=D
  136 ITA=ITA-KDLEN-1
***      CALL COPYAJ(JTOPA+ITA+2,IA(ITA+1),KDLEN)
      J=IA(ITA+1)-JTOPA
      IA(J)=IA(ITA+2)
      IA(J+1)=IA(ITA+3)
      GO TO 998
*LDK DK
  137 IPC=IPC+1
***      CALL CCOPYA(KD(IPC),IA(ITA+1),KDLEN)
      IA(ITA+1)=KD(IPC)
      IA(ITA+2)=KD1(IPC)
 1370 IPC=IPC+KDLEN
      ITA=ITA+KDLEN
      GO TO 4502
*LADK DK
  138 IPC=IPC+1
      ITA=ITA+1
      IA(ITA)=LOCF(KD(IPC))
      IPC=IPC+KDLEN
      GO TO 4502
*LVDL ISHIFT
  139 I=IDP+KD1(IPC)
***      CALL CCOPYA(IDA(I),IA(ITA+1),KDLEN)
      IA(ITA+1)=IDA(I)
      IA(ITA+2)=IDA(I+1)
      IPC=IPC+2
      ITA=ITA+KDLEN
      GO TO 4502
*LADL ISHIFT
  140 GO TO 51
*LVDG N,I
  141 IPC=IPC+1
      J=IQ(KD(IPC))+KD1(IPC)
 1410 IPC=IPC+2
***      CALL COPYAJ(J,JTOPA+ITA+1,KDLEN)
      IA(ITA+1)=IA(J-JTOPA)
      IA(ITA+2)=IA(J-JTOPA+1)
      ITA=ITA+KDLEN
      GO TO 4502
*LADG N,I
  142 GO TO 53
*LVDP I
  143 J=IA(IAP+KD1(IPC))
      GO TO 1410
*LADP I
  144 GO TO 55
*LVDF I
  145 GO TO 139
*LADF I
  146 GO TO 140
*LEDAL IDARR,NIND
  147 IVAL=1
      LENEGI=KDLEN
      GO TO 7401
*LADAL IDARR,NIND
  148 IVAL=0
      LENEGI=KDLEN
      GO TO 7401
*WRITE ISFI,IPTF,LABEND, LABERR
  149 KEYRW=2
      GO TO 1501
*READ IOLUN,IPTF,LABEND, LABERR
  150 KEYRW=1
 1501 I=CSKRDW(JFMT,JINTBF,JCHIBF,LENIBF,CHIBUF,KINTWR)
      IF(I.LT.0)THEN
         CSINTZ=I
         GO TO 7771
      ENDIF
      GO TO 999
*IOV IT
  151 I=CSKIOV(CHIOV,JCHIOV,LCHIOV)
      IF(I.LT.0)THEN
         CSINTZ=I
         GO TO 7771
      ENDIF
      GO TO 999
*IOA IT,IDARR
  152 I=CSKIOA(CHIOV,JCHIOV,LCHIOV)
      IF(I.LT.0)THEN
         CSINTZ=I
         GO TO 7771
      ENDIF
      GO TO 999
*SVDL I
  153 ITA=ITA-KDLEN
***      CALL CCOPYA(IA(ITA+1),IDA(IDP+KD1(IPC)),KDLEN)
      I=IDP+KD1(IPC)
      IDA(I)=IA(ITA+1)
      IDA(I+1)=IA(ITA+2)
      IPC=IPC+2
      GO TO 999
*IFAI L1,L2,L3
  154 ITA=ITA-1
      IF(IA(ITA+1)) 1541,1542,1543
 1541 IPC=KD1(IPC)+IBASE
      GO TO 999
 1542 IPC=KD2(IPC)+IBASE
      GO TO 999
 1543 IPC=KD3(IPC)+IBASE
      GO TO 999
*IFAR L1,L2,L3
  155 ITA=ITA-1
      IF(RA(ITA+1)) 1541,1542,1543
*IFAD L1,L2,L3
  156 ITA=ITA-KDLEN
***      CALL CCOPYA(IA(ITA+1),D,KDLEN)
      IEQD(1)=IA(ITA+1)
      IEQD(2)=IA(ITA+2)
      IF(D) 1541,1542,1543
*CLOSE
 157  I=IA(ITA)
      ITA=ITA-1
      CLOSE(I)
      GO TO 998
*REWIND
 158  I=IA(ITA)
      ITA=ITA-1
      REWIND I
      GO TO 998
*BACKSPACE
 159  I=IA(ITA)
      ITA=ITA-1
      BACKSPACE I
      GO TO 998
*ENDFILE
 160  I=IA(ITA)
      ITA=ITA-1
      ENDFILE I
      GO TO 998
*INQUIRE LUNorFILE,ERRLAB
 161  I=KD1(IPC)
      LIOERR=KD2(IPC)
      IF(LIOERR.NE.-1)LIOERR=IBASE+LIOERR
      CALL CSKINQ(I)
      IF(I.EQ.0)THEN
        IF(LIOERR.NE.-1)THEN
          IPC=LIOERR
        ELSE
          PRINT *,' CS: Error during inquire'
          GO TO 704
        ENDIF
      ELSE
        IPC=IPC+3
      ENDIF
      GO TO 999
#if defined(CERNLIB_PAW)
*Load address of a kuip vector.
*lkva index,modegi
 162  I=KD1(IPC)
 1620 LLOW=KUVLNK(I)
      IF(LLOW.GT.0)THEN
        J=JKUVBS+LLOW
        IA(ITA)=J
        ITA=ITA+1
        IQ(KD2(IPC)+1)=J
      ELSE
        CALL KUVECT(KUVNMS(I),LLOW,LHIGH)
        IF(LLOW.EQ.0)THEN
          CSINTZ=-13
          GO TO 7771
        ENDIF
        KUVLNK(I)=LLOW-KUVOFS
        GO TO 1620
      ENDIF
      IPC=IPC+3
      GO TO 999
#endif
#if !defined(CERNLIB_PAW)
 162  GO TO 700
#endif
* CX==CX
 163  K=1
      GO TO 1640
*CX<>CX
 164  K=2
 1640 ITA=ITA-KLCMLX*2
      IEQD1(1)=IA(ITA+1)
      IEQD1(2)=IA(ITA+2)
      IEQD1(3)=IA(ITA+3)
      IEQD1(4)=IA(ITA+4)
      I=0
      GO TO (1641,1642, 1791,1792,1793,1794,1795),K
 1641 IF(CX1.EQ.CX2)I=1
      GO TO 1643
 1642 IF(CX1.NE.CX2)I=1
 1643 ITA=ITA+1
      IA(ITA)=I
      GO TO 998
*-CX
  165 I=ITA-KLCMLX+1
      IEQD(1)=IA(I)
      IEQD(2)=IA(I+1)
      CX=-CX
      IA(I)=IEQD(1)
      IA(I+1)=IEQD(2)
      GO TO 998
*CX(I)
  166 CX=IA(ITA)
      GO TO 1671
*CX(R)
  167 CX=RA(ITA)
 1671 IA(ITA)=IEQD(1)
      IA(ITA+1)=IEQD(2)
      ITA=ITA-1+KLCMLX
      IPC=IPC+1
      GO TO 4502
*CX(D)
  168 IEQD(1)=IA(ITA-1)
      IEQD(2)=IA(ITA)
      CX=CMPLX(D)
      IA(ITA-1)=IEQD(1)
      IA(ITA)=IEQD(2)
      GO TO 998
*I(CX) ...;CX --> ...;I
  169 ITA=ITA-KLCMLX+1
***      CALL CCOPYA(IA(ITA),D,KDLEN)
      IEQD(1)=IA(ITA)
      IEQD(2)=IA(ITA+1)
      IA(ITA)=CX
      GO TO 998
*R(CX) ...;CX --> ...;R
  170 ITA=ITA-KLCMLX+1
***      CALL CCOPYA(IA(ITA),D,KDLEN)
      IEQD(1)=IA(ITA)
      IEQD(2)=IA(ITA+1)
      RA(ITA)=CX
      GO TO 998
*D(CX) ...;CX --> ...;DP
  171 IEQD(1)=IA(ITA)
      IEQD(2)=IA(ITA+1)
      D=CX
      IA(ITA)=IEQD(1)
      IA(ITA+1)=IEQD(2)
      GO TO 998
*CX2(I)   ....I;CX  --->  ...CX;CX
  172 K=0
 1720 I=ITA-KLCMLX
***      CALL CCOPYA(IA(I+1),D1,KDLEN)
      IF(K.EQ.0)THEN
         CX=IA(I)
      ELSE
         CX=RA(I)
      ENDIF
      IEQD(3)=IA(I+1)
      IEQD(4)=IA(I+2)
***      CALL CCOPYA(D,IA(I),KDLEN2)
      IA(I)=IEQD(1)
      IA(I+1)=IEQD(2)
      IA(I+2)=IEQD(3)
      IA(I+3)=IEQD(4)
      ITA=I+KLCMLX*2-1
      IPC=IPC+1
      GO TO 4502
*CX2(R)
  173 K=1
      GO TO 1720
*CX2(D)   ....D;CX  --->  ...CX;CX
  174 IEQD(1)=IA(ITA-3)
      IEQD(2)=IA(ITA-2)
      CX=D
      IA(ITA-3)=IEQD(1)
      IA(ITA-2)=IEQD(2)
      GO TO 998
*CX+CX
  175 K=3
      GO TO 1640
*CX-CX
  176 K=4
      GO TO 1640
*CX*CX
  177 K=5
      GO TO 1640
*CX/CX
  178 K=6
      GO TO 1640
*CX**CX
  179 K=7
      GO TO 1640
 1791 CX=CX1+CX2
      GO TO 1796
 1792 CX=CX1-CX2
      GO TO 1796
 1793 CX=CX1*CX2
      GO TO 1796
 1794 CX=CX1/CX2
      GO TO 1796
 1795 CX=CX1**CX2
 1796 IA(ITA+1)=IEQD(1)
      IA(ITA+2)=IEQD(2)
      ITA=ITA+KLCMLX
      GO TO 998
*CX**I
  180 K=0
 1800 I=ITA-KLCMLX
***      CALL CCOPYA(IA(I),D1,KDLEN)
      IEQD1(1)=IA(I)
      IEQD1(2)=IA(I+1)
      IF(K.EQ.0) THEN
        CX=CX1**IA(ITA)
      ELSE
        CX=CX1**RA(ITA)
      ENDIF
***      CALL CCOPYA(D,IA(I),KDLEN)
      IA(I)=IEQD(1)
      IA(I+1)=IEQD(2)
      ITA=ITA-1
      GO TO 998
*CX**R
  181 K=1
      GO TO 1800
*CX**D
  182 ITA=ITA-KDLEN
      IEQD(1)=IA(ITA-1)
      IEQD(2)=IA(ITA)
      IEQD1(1)=IA(ITA+1)
      IEQD1(2)=IA(ITA+2)
      CX=CX**SNGL(D1)
      IA(ITA-1)=IEQD(1)
      IA(ITA)=IEQD(2)
      GO TO 998
*CX=CX
  183 ITA=ITA-KLCMLX-1
      J=IA(ITA+1)-JTOPA
      IA(J)=IA(ITA+2)
      IA(J+1)=IA(ITA+3)
      GO TO 998
*LCXK CXK
  184 IPC=IPC+1
***      CALL CCOPYA(KD(IPC),IA(ITA+1),KDLEN)
      IA(ITA+1)=KD(IPC)
      IA(ITA+2)=KD1(IPC)
 1840 IPC=IPC+KLCMLX
      ITA=ITA+KLCMLX
      GO TO 4502
*LACXK CXK
  185 IPC=IPC+1
      ITA=ITA+1
      IA(ITA)=LOCF(KD(IPC))
      IPC=IPC+KLCMLX
      GO TO 4502
*LVCXL ISHIFT
  186 I=IDP+KD1(IPC)
***      CALL CCOPYA(IDA(I),IA(ITA+1),KDLEN)
      IA(ITA+1)=IDA(I)
      IA(ITA+2)=IDA(I+1)
      IPC=IPC+2
      ITA=ITA+2
      GO TO 4502
*LACXL ISHIFT
  187 GO TO 51
*LVCXG N,I
  188 IPC=IPC+1
      J=IQ(KD(IPC))+KD1(IPC)
 1880 IPC=IPC+2
***      CALL COPYAJ(J,JTOPA+ITA+1,KDLEN)
      IA(ITA+1)=IA(J-JTOPA)
      IA(ITA+2)=IA(J-JTOPA+1)
      ITA=ITA+2
      GO TO 4502
*LACXG N,I
  189 GO TO 53
*LVCXP I
  190 J=IA(IAP+KD1(IPC))
      GO TO 1880
*LACXP I
  191 GO TO 55
*LVCXF I
  192 GO TO 186
*LACXF I
  193 GO TO 187
*LECXAL IDARR,NIND
  194 IVAL=1
      LENEGI=KLCMLX
      GO TO 7401
*LACXAL IDARR,NIND
  195 IVAL=0
      LENEGI=KLCMLX
      GO TO 7401
*SVCXL I
  196 ITA=ITA-KLCMLX
***      CALL CCOPYA(IA(ITA+1),IDA(IDP+KD1(IPC)),KDLEN)
      I=IDP+KD1(IPC)
      IDA(I)=IA(ITA+1)
      IDA(I+1)=IA(ITA+2)
      IPC=IPC+2
      GO TO 999
*I**CX
  197 K=0
 1970 ITA=ITA-1
***      CALL CCOPYA(IA(I),D1,KDLEN)
      IEQD1(1)=IA(ITA)
      IEQD1(2)=IA(ITA+1)
      IF(K.EQ.0) THEN
        CX=IA(ITA-1)**CX1
      ELSE
        CX=RA(ITA-1)**CX1
      ENDIF
      IA(ITA-1)=IEQD(1)
      IA(ITA)  =IEQD(2)
      GO TO 998
*R**CX
  198 K=1
      GO TO 1970
*D**CX
  199 ITA=ITA-KDLEN
      IEQD1(1)=IA(ITA-1)
      IEQD1(2)=IA(ITA)
      IEQD(1)=IA(ITA+1)
      IEQD(2)=IA(ITA+2)
      CX=SNGL(D1)**CX
      IA(ITA-1)=IEQD(1)
      IA(ITA)=IEQD(2)
      GO TO 998
*LEKV index,IDARR,NIND
#if defined(CERNLIB_PAW)
 200  IVAL=1
 2001 LENEGI=1
      I=KD1(IPC)
 2002 LLOW=KUVLNK(I)
      IF(LLOW.GT.0)THEN
        ID=KD2(IPC)
        IQ(ID+1)=JKUVBS+LLOW
      ELSE
        CALL KUVECT(KUVNMS(I),LLOW,LHIGH)
        IF(LLOW.EQ.0)THEN
          CSINTZ=-13
          GO TO 7771
        ENDIF
        KUVLNK(I)=LLOW-KUVOFS
        GO TO 2002
      ENDIF
      IPC=IPC+1
      GO TO 7402
#endif
#if !defined(CERNLIB_PAW)
 200  GO TO 700
#endif
*LAKV index,IDARR,NIND
 201  IVAL=0
      GO TO 2001
 700  CSINTZ=0
      GO TO 7771
 701  CSINTZ=-1
      GO TO 7771
 702  CSINTZ=-2
      GO TO 7771
 703  CSINTZ=-3
      GO TO 7771
 704  CSINTZ=-4
      GO TO 7771
* 705  CSINTZ=-5
*      GO TO 7771
 706  CSINTZ=-6
      GO TO 7771
C 707  CSINTZ=-7
C      GO TO 7771
* 708  CSINTZ=-8
*      GO TO 7771
* 709  CSINTZ=-9
*      GO TO 7771
 710  CSINTZ=-10
       GO TO 7771
 711  CSINTZ=-11
       GO TO 7771
 712  CSINTZ=-12
 7771 CALL CSIERR(CSINTZ)
  777 RETURN
      END
#endif
