*
* $Id: csintx.F,v 1.2 1996/12/05 09:50:37 berejnoi Exp $
*
* $Log: csintx.F,v $
* Revision 1.2  1996/12/05 09:50:37  berejnoi
* Mods for WINNT: form correct arg. list for chars
*
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :          15/11/95  12.50.16  by  Julian Bunn
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSINTX(IPARCE,IJMPT)
***--------------------------------------------
*       The interpreter
***--------------------------------------------

#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
*+SEQ,CSBUF.
#include "comis/csrec.inc"
#include "comis/cstabps.inc"
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
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_ALPHA_OSF))
      INTEGER CSTRCMP
#endif
#if !defined(CERNLIB_UNIX)||defined(CERNLIB_ALPHA_OSF)
      INTEGER CSLEQS,CSLNES,CSLLTS,CSLLES,CSLGES,CSLGTS
#endif
      INTEGER CSCALI
      DOUBLE PRECISION CSCALD
#if defined(CERNLIB_APOLLO)
      INTEGER JNAME,INAME
      POINTER/JNAME/INAME
#endif
#if defined(CERNLIB_IBM)
      DATA IBZ00/4HBZ00/
#endif
       INTEGER  CSIOED,CSKCAL,CSKRDW,CSKIOV,CSKIOA
#if defined(CERNLIB_VAX)
       EXTERNAL CSHNDL
       INTEGER CSHNDL
#endif
      EQUIVALENCE (IDOE1,RDOE1)
      INTEGER IEQD(6),IEQD1(4),IEQD2(2),IDPRS(2)
      EQUIVALENCE (IEQD,D),(IEQD1,D1),(IEQD2,D2),(IDPRS,DCSRES)
      CHARACTER *256 CHIOV, CHIBUF
      SAVE CHIOV, CHIBUF,JCHIBF,JFMT,JCHIOV,LCHIOV,JDOE1,IDOE1
      SAVE JINTBF,LENIBF,KINTWR
      SAVE JTCHB,NCHSB
        PARAMETER (KLCMLX=2)
      DATA NOENTR/0/
      MIWORD(J)=IA(J-JTOPA)
      IF(NOENTR.EQ.0)THEN
        JCHIBF=MJSCHA(CHIBUF)
        JFMT=MJSCHA(FMT)
        JCHIOV=MJSCHA(CHIOV)
        LCHIOV=LEN(CHIOV)
        JDOE1=LOCF(IDOE1)
        JTCHB=JTCH
        NCHSB=NCHS
        NOENTR=1
      ENDIF
#if defined(CERNLIB_VAX)
      CALL LIB$ESTABLISH(CSHNDL)
#endif
****      JTOPA=LOCF(IA(1))-1
      GO TO 999
** 400  JTCH=IA(ITA-2)
**      NCHS=NCHS+IA(ITA-3)+IA(ITA-1)
 400  JTCH=JTCHB
      NCHS=NCHSB
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
     3  188,189,190,191,192,193,194,195,196,197,198,199,200,201,
     8  202,203,204,205,206,207,208  ),KD(IPC)
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
#if !defined(CERNLIB_UNIX)||defined(CERNLIB_ALPHA_OSF)
 9    I=CSLEQS(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1))
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_ALPHA_OSF))
 9    I=0
      IF(CSTRCMP(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1)).EQ.0)I=1
#endif
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
#if !defined(CERNLIB_UNIX)||defined(CERNLIB_ALPHA_OSF)
 12   I=CSLNES(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1))
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_ALPHA_OSF))
 12   I=0
      IF(CSTRCMP(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1)).NE.0)I=1
#endif
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
#if !defined(CERNLIB_UNIX)||defined(CERNLIB_ALPHA_OSF)
  15  I=CSLLTS(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1))
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_ALPHA_OSF))
  15  I=0
      IF(CSTRCMP(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1)).LT.0)I=1
#endif
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
#if !defined(CERNLIB_UNIX)||defined(CERNLIB_ALPHA_OSF)
  18  I=CSLLES(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1))
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_ALPHA_OSF))
  18  I=0
      IF(CSTRCMP(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1)).LE.0)I=1
#endif
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
#if !defined(CERNLIB_UNIX)||defined(CERNLIB_ALPHA_OSF)
  21  I=CSLGES(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1))
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_ALPHA_OSF))
  21  I=0
      IF(CSTRCMP(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1)).GE.0)I=1
#endif
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
#if !defined(CERNLIB_UNIX)||defined(CERNLIB_ALPHA_OSF)
  24  I=CSLGTS(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1))
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_ALPHA_OSF))
  24  I=0
      IF(CSTRCMP(IA(ITA-2),IA(ITA-3),IA(ITA),IA(ITA-1)).GT.0)I=1
#endif
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
**      JTCH=IA(ITA)
**      NCHS=NCHS+N2
      JTCH=JTCHB
      NCHS=NCHSB
      ITA=ITA-4
      GO TO 998
*CH//CH
**  44  IA(ITA-3)=IA(ITA-3) + IA(ITA-1)
  44  J=IA(ITA-2)
      N1=IA(ITA-3)
      J2=IA(ITA)
      N2=IA(ITA-1)
      IF(J2+N2 .EQ. JTCH)THEN
        IF(J+N1 .EQ. J2)THEN
*   stack,stack
*          IA(ITA-3)=N1+N2
        ELSE
*   memory,stack
          NCHS=NCHS-N1
          IF(NCHS.LT.0)GO TO 702
          CALL CCOPYS(J2,J2+N1,N2)
          CALL CCOPYS(J,J2,N1)
          IA(ITA-2)=J2
          JTCH=JTCH+N1
        ENDIF
      ELSE
        IF(J+N1 .EQ. JTCH)THEN
*   stack,memory
          NCHS=NCHS-N2
          IF(NCHS.LT.0)GO TO 702
          CALL CCOPYS(J2,JTCH,N2)
          JTCH=JTCH+N2
        ELSE
*   memory,memory
          NCHS=NCHS-(N1+N2)
          IF(NCHS.LT.0)GO TO 702
          CALL CCOPYS(J,JTCH,N1)
          IA(ITA-2)=JTCH
          JTCH=JTCH+N1
          CALL CCOPYS(J2,JTCH,N2)
          JTCH=JTCH+N2
        ENDIF
      ENDIF
      IA(ITA-3)=N1+N2
      ITA= ITA-2
      GO TO 998
*LK K
  45  ITA=ITA+1
      IA(ITA)=KD1(IPC)
 4501 IPC=IPC+2
 4502 CONTINUE
****      IF(ITA.GT.LAST)GO TO 701
*                    ---------
      GO TO 999
* LAK K
  46  ITA=ITA+1
      IA(ITA)=LOCF(KD1(IPC))
***      GO TO 4501
      IPC=IPC+2
      GO TO 999
*LCK N,'TEXT'
  47  N=KD1(IPC)
      J=MJCHAR(KD2(IPC))
      ITA=ITA+2
      IA(ITA-1)=N
      IA(ITA)=J
**      IA(ITA)=JTCH
**      NCHS=NCHS-N
**      IF(NCHS.LT.0)GO TO 702
***                 -----------
**      CALL CCOPYS(J,JTCH,N)
**      JTCH=JTCH+N
 4701 I=(N-1)/NBYTPW+3
      IPC=IPC+I
**      GO TO 4502
      GO TO 999
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
***      GO TO 4501
      IPC=IPC+2
      GO TO 999
*LAL  I
  51  ITA=ITA+1
      IA(ITA)=LOCF(IDA(IDP+KD1(IPC)))
***      GO TO 4501
      IPC=IPC+2
      GO TO 999
*LVG N,I
  52  J=IQ(KD1(IPC))+KD2(IPC)
      IPC=IPC+3
 5201 ITA=ITA+1
      IA(ITA)=IA(J-JTOPA)
      GO TO 999
*LAG N,I
  53  ITA=ITA+1
      IA(ITA)=IQ(KD1(IPC))+KD2(IPC)
      IPC=IPC+3
**      GO TO 4502
      GO TO 999
*LVP I
  54  J=IA(IAP+KD1(IPC))
      IPC=IPC+2
      ITA=ITA+1
      IA(ITA)=IA(J-JTOPA)
      GO TO 999
*LAP I
  55  ITA=ITA+1
      IA(ITA)=IA(IAP+KD1(IPC))
***      GO TO 4501
      IPC=IPC+2
      GO TO 999
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
  58  J=MJCHAR(IDA(IDP+KD2(IPC)))
 5801 NB=KD1(IPC)
      IF(NB.LT.0)THEN
        N=-NB
        ITA=ITA+2
      ELSE
        N2=IA(ITA)
        N1=IA(ITA-1)
        N=N2-N1+1
        IF(N.LT.1.OR.N1.LT.1.OR.N2.GT.NB)GO TO 703
*                                      ---------
        J=J+N1-1
      ENDIF
      IA(ITA-1)=N
      IA(ITA)=J
**      IA(ITA)=JTCH
**      NCHS=NCHS-N
**      IF(NCHS.LT.0)GO TO 702
***                   ----------
**      CALL CCOPYS(J,JTCH,N)
**      JTCH=JTCH+N
      IPC=IPC+3
      GO TO 999
*LACL NB,I
  59  J=MJCHAR(IDA(IDP+KD2(IPC)))
 5901 NB=KD1(IPC)
      IF(NB.LT.0)THEN
        N=-NB
        ITA=ITA+2
      ELSE
        N2=IA(ITA)
        N1=IA(ITA-1)
        N=N2-N1+1
        IF(N.LT.1.OR.N1.LT.1.OR.N2.GT.NB)GO TO 703
*                                      ---------
        J=J+N1-1
      ENDIF
      IA(ITA-1)=N
      IA(ITA)=J
      IPC=IPC+3
      GO TO 999
*LVCG N,NB,I
  60  IPC=IPC+1
      J=(IQ(KD(IPC))+KD2(IPC))*NBYTPW
      GO TO 5801
*LACG N,NB,I
  61  IPC=IPC+1
      J=(IQ(KD(IPC))+KD2(IPC))*NBYTPW
      GO TO 5901
*LVCP X,I
  62  JD=IA(IAP+KD2(IPC))
      J=MIWORD(JD+1)
      NB=MOD(MIWORD(JD),KON3)
      IF(IA(ITA).EQ.0)IA(ITA)=NB
      N2=IA(ITA)
      N1=IA(ITA-1)
      N=N2-N1+1
      IF(N.LT.1.OR.N1.LT.1.OR.N2.GT.NB)GO TO 703
*                                      ---------
      J=J+N1-1
      IA(ITA-1)=N
      IA(ITA)=J
**      IA(ITA)=JTCH
**      NCHS=NCHS-N
**      IF(NCHS.LT.0)GO TO 702
***                   ----------
**      CALL CCOPYS(J,JTCH,N)
**      JTCH=JTCH+N
      IPC=IPC+3
      GO TO 999
*LACP X,I
  63  JD=IA(IAP+KD2(IPC))
      J=MIWORD(JD+1)
      NB=MOD(MIWORD(JD),KON3)
      IF(IA(ITA).EQ.0)IA(ITA)=NB
      N2=IA(ITA)
      N1=IA(ITA-1)
      N=N2-N1+1
      IF(N.LT.1.OR.N1.LT.1.OR.N2.GT.NB)GO TO 703
*                                      ---------
      IA(ITA-1)=N
      IA(ITA)=J+N1-1
      IPC=IPC+3
      GO TO 999
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
*****      IF(ITA.GT.LAST)GO TO 701
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
  70  I=KD2(IPC)
      IQ(I+1)=IA(IAP+KD1(IPC))
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
        IF(M.LE.0)GO TO 705
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
        IA(ITA+1)=IA(J-JTOPA)
        IF(LENEGI.EQ.2)IA(ITA+2)=IA(J-JTOPA+1)
        ITA=ITA+LENEGI
      ENDIF
      IPC=IPC+3
      GO TO 999
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
      CSINTX=1
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
      N=N2-N1+1
      IF(N.LT.1.OR.N1.LT.1.OR.N2.GT.NB)GO TO 703
*                                      ---------
      J=J+N1-1
      IA(ITA-1)=N
**      IF(IVAL.EQ.1)THEN
        IA(ITA)=J
**        IA(ITA)=JTCH
**        NCHS=NCHS-N
**        IF(NCHS.LT.0)GO TO 702
***                   ----------
**        CALL CCOPYS(J,JTCH,N)
**        JTCH=JTCH+N
**      ELSE
**         IA(ITA)=J
**      ENDIF
      IPC=IPC+3
      GO TO 999
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
#if defined(CERNLIB_VAX)
     +       SHARED,
#endif
     +       IOSTAT=IOSTA , ERR=8201)
      ELSEIF(I.EQ.0 .AND. I1.EQ.1)THEN
        OPEN(LUNOP,STATUS=STATOP,ACCESS=ACCEOP,FORM=FORMOP,
     +       RECL=LRECOP,
#if defined(CERNLIB_VAX)
     +       SHARED,
#endif
     +       IOSTAT=IOSTA , ERR=8201)
      ELSEIF(I.EQ.1 .AND. I1.EQ.0)THEN
        OPEN(LUNOP,STATUS=STATOP,ACCESS=ACCEOP,FORM=FORMOP,
     +       FILE=FILEOP,
#if defined(CERNLIB_VAX)
     +       SHARED,
#endif
     +       IOSTAT=IOSTA , ERR=8201)
      ELSEIF(I.EQ.1 .AND. I1.EQ.1)THEN
        OPEN(LUNOP,STATUS=STATOP,ACCESS=ACCEOP,FORM=FORMOP,
     +       FILE=FILEOP,
     +       RECL=LRECOP,
#if defined(CERNLIB_VAX)
     +       SHARED,
#endif
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
**      JTCH=IA(ITA)
**      NCHS=NCHS+N2
      JTCH=JTCHB
      NCHS=NCHSB
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
***      GO TO 4501
      IPC=IPC+2
      GO TO 999
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
***      GO TO 4501
      IPC=IPC+2
      GO TO 999
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
         CSINTX=I
         GO TO 7771
      ENDIF
      GO TO 999
*RET
***  93  CALL CCOPYA(IDA(IDP),DCSRES,KDLEN)
  93  IDPRS(1)=IDA(IDP)
      IDPRS(2)=IDA(IDP+1)
 9301 CONTINUE
***      CALL CCOPYA(IA(IFP+6),NAMEST(1),3)
      NUMST=IA(IFP+8)
      IF(NUMST.LT.0)THEN
        NUMST=0
        CSINTX=2
        RETURN
      ENDIF
      NAMEST(1)=IA(IFP+6)
      NAMEST(2)=IA(IFP+7)
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
      PRINT *, ' CSINTX PAUSE'
*     PAUSE
      IPC=IPC+2
      IF(NCH.GT.0)IPC=IPC+(NCH-1)/NBYTPW+1
      GO TO 999
* it was command QUIT
*  104 print *,' command QUIT??'
*      CSINTX=2
*      GO TO 777
*FARGL [-]call_offset
  104 I=KD1(IPC)
      IF(I.GT.0)THEN
        IPC=IPC+I
      ELSE
        KD1(IPC)=-I
        I=2-I
        IOFSPL=IDP+KD1(IPC+I)
#if defined(CERNLIB_APOLLO)
        NPAR=KD(IPC+I)/100
        NPARCH=MOD(KD(IPC+I),100)
        IOFSCA=IOFSPL+NPAR
        IOFSCL=IOFSCA+NPARCH
#endif
#if defined(CERNLIB_IBM)
        NPAR=KD(IPC+I)/100
        IOFSCL=IOFSPL+NPAR*2
        IOFSBL=IOFSPL-1
        IDA(IOFSPL-2)=IBZ00
        IDA(IOFSPL-1)=NPAR*4
#endif
#if defined(CERNLIB_SGI)||defined(CERNLIB_IBMRT)||defined(CERNLIB_DECS)||defined(CERNLIB_HPUX)||defined(CERNLIB_SUN)||defined(CERNLIB_MSDOS)||defined(CERNLIB_LINUX)||defined(CERNLIB_WINNT)
        NPAR=KD(IPC+I)/100
        IOFSCL=IOFSPL+NPAR
#endif
        IPC=IPC+2
      ENDIF
      GO TO 999
* it was command STOP
*  105 print *,' command STOP??'
*      CSINTX = 2
*      GO TO 777
*FCALL index_tgp,npar*100+nparch,agroffs
  105 IP=KD1(IPC)
      ITP=IABS(IQ(IP+KSTPGP))
      IADGP=IQ(IP+KSADGP)
      NPAR=KD2(IPC)/100
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_SGI)||defined(CERNLIB_IBMRT)||defined(CERNLIB_DECS)||defined(CERNLIB_HPUX)||defined(CERNLIB_SUN)||defined(CERNLIB_MSDOS)||defined(CERNLIB_LINUX)||defined(CERNLIB_WINNT)
      NPAR=NPAR+MOD(KD2(IPC),100)
#endif
      IOFSPL=IDP+KD3(IPC)
#if defined(CERNLIB_VAX)
      IF(ITP.EQ.1.OR.ITP.EQ.4.OR.ITP.EQ.6)THEN
        ICSRES=CSCALI(%VAL(IADGP),NPAR,IDA(IOFSPL))
      ELSEIF(ITP.EQ.2)THEN
        RCSRES=CSCALR(%VAL(IADGP),NPAR,IDA(IOFSPL))
      ELSEIF(ITP.EQ.5)THEN
        DCSRES=CSCALD(%VAL(IADGP),NPAR,IDA(IOFSPL))
      ELSEIF(ITP.EQ.7)THEN
*Ithink it will works correctly:calls complex function
        DCSRES=CSCALD(%VAL(IADGP),NPAR,IDA(IOFSPL))
      ENDIF
#endif
#if defined(CERNLIB_APOLLO)
      JNAME=IADGP
      IF(ITP.EQ.1.OR.ITP.EQ.4.OR.ITP.EQ.6)THEN
        ICSRES=CSCALI(INAME,NPAR,IDA(IOFSPL))
      ELSEIF(ITP.EQ.2)THEN
        RCSRES=CSCALR(INAME,NPAR,IDA(IOFSPL))
      ELSEIF(ITP.EQ.5)THEN
        DCSRES=CSCALD(INAME,NPAR,IDA(IOFSPL))
      ELSEIF(ITP.EQ.7)THEN
        DCSRES=CSCALD(INAME,NPAR,IDA(IOFSPL))
      ENDIF
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_APOLLO))
      IF(ITP.EQ.1.OR.ITP.EQ.4.OR.ITP.EQ.6)THEN
        ICSRES=CSCALI(IADGP,NPAR,IDA(IOFSPL))
      ELSEIF(ITP.EQ.2)THEN
        RCSRES=CSCALR(IADGP,NPAR,IDA(IOFSPL))
      ELSEIF(ITP.EQ.5)THEN
        DCSRES=CSCALD(IADGP,NPAR,IDA(IOFSPL))
      ELSEIF(ITP.EQ.7)THEN
        DCSRES=CSCALD(IADGP,NPAR,IDA(IOFSPL))
      ENDIF
#endif
      IPC=IPC+4
      GO TO 999
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
      CSINTX=IA(ITA)
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
 112  CSINTX=1
      GO TO 777
*EXIT
 113  CSINTX=2
      GO TO 777
*PUSH I
 114  I=KD1(IPC)
***      CALL CCOPYA(DCSRES,IA(ITA+1),I)
      IA(ITA+1)=IDPRS(1)
      IA(ITA+2)=IDPRS(2)
      IPC=IPC+2
      ITA=ITA+I
**      GO TO 4502
      GO TO 999
*LPB LBLK
 115  IPC=IPC+2
      ITA=ITA+1
      IA(ITA)=-IPC
      KD1(IPC)=IAP
      KD2(IPC)=IBASE
      KD3(IPC)=IDP
      IPC=IPC+KD(IPC-1)
**      GO TO 4502
      GO TO 999
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
**      GO TO 4502
      GO TO 999
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
**      GO TO 4502
      GO TO 999
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
**      GO TO 4502
      GO TO 999
*LADK DK
  138 IPC=IPC+1
      ITA=ITA+1
      IA(ITA)=LOCF(KD(IPC))
      IPC=IPC+KDLEN
**      GO TO 4502
      GO TO 999
*LVDL ISHIFT
  139 I=IDP+KD1(IPC)
***      CALL CCOPYA(IDA(I),IA(ITA+1),KDLEN)
      IA(ITA+1)=IDA(I)
      IA(ITA+2)=IDA(I+1)
      IPC=IPC+2
      ITA=ITA+KDLEN
**      GO TO 4502
      GO TO 999
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
**      GO TO 4502
      GO TO 999
*LADG N,I
  142 GO TO 53
*LVDP I
  143 J=IA(IAP+KD1(IPC))
      GO TO 1410
*LADP I
  144 GO TO 55
* it was command LVDF I
*  145 GO TO 139
*LBAK constant
  145 IDA(IOFSPL)=LOCB(KD1(IPC))
      IOFSPL=IOFSPL+1
      IPC=IPC+2
      GO TO 999
* it was command LADF I
*  146 GO TO 140
*LBA2K part1,part2
  146 IDA(IOFSPL)=LOCB(KD1(IPC))
      IOFSPL=IOFSPL+1
      IPC=IPC+3
      GO TO 999
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
         CSINTX=I
         GO TO 7771
      ENDIF
      GO TO 999
*IOV IT
  151 I=CSKIOV(CHIOV,JCHIOV,LCHIOV)
      IF(I.LT.0)THEN
         CSINTX=I
         GO TO 7771
      ENDIF
      GO TO 999
*IOA IT,IDARR
  152 I=CSKIOA(CHIOV,JCHIOV,LCHIOV)
      IF(I.LT.0)THEN
         CSINTX=I
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
      IF(LLOW.ne.0)THEN
        J=JKUVBS+LLOW
        ITA=ITA+1
        IA(ITA)=J
        IQ(KD2(IPC)+1)=J
      ELSE
        CALL KUVECT(KUVNMS(I),LLOW,LHIGH)
        IF(LLOW.EQ.0)THEN
          CSINTX=-13
          GO TO 7771
        ENDIF
        KUVLNK(I)=LLOW-KUVOFS
        GO TO 1620
      ENDIF
      IPC=IPC+3
**      GO TO 4502
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
**      GO TO 4502
      GO TO 999
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
  171 IEQD(1)=IA(ITA-1)
      IEQD(2)=IA(ITA)
      D=CX
      IA(ITA-1)=IEQD(1)
      IA(ITA)=IEQD(2)
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
**      GO TO 4502
      GO TO 999
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
**      GO TO 4502
      GO TO 999
*LACXK CXK
  185 IPC=IPC+1
      ITA=ITA+1
      IA(ITA)=LOCF(KD(IPC))
      IPC=IPC+KLCMLX
**      GO TO 4502
      GO TO 999
*LVCXL ISHIFT
  186 I=IDP+KD1(IPC)
***      CALL CCOPYA(IDA(I),IA(ITA+1),KDLEN)
      IA(ITA+1)=IDA(I)
      IA(ITA+2)=IDA(I+1)
      IPC=IPC+2
      ITA=ITA+2
**      GO TO 4502
      GO TO 999
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
**      GO TO 4502
      GO TO 999
*LACXG N,I
  189 GO TO 53
*LVCXP I
  190 J=IA(IAP+KD1(IPC))
      GO TO 1880
*LACXP I
  191 GO TO 55
* it was command LVCXF I
*  192 GO TO 186
*LBAL locvaroffset
  192 IDA(IOFSPL)=LOCB(IDA(IDP+KD1(IPC)))
      IOFSPL=IOFSPL+1
      IPC=IPC+2
      GO TO 999
*it was command LACXF I
*  193 GO TO 187
*LBAG  noarea,offset
  193 IDA(IOFSPL)=(IQ(KD1(IPC))+KD2(IPC))*NBYTPW
      IOFSPL=IOFSPL+1
      IPC=IPC+3
      GO TO 999
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
      IF(LLOW.ne.0)THEN
        ID=KD2(IPC)
        IQ(ID+1)=JKUVBS+LLOW
      ELSE
        CALL KUVECT(KUVNMS(I),LLOW,LHIGH)
        IF(LLOW.EQ.0)THEN
          CSINTX=-13
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
#if defined(CERNLIB_PAW)
*LAKV index,IDARR,NIND
 201  IVAL=0
      GO TO 2001
#endif
#if !defined(CERNLIB_PAW)
 201  GO TO 700
#endif
*LBAKA idarr,offset
  202 IDA(IOFSPL)=(IQ(KD1(IPC)+1)+KD2(IPC))*NBYTPW
      IOFSPL=IOFSPL+1
      IPC=IPC+3
      GO TO 999
*LBAA idarr
  203 IDA(IOFSPL)=IQ(KD1(IPC)+1)*NBYTPW
      IOFSPL=IOFSPL+1
      IPC=IPC+2
      GO TO 999
*LBACK N,'TEXT'
  204 N=KD1(IPC)
      J=MJCHAR(KD2(IPC))
      I=(N-1)/NBYTPW+3
      IPC=IPC+I
 2040 CONTINUE
#if defined(CERNLIB_VAX)
*  the next command is "DCV offset"
      ID=IDP+KD1(IPC)
      IDA(ID)=N+KON1
      IDA(ID+1)=J
      IDA(IOFSPL)=LOCB(IDA(ID))
#endif
#if defined(CERNLIB_APOLLO)
      IDA(IOFSPL)=J
      IDA(IOFSCL)=N*KON3
      IDA(IOFSCA)=IADDR(IDA(IOFSCL))
      IOFSCA=IOFSCA+1
      IOFSCL=IOFSCL+1
#endif
#if defined(CERNLIB_IBM)
      IDA(IOFSPL)=J
      IDA(IOFSCL)=N
      IDA(IOFSPL+NPAR)=MJCHAR(IDA(IOFSCL))
      IOFSCL=IOFSCL+1
*?-----     CALL SBIT1(IDA(IOFSBL+NPAR*2),32) ----
      CALL SBIT1(IDA(IOFSBL+NPAR*2),32)
*+SELF,IF=SGI,IBMRT,DECS,HPUX,SUN,MSDOS.
#endif
#if (defined(CERNLIB_UNIX))&&(defined(CERNLIB_STDUNIX))
      IDA(IOFSPL)=J
#if defined(CERNLIB_WINNT)
      IOFSPL=IOFSPL+1
      IDA(IOFSPL)=N
#else
      IDA(IOFSCL)=N
      IOFSCL=IOFSCL+1
#endif
#endif
#if (defined(CERNLIB_UNIX))&&(!defined(CERNLIB_STDUNIX))
      IDA(IOFSPL)=J
#endif
      IOFSPL=IOFSPL+1
*  the next command is "DCV offset"
      IPC=IPC+2
      GO TO 999
*LBACL NBytes,I
  205 J=MJCHAR(IDA(IDP+KD2(IPC)))
      N=-KD1(IPC)
      IPC=IPC+3
      GO TO 2040
*LBACG Noarea,NBytes,I
  206 IPC=IPC+1
      J=(IQ(KD(IPC))+KD2(IPC))*NBYTPW
      N=-KD1(IPC)
      IPC=IPC+3
      GO TO 2040
*BACA IDARR
  207 ID=KD1(IPC)
#if defined(CERNLIB_VAX)
      IDA(IOFSPL)=LOCB(IQ(ID))
      IOFSPL=IOFSPL+1
      IPC=IPC+2
      GO TO 999
#endif
#if !defined(CERNLIB_VAX)
      J=IQ(ID+1)
      N=MOD(IQ(ID),KON3)
      GO TO 2040
#endif
*LBAHK NW,TEXT
 208  N=KD1(IPC)
      IDA(IOFSPL)=LOCB(KD2(IPC))
      IOFSPL=IOFSPL+1
      IPC=IPC+N+2
      GO TO 999
 700  CSINTX=0
      GO TO 7771
 702  CSINTX=-2
      GO TO 7771
 703  CSINTX=-3
      GO TO 7771
 704  CSINTX=-4
      GO TO 7771
 705  CSINTX=-5
      GO TO 7771
 710  CSINTX=-10
       GO TO 7771
 711  CSINTX=-11
       GO TO 7771
 712  CSINTX=-12
 7771 CALL CSIERR(CSINTX)
  777 RETURN
      END
