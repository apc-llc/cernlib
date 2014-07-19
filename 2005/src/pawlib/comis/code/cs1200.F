*
* $Id: cs1200.F,v 1.1.1.1 1996/02/26 17:16:21 mclareni Exp $
*
* $Log: cs1200.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:21  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/05 14/08/93  15.59.17  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CS1200(IPCE,KT1T2)
***--------------------------------------------
*       translater's routine
***--------------------------------------------
       INTEGER KT1T2(7,7)
       PARAMETER ( KIR=27, KIR2=29, KRTOCX=167,
     + KI2TOD=127,KR2TOD=128,KITOD=123,KRTOD=124,KITOCX=166,
     + KLDK=137,   KLADK=138, KLVDL=139, KDTOCX=168,
     +  KI2CX=172, KR2CX=173,KD2CX= 174, KLCXK=184)
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/cspnts.inc"
#include "comis/csdpvs.inc"
      PARAMETER (  KTINT=1,   KLINT=1, KTREAL=2,  KLREAL=1,
     +  KTCHAR=3,  KTLOG=4,   KLLOG=1, KTDOU=5,
     +            KTCMLX=7,   KLCMLX=2 )
      COMMON /CSGSCM/IGSST,JGSST,NGSST,CSJUNK(3)
#include "comis/cstvrs.inc"
C      COMMON/CSTVRS/IB1,IB2,IBEXP,IBOFS,IDES,IOFS,IPCBL,IPCL,IPCP,
C     +              IPVS,IPVSP,IT1,IT2,KDA,KDP,KEY,KGLO,KI,KNUMB,KODT,
C     +              LAB1,LCOD,LENEL,LENT,LOCLLAB,LSFT,NDIM,NDVAR,
C     +              NEL,NELT,NLAB,NUMGB,RNUM,INFVEC(18),INDX(7)
      INTEGER IEQD(2)
      EQUIVALENCE (D,IEQD)
      ITS=ITS-2
      IT1=ISEM(ITS-2)
      IB1=ISEM(ITS-1)
      IT2=ISEM(ITS)
      IB2=ISEM(ITS+1)
      IF(IB1.GT.5 .OR. IB2.GT.5)GO TO 726
*        ----------
      IBEXP=1
      IF(IB1.EQ.2.AND.IB2.EQ.2)IBEXP=2
      KEY=KT1T2(IT1,IT2)
      IF(KEY.EQ.0)GO TO 726
*       -------
      IF(KEY.GT.0)THEN
        IF(KEY.LT.8)RETURN
        K=KEY-7
        GO TO (1201,1202,1203,1204,1205,1206,1207,1208,1209),K
*               IR   IL   ID   IH   RD   RH   ICX  RCX  DCX
 1201   KODT=KIR2
        KEY=2
        IF(IB1.EQ.2)THEN
          IF(IB2.EQ.2 .OR.IB2.EQ.3)THEN
            RA(IPCP-1)=KOD(IPCP-1)
            ISEM(ITS-2)=KEY
            RETURN
          ENDIF
        ENDIF
        GO TO 1210
 1202   KEY=1
        RETURN
 1203   KODT=KI2TOD
        KEY=5
        IF(IB1.EQ.2)THEN
          IF(IB2.EQ.2.OR.IB2.EQ.3)THEN
            D=KOD(IPCP-1)
            L=IPCE-IPCP
            I=KDLEN-KLINT
            IPCE=IPCE+I
            IF(IPCE.GT.LAST)GO TO 727
*               --------
            CALL CCOPYA(KOD(IPCP),KOD(IPCP+I),L)
            CALL CCOPYA(D,KOD(IPCP-1),KDLEN)
            KOD(IPCP-2)=KLDK
            IPCP=IPCP+I
            ISEM(ITS-2)=KEY
            RETURN
          ENDIF
        ENDIF
        GO TO 1210
 1204   KEY=1
        RETURN
 1205   KODT=KR2TOD
        KEY=5
        IF(IB1.EQ.2)THEN
          IF(IB2.EQ.2.OR.IB2.EQ.3)THEN
            D=RA(IPCP-1)
            L=IPCE-IPCP
            I=KDLEN-KLREAL
            IPCE=IPCE+I
            IF(IPCE.GT.LAST)GO TO 727
*               --------
            CALL CCOPYA(KOD(IPCP),KOD(IPCP+I),L)
            CALL CCOPYA(D,KOD(IPCP-1),KDLEN)
            KOD(IPCP-2)=KLDK
            IPCP=IPCP+I
            ISEM(ITS-2)=KEY
            RETURN
          ENDIF
        ENDIF
        GO TO 1210
 1206   KEY=2
        RETURN
 1207   KODT=KI2CX
        KEY=7
        IF(IB1.EQ.2)THEN
          IF(IB2.EQ.2.OR.IB2.EQ.3)THEN
            CX=KOD(IPCP-1)
            L=IPCE-IPCP
            I=KLCMLX-KLINT
            IPCE=IPCE+I
            IF(IPCE.GT.LAST)GO TO 727
*               --------
            CALL CCOPYA(KOD(IPCP),KOD(IPCP+I),L)
            CALL CCOPYA(CX,KOD(IPCP-1),KLCMLX)
            KOD(IPCP-2)=KLCXK
            IPCP=IPCP+I
            ISEM(ITS-2)=KEY
            RETURN
          ENDIF
        ENDIF
        GO TO 1210
 1208   KODT=KR2CX
        KEY=7
        IF(IB1.EQ.2)THEN
          IF(IB2.EQ.2.OR.IB2.EQ.3)THEN
            CX=RA(IPCP-1)
            L=IPCE-IPCP
            I=KLCMLX-KLREAL
            IPCE=IPCE+I
            IF(IPCE.GT.LAST)GO TO 727
*               --------
            CALL CCOPYA(KOD(IPCP),KOD(IPCP+I),L)
            CALL CCOPYA(CX,KOD(IPCP-1),KLCMLX)
            KOD(IPCP-2)=KLCXK
            IPCP=IPCP+I
            ISEM(ITS-2)=KEY
            RETURN
          ENDIF
        ENDIF
        GO TO 1210
 1209   KODT=KD2CX
        KEY=7
        IF(IB1.EQ.2)THEN
          IF(IB2.EQ.2.OR.IB2.EQ.3)THEN
**!!!       triuk
            IEQD(1)=KOD(IPCP-2)
            IEQD(2)=KOD(IPCP-1)
            CX=CMPLX(D)
            CALL CCOPYA(CX,KOD(IPCP-2),KLCMLX)
            KOD(IPCP-3)=KLCXK
            ISEM(ITS-2)=KEY
            RETURN
          ENDIF
        ENDIF
        GO TO 1210
      ELSEIF(KEY.LT.0)THEN
        K=-KEY-7
        GO TO (1301,1302,1303,1304,1305,1306,1307,1308,1309),K
*               RI   LI    DI  HI   DR   HR   CXI  CXR  CXD
 1301   KODT=KIR
        KEY=2
        IF(IB2.EQ.2)THEN
          RA(IPCP+1)=KOD(IPCP+1)
          RETURN
        ENDIF
        GO TO 1210
 1302    KEY=1
         ISEM(ITS-2)=KEY
         RETURN
 1303   KODT=KITOD
        KEY=5
        IF(IB2.EQ.2)THEN
          D=KOD(IPCP+1)
          I=KDLEN-KLINT
          IPCE=IPCE+I
          IF(IPCE.GT.LAST)GO TO 727
*             ---------
          CALL CCOPYA(D,KOD(IPCP+1),KDLEN)
          KOD(IPCP)=KLDK
          RETURN
        ENDIF
        GO TO 1210
 1304    KEY=1
         ISEM(ITS-2)=KEY
         RETURN
 1305   KODT=KRTOD
        KEY=5
        IF(IB2.EQ.2)THEN
          D=RA(IPCP+1)
          I=KDLEN-KLREAL
          IPCE=IPCE+I
          IF(IPCE.GT.LAST)GO TO 727
*             ---------
          CALL CCOPYA(D,KOD(IPCP+1),KDLEN)
          KOD(IPCP)=KLDK
          RETURN
        ENDIF
        GO TO 1210
 1306   KEY=2
        ISEM(ITS-2)=KEY
        RETURN
 1307   KODT=KITOCX
        KEY=7
        IF(IB2.EQ.2)THEN
          CX=CMPLX(KOD(IPCP+1))
          I=KLCMLX-KLINT
          IPCE=IPCE+I
          IF(IPCE.GT.LAST)GO TO 727
*             ---------
          CALL CCOPYA(CX,KOD(IPCP+1),KLCMLX)
          KOD(IPCP)=KLCXK
          RETURN
        ENDIF
        GO TO 1210
 1308   KODT=KRTOCX
        KEY=7
        IF(IB2.EQ.2)THEN
          CX=CMPLX(RA(IPCP+1))
          I=KLCMLX-KLREAL
          IPCE=IPCE+I
          IF(IPCE.GT.LAST)GO TO 727
*             ---------
          CALL CCOPYA(CX,KOD(IPCP+1),KLCMLX)
          KOD(IPCP)=KLCXK
          RETURN
        ENDIF
        GO TO 1210
 1309   KODT=KDTOCX
        KEY=7
        IF(IB2.EQ.2)THEN
**!!!       triuk
          IEQD(1)=KOD(IPCP+1)
          IEQD(2)=KOD(IPCP+2)
          CX=CMPLX(D)
          I=KLCMLX-KDLEN
          IPCE=IPCE+I
          IF(IPCE.GT.LAST)GO TO 727
*             ---------
          CALL CCOPYA(CX,KOD(IPCP+1),KLCMLX)
          KOD(IPCP)=KLCXK
          RETURN
        ENDIF
        GO TO 1210
      ENDIF
* ADD CODE
 1210 IPCE=IPCE+1
      IF(IPCE.GT.LAST)GO TO 727
*         --------
      KOD(IPCE-1)=KODT
      RETURN
 726  IGSST=-26
      RETURN
 727  IGSST=-27
      END
