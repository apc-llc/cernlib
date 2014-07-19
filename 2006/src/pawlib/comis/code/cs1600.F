*
* $Id: cs1600.F,v 1.1.1.1 1996/02/26 17:16:22 mclareni Exp $
*
* $Log: cs1600.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/05 16/08/93  10.31.39  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CS1600(IPCE)
***--------------------------------------------
*       translater's routine
***--------------------------------------------
        PARAMETER (KIR=27,     KRI=28,     KLK=45,
     + KITOD=123,  KRTOD=124,  KDTOI=125,  KDTOR=126,
     + KLDK=137,   KITOCX=166, KRTOCX=167, KDTOCX=168,
     + KCXTOI=169, KCXTOR=170, KCXTOD=171, KLCXK=184)
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
      INTEGER IEQD(2)
      EQUIVALENCE (D,IEQD)
      IF(KEY.GT.0)THEN
        IF(KEY.EQ.4)KEY=1
        IF(KEY.LT.8)RETURN
        K=KEY-7
        GO TO (1601,1602,1603,1604,1605,1604,1607,1608,1609),K
*               IR   IL   ID   IH   RD   RH   ICX  RCX  DCX
*IR
 1601   KEY=1
        IT2=1
        KODT=KRI
        IF(IB2.EQ.2)THEN
        CALL CCOPYA(KOD(IPCP+1),RNUM,KLREAL)
        KOD(IPCP+1)=RNUM
        RETURN
      ENDIF
      GO TO 1620
*IL
 1602 KEY=1
      RETURN
*ID
 1603 KEY=1
      IT2=1
      KODT=KDTOI
      IF(IB2.EQ.2)THEN
        KOD(IPCP)=KLK
        CALL CCOPYA(KOD(IPCP+1),D,KDLEN)
        KOD(IPCP+1)=D
        IPCE=IPCP+2
        RETURN
      ENDIF
      GO TO 1620
*IH;RH
 1604 KEY=6
      RETURN
*RD
 1605 KEY=2
      IT2=2
      KODT=KDTOR
      IF(IB2.EQ.2)THEN
        KOD(IPCP)=KLK
        CALL CCOPYA(KOD(IPCP+1),D,KDLEN)
        RNUM=D
        CALL CCOPYA(RNUM,KOD(IPCP+1),KLREAL)
        IPCE=IPCP+2
        RETURN
      ENDIF
      GO TO 1620
*ICX
 1607 KEY=1
      IT2=1
      KODT=KCXTOI
      IF(IB2.EQ.2)THEN
        KOD(IPCP)=KLK
        CALL CCOPYA(KOD(IPCP+1),CX,KLCMLX)
        KOD(IPCP+1)=CX
        IPCE=IPCP+2
        RETURN
      ENDIF
      GO TO 1620
*RCX
 1608 KEY=2
      IT2=2
      KODT=KCXTOR
      IF(IB2.EQ.2)THEN
        KOD(IPCP)=KLK
        CALL CCOPYA(KOD(IPCP+1),CX,KLCMLX)
        RNUM=CX
        RA(IPCP+1)=RNUM
        IPCE=IPCP+2
        RETURN
      ENDIF
      GO TO 1620
*DCX
 1609 KEY=5
      IT2=5
      KODT=KCXTOD
      IF(IB2.EQ.2)THEN
        KOD(IPCP)=KLDK
        CALL CCOPYA(KOD(IPCP+1),CX,KLCMLX)
        D=CX
        CALL CCOPYA(D,KOD(IPCP+1),KDLEN)
        IPCE=IPCP+1+KDLEN
        RETURN
      ENDIF
        GO TO 1620
      ELSEIF(KEY.LT.0)THEN
        K=-KEY-7
      GO TO (1611,1612,1613,709,1615,709, 1617,1618,1619),K
*             RI   LI   DI  H=I? DR  H=R?  CXI  CXR  CXD
*RI
 1611 KEY=2
      IT2=2
      KODT=KIR
      IF(IB2.EQ.2)THEN
        RNUM=KOD(IPCP+1)
        CALL CCOPYA(RNUM,KOD(IPCP+1),KLREAL)
        RETURN
      ENDIF
      GO TO 1620
*LI
 1612 KEY=1
      RETURN
*DI
 1613 KEY=5
      IT2=5
      KODT=KITOD
      IF(IB2.EQ.2)THEN
        KOD(IPCP)=KLDK
        D=KOD(IPCP+1)
        I=KDLEN-KLINT
        IF(IPCE+I.GT.LAST)GOTO 727
*                         --------
        CALL CCOPYA(D,KOD(IPCP+1),KDLEN)
        IPCE=IPCE+I
        RETURN
        ENDIF
        GO TO 1620
*DR
 1615 KEY=5
      IT2=5
      KODT=KRTOD
      IF(IB2.EQ.2)THEN
        KOD(IPCP)=KLDK
        D=RA(IPCP+1)
        I=KDLEN-KLREAL
        IF(IPCE+I.GT.LAST)GOTO 727
*                         --------
        CALL CCOPYA(D,KOD(IPCP+1),KDLEN)
        IPCE=IPCE+I
        RETURN
        ENDIF
        GO TO 1620
*CXI
 1617 KEY=7
      IT2=7
      KODT=KITOCX
      IF(IB2.EQ.2)THEN
        KOD(IPCP)=KLCXK
        CX=CMPLX(KOD(IPCP+1))
        I=KLCMLX-KLINT
        IF(IPCE+I.GT.LAST)GOTO 727
*                         --------
        CALL CCOPYA(CX,KOD(IPCP+1),KLCMLX)
        IPCE=IPCE+I
        RETURN
        ENDIF
        GO TO 1620
*CXR
 1618 KEY=7
      IT2=7
      KODT=KRTOCX
      IF(IB2.EQ.2)THEN
        KOD(IPCP)=KLCXK
        CX=CMPLX(RA(IPCP+1))
        I=KLCMLX-KLREAL
        IF(IPCE+I.GT.LAST)GOTO 727
*                         --------
        CALL CCOPYA(CX,RA(IPCP+1),KLCMLX)
        IPCE=IPCE+I
        RETURN
        ENDIF
        GO TO 1620
*CXD
 1619 KEY=7
      IT2=7
      KODT=KDTOCX
      IF(IB2.EQ.2)THEN
**!!!       triuk
        IEQD(1)=KOD(IPCP+1)
        IEQD(2)=KOD(IPCP+2)
        CX=CMPLX(D)
        I=KLCMLX-KDLEN
        IPCE=IPCE+I
        IF(IPCE.GT.LAST)GO TO 727
*                       ---------
        CALL CCOPYA(CX,KOD(IPCP+1),KLCMLX)
        KOD(IPCP)=KLCXK
        RETURN
      ENDIF
        GO TO 1620
      ENDIF
*ADD COMMAND
 1620 IPCE=IPCE+1
      IF(IPCE.GT.LAST)GO TO 727
*                     ---------
      KOD(IPCE-1)=KODT
      RETURN
 727  IGSST=-27
      RETURN
 709  IGSST=-9
      END
