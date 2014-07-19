*
* $Id: csdpow.F,v 1.1.1.1 1996/02/26 17:16:21 mclareni Exp $
*
* $Log: csdpow.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:21  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/05 14/08/93  15.27.18  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
       SUBROUTINE CSDPOW(IPCE)
***--------------------------------------------
*       translater's routine
***--------------------------------------------
      PARAMETER(KIIPW=39, KRIPW=40, KRRPW=41,KIRPW=64, KIDPW=65,
     +  KRDPW=66,KDDPW=133, KDIPW=134, KDRPW=135, KCXPW=179,
     +  KCXIPW=180, KCXRPW=181, KCXDPW=182, KICXPW=197, KRCXPW=198,
     +  KDCXPW=199 )
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/csdpvs.inc"
      PARAMETER (  KTINT=1,   KLINT=1, KTREAL=2,  KLREAL=1,
     +  KTCHAR=3,  KTLOG=4,   KLLOG=1, KTDOU=5 ,  KLCMLX=2)
      COMMON/CSGSCM/IGSST,JGSST,NGSST,CSJUNK(3)
#include "comis/cstvrs.inc"
      ITS=ITS-2
      IT1=ISEM(ITS-2)
      IB1=ISEM(ITS-1)
      IT2=ISEM(ITS)
      IB2=ISEM(ITS+1)
      IF(IB1.GT.5.OR.IB2.GT.5)GO TO 726
*                             ---------
      IBEXP=1
      IF(IB1.EQ.2.AND.IB2.EQ.2)IBEXP=2
      KT=2
      IF(IT1.EQ.1)THEN
        IF(IT2.EQ.1)THEN
          IF(IBEXP.EQ.2)THEN
            I=IPCP-1
            KOD(I)=KOD(I)**KOD(IPCP+1)
            IPCE=IPCP
            IPCP=I-1
            RETURN
          ENDIF
          KODT=KIIPW
          KT=1
        ELSEIF(IT2.EQ.2)THEN
          KODT=KIRPW
        ELSEIF(IT2.EQ.5)THEN
          KODT=KIDPW
          KT=5
        ELSEIF(IT2.EQ.7)THEN
          KODT=KICXPW
          KT=7
        ELSE
          GO TO 726
*         ---------
        ENDIF
      ELSEIF(IT1.EQ.2)THEN
        IF(IT2.EQ.1)THEN
          IF(IBEXP.EQ.2)THEN
            I=IPCP-1
            RA(I)=RA(I)**KOD(IPCP+1)
            IPCE=IPCP
            IPCP=I-1
            RETURN
          ENDIF
          KODT=KRIPW
        ELSEIF(IT2.EQ.2)THEN
          KODT=KRRPW
        ELSEIF(IT2.EQ.5)THEN
          KODT=KRDPW
          KT=5
        ELSEIF(IT2.EQ.7)THEN
          KODT=KRCXPW
          KT=7
        ELSE
          GO TO 726
*         ---------
        ENDIF
      ELSEIF(IT1.EQ.5)THEN
        KT=5
        IF(IT2.EQ.1)THEN
          IF(IBEXP.EQ.2)THEN
            I=IPCP-KDLEN
            CALL CCOPYA(KOD(I),D,KDLEN)
            D=D**KOD(IPCP+1)
            CALL CCOPYA(D,KOD(I),KDLEN)
            IPCE=IPCP
            IPCP=I-1
            RETURN
          ENDIF
          KODT=KDIPW
        ELSEIF(IT2.EQ.2)THEN
          KODT=KDRPW
        ELSEIF(IT2.EQ.5)THEN
          KODT=KDDPW
        ELSEIF(IT2.EQ.7)THEN
          KODT=KDCXPW
          KT=7
        ELSE
          GO TO 726
*         ---------
        ENDIF
      ELSEIF(IT1.EQ.7)THEN
        KT=7
        IF(IT2.EQ.1)THEN
          IF(IBEXP.EQ.2)THEN
            I=IPCP-KLCMLX
            CALL CCOPYA(KOD(I),CX,KLCMLX)
            CX=CX**KOD(IPCP+1)
            CALL CCOPYA(CX,KOD(I),KLCMLX)
            IPCE=IPCP
            IPCP=I-1
            RETURN
          ENDIF
          KODT=KCXIPW
        ELSEIF(IT2.EQ.2)THEN
          KODT=KCXRPW
        ELSEIF(IT2.EQ.5)THEN
          KODT=KCXDPW
        ELSEIF(IT2.EQ.7)THEN
          KODT=KCXPW
        ELSE
          GO TO 726
*         ---------
        ENDIF
      ENDIF
      IPCE=IPCE+1
      IF(IPCE.GT.LAST)GO TO 727
*                    ----------
      KOD(IPCE-1)=KODT
      ISEM(ITS-2)=KT
      ISEM(ITS-1)=1
      RETURN
 726  IGSST=-26
      RETURN
 727  IGSST=-27
      END




