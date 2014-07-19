*
* $Id: cscath.F,v 1.1.1.1 1996/02/26 17:16:29 mclareni Exp $
*
* $Log: cscath.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.16/07 19/08/93  17.54.13  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSCATH( I )
#include "comis/cspar.inc"
#include "comis/comis.inc"
#include "comis/csdpvs.inc"
#include "comis/cstvrs.inc"
      PARAMETER ( KIPLUS=31, KIMINU=33, KIMULT=35,  KIDIV=37)
      PARAMETER (  KTINT=1,   KLINT=1, KTREAL=2,  KLREAL=1,
     +            KTCHAR=3,   KTLOG=4, KLLOG=1,   KTDOU=5,
     +            KTHOLL=6,   KTCMLX=7,KLCMLX=2 )
      I=IPCP-1
      IF(KEY.EQ.1)THEN
        IF(KI.EQ.KIPLUS)THEN
          KOD(I)=KOD(I)+KOD(IPCP+1)
        ELSE IF(KI.EQ.KIMINU)THEN
          KOD(I)=KOD(I)-KOD(IPCP+1)
        ELSE IF(KI.EQ.KIMULT)THEN
          KOD(I)=KOD(I)*KOD(IPCP+1)
        ELSE IF(KI.EQ.KIDIV)THEN
          KOD(I)=KOD(I)/KOD(IPCP+1)
        END IF
      ELSEIF(KEY.EQ.2)THEN
        IF(KI.EQ.KIPLUS)THEN
          RA(I)=RA(I)+RA(IPCP+1)
        ELSE IF(KI.EQ.KIMINU)THEN
          RA(I)=RA(I)-RA(IPCP+1)
        ELSE IF(KI.EQ.KIMULT)THEN
          RA(I)=RA(I)*RA(IPCP+1)
        ELSE IF(KI.EQ.KIDIV)THEN
          RA(I)=RA(I)/RA(IPCP+1)
        END IF
      ELSEIF(KEY.EQ.5)THEN
        I=IPCP-KDLEN
        CALL CCOPYA(KOD(I),D,KDLEN)
        CALL CCOPYA(KOD(IPCP+1),D1,KDLEN)
        IF(KI.EQ.KIPLUS)THEN
          D=D+D1
        ELSE IF(KI.EQ.KIMINU)THEN
          D=D-D1
        ELSE IF(KI.EQ.KIMULT)THEN
          D=D*D1
        ELSE IF(KI.EQ.KIDIV)THEN
          D=D/D1
        END IF
        CALL CCOPYA(D,KOD(I),KDLEN)
      ELSEIF(KEY.EQ.7)THEN
        I=IPCP-KLCMLX
        CALL CCOPYA(KOD(I),CX,KLCMLX)
        CALL CCOPYA(KOD(IPCP+1),CX1,KLCMLX)
        IF(KI.EQ.KIPLUS)THEN
          CX=CX+CX1
        ELSE IF(KI.EQ.KIMINU)THEN
          CX=CX-CX1
        ELSE IF(KI.EQ.KIMULT)THEN
          CX=CX*CX1
        ELSE IF(KI.EQ.KIDIV)THEN
          CX=CX/CX1
        END IF
        CALL CCOPYA(CX,KOD(I),KLCMLX)
      ENDIF
      END
