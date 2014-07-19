*
* $Id: csltgi.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: csltgi.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 05/10/93  15.12.00  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSLTGI(IPVS)
***-------------------------
      INTEGER CSLTAB
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstabps.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
      IF(NBB.EQ.0)THEN
**         CALL CCOPYA(IDEN(1),IDGI(1),2)
*         CALL VFILL(ISHGI,KSIDG-2,0)
         CALL VZERO(ISHGI,KSIDG-2)
         I=0
         IPVS=IPGI
      ELSE
         IP=IPGI
         I=CSLTAB(IP,IPVS,KSIDG-1)
         IF(I.NE.0)THEN
           CALL CCOPYA(IQ(I+2),ISHGI,KSIDG-2)
           IF(NBB.GT.0)THEN
             DO 2 K=1,NBB
              IF(NUMGI.EQ.IQ(IBB+K))GO TO 6
    2        CONTINUE
*             CALL VFILL(ISHGI,KSIDG-2,0)
             CALL VZERO(ISHGI,KSIDG-2)
    3        IF(I.GT.0)THEN
               IPVS=I
               I=IQ(I)
               GO TO 3
             ENDIF
           ENDIF
         ELSE
**           CALL CCOPYA(IDEN(1),IDGI(1),2)
*           CALL VFILL(ISHGI,KSIDG-2,0)
           CALL VZERO(ISHGI,KSIDG-2)
         ENDIF
      ENDIF
  6   CSLTGI=I
      END
