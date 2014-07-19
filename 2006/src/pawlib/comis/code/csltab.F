*
* $Id: csltab.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: csltab.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZU:  1.16/16 28/09/93  11.08.26  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSLTAB(IP,IPVS,OFFSET)
***------------------------------
      INTEGER OFFSET
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cspnts.inc"
      I=IP
      IPVS=0
   1  IF(I.EQ.0)GO TO 3
      IF(IQ(I+1).NE.NCIDEN)GO TO 2
      DO 11 K=1,NWIDEN
        IF(IQ(I+OFFSET+K).NE.IDEN(K))GO TO 2
   11 CONTINUE
      GO TO 3
   2  IPVS=I
      I=IQ(I)
      GO TO 1
   3  CSLTAB=I
      END
