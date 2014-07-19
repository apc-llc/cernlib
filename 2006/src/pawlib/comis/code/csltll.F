*
* $Id: csltll.F,v 1.1.1.1 1996/02/26 17:16:17 mclareni Exp $
*
* $Log: csltll.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:17  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.36  by  Rene Brun
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSLTLL(IPVS)
***---------------------------
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
      IPVS=0
      LABLL=LAB
      I=IPLL
  1   IF(I.EQ.0)GO TO 3
      IF(IQ(I+1).EQ.LAB)GO TO 2
      IPVS=I
      I=IQ(I)
      GO TO 1
  2   CALL CCOPYA(IQ(I+1),LABLL,3)
  3   CSLTLL=I
      END
