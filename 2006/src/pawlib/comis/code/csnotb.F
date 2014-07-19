*
* $Id: csnotb.F,v 1.1.1.1 1996/02/26 17:16:24 mclareni Exp $
*
* $Log: csnotb.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:24  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/04 22/06/94  16.09.15  by  Rene Brun
*-- Author : V.Berezhnoi
      CHARACTER*1 FUNCTION CSNOTB(STR,I,N)
      CHARACTER*(*) STR,CH*1
 1    IF(I.GT.N)GO TO 2
      CH=STR(I:I)
      I=I+1
      IF(CH.EQ.' ')GO TO 1
      CALL CLTOU(CH)
      CSNOTB=CH
      RETURN
 2    CSNOTB=' '
      END
