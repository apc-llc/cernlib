*
* $Id: irnd01.F,v 1.1.1.1 1996/04/01 15:03:05 mclareni Exp $
*
* $Log: irnd01.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:05  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if defined(CERNLIB_IBM)
      FUNCTION IRND01(DUMMY)
C   GENERATES ONE RANDOM BIT WITH P(0)=P, P(1)=1-P
      DATA P/0.50/
      IRND01=0
      IF(RNDM2(DUMMY).GT.P) IRND01=1
      RETURN
      ENTRY IR01IN(SEED)
      CALL RD2IN(SEED,0)
      RETURN
      ENTRY IR01UT(SEED)
      CALL RD2OUT(SEED,JS)
      RETURN
      END
#endif
