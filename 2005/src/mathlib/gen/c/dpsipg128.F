*
* $Id: dpsipg128.F,v 1.1.1.1 1996/04/01 15:02:00 mclareni Exp $
*
* $Log: dpsipg128.F,v $
* Revision 1.1.1.1  1996/04/01 15:02:00  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if !defined(CERNLIB_DOUBLE)
      FUNCTION DPSIPG(X,K)
C
#include "gen/imp128.inc"
      CHARACTER*(*) NAME
      PARAMETER(NAME='DPSIPG')
C
      DPSIPG=0
      CALL MTLPRT(NAME,'C316.0','Not available on this machine')
      RETURN
      END
#endif
