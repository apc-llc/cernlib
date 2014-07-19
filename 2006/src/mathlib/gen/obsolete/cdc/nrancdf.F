*
* $Id: nrancdf.F,v 1.1.1.1 1996/04/01 15:03:02 mclareni Exp $
*
* $Log: nrancdf.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:02  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if (defined(CERNLIB_CDC))&&(defined(CERNLIB_FORTRAN))
      SUBROUTINE NRAN(VECTOR,N)
      DIMENSION VECTOR(N)
      DO 100 I= 1, N
      VECTOR(I) = RNDM(I)
  100 CONTINUE
      RETURN
      ENTRY NRANIN
      CALL RDMIN(VECTOR)
      RETURN
      ENTRY NRANUT
      CALL RDMOUT(VECTOR)
      RETURN
      END
#endif
