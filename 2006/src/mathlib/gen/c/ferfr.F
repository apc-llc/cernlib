*
* $Id: ferfr.F,v 1.1.1.1 1996/04/01 15:02:02 mclareni Exp $
*
* $Log: ferfr.F,v $
* Revision 1.1.1.1  1996/04/01 15:02:02  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if !defined(CERNLIB_DOUBLE)
      FUNCTION FERDR(X,K)
      SROUND(D)=D+(D-SNGL(D))

      FERDR=SROUND(DFERDR(DBLE(X),K))
      RETURN

      END
#endif
