*
* $Id: betaeu.F,v 1.1.1.1 1996/01/11 14:14:31 mclareni Exp $
*
* $Log: betaeu.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:31  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION BETAEU(A1,B1)
C     **********************
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      REAL GAMMA,A,B
      A=A1
      B=B1
      BETAEU=GAMMA(A)*GAMMA(B)/GAMMA(A+B)
      RETURN
      END
