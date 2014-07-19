*
* $Id: cjeb.F,v 1.1.1.1 1996/01/11 14:14:33 mclareni Exp $
*
* $Log: cjeb.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:33  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE CJEB(X,TC)
C     *********************
C-- CHEBYSHEV POLINOMIALS
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      DIMENSION TC(6)
      X2=X*X
      X3=X*X2
      X4=X2*X2
      X5=X2*X3
      TC(1)=1.
      TC(3)=-1.+2.*X2
      TC(5)=1.-8.*X2+8.*X4
      TC(2)=X
      TC(4)=-3.*X+4.*X3
      TC(6)=5.*X-20.*X3+16.*X5
      RETURN
      END
