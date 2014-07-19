*
* $Id: fun3.F,v 1.1.1.1 1996/01/11 14:14:37 mclareni Exp $
*
* $Log: fun3.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:37  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION FUN3(ALX)
C     ******************
C-- USED BY INPART
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      X=EXP(ALX)
      FUN3=F2IN(X,7)
      RETURN
      END
