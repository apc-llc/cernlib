*
* $Id: fun2.F,v 1.1.1.1 1996/01/11 14:14:37 mclareni Exp $
*
* $Log: fun2.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:37  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION FUN2(X)
C     ****************
C-- USED BY FGLU
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/cfun1.inc"
#include "cojets/itapes.inc"
      FUN2=F2IN(X,IFUN)
      RETURN
      END
