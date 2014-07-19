*
* $Id: fun1.F,v 1.1.1.1 1996/01/11 14:14:37 mclareni Exp $
*
* $Log: fun1.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:37  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION FUN1(ALX)
C     ******************
C-- USED BY INPART
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/cfun1.inc"
#include "cojets/itapes.inc"
      X=EXP(ALX)
      FUN1=F2IN(X,IFUN)
      RETURN
      END
