*
* $Id: cjrn.F,v 1.1.1.1 1996/01/11 14:14:34 mclareni Exp $
*
* $Log: cjrn.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:34  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION CJRN(A)
C     ****************
C-- UNIFORM RANDOM NUMBER GENERATOR
C-- N.B. CJRN SHOULD BE  0. < CJRN < 1.
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL RANF
#endif
#include "cojets/itapes.inc"
1     CJRN=RANF()
      IF(CJRN.LE.0.OR.CJRN.GE.1.) GO TO 1
      RETURN
      END
