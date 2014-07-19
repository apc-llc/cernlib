*
* $Id: fmuldg.F,v 1.1.1.1 1996/01/11 14:14:37 mclareni Exp $
*
* $Log: fmuldg.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:37  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION FMULDG(Z1)
C     *******************
C--  KNO (DE GROOT) MULTIPLICITY FUNCTION
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      REAL GAMMA,Z
      Z=Z1
      FMULDG=1.333*Z/(GAMMA(Z+1))**2*EXP(-.892*Z)*2.5
      RETURN
      END
