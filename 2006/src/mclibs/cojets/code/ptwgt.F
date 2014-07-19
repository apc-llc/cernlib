*
* $Id: ptwgt.F,v 1.1.1.1 1996/01/11 14:14:41 mclareni Exp $
*
* $Log: ptwgt.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:41  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION PTWGT(QSQ)
C     *******************
C-- ORIGINAL QSQ (APPROX. EQ. TO PT**2 OF HARD PARTONS) PRETABULATION
C-- IS MULTIPLIED BY PTWGT(QSQ) IF IPTWGT=1
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/alqgen.inc"
#include "cojets/itapes.inc"
      PTWGT=QSQ**2+1.
      RETURN
      END
