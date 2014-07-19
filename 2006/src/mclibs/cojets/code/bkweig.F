*
* $Id: bkweig.F,v 1.1.1.1 1996/01/11 14:14:32 mclareni Exp $
*
* $Log: bkweig.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:32  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION BKWEIG(X)
C     ******************
C-- ORGANIZATION OF BLOCK WEIGHT ON PARTON DENSITIES (IN PRETABULATION)
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      XIN=AINT(X/.1)*.1+.05
      BKWEIG=WPARDN(XIN)
      RETURN
      END
