*
* $Id: wpardn.F,v 1.1.1.1 1996/01/11 14:14:44 mclareni Exp $
*
* $Log: wpardn.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:44  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION WPARDN(X)
C     ******************
C-- CALLED BY BKWEIG -- INITIAL WEIGHT ON PARTON DENSITIES
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      WPARDN=1.
      RETURN
      END
