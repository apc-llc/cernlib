*
* $Id: atan2x.F,v 1.1.1.1 1996/01/11 14:14:31 mclareni Exp $
*
* $Log: atan2x.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:31  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION ATAN2X(Y,X)
C     ********************
C-- CHECKS WITH X, Y BOUNDS IN ATAN2
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
      IF(X.NE.0..OR.Y.NE.0.) THEN
        ATAN2X=ATAN2(Y,X)
      ELSE
        ATAN2X=0.
      ENDIF
      RETURN
      END
