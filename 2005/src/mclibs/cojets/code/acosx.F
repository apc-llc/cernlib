*
* $Id: acosx.F,v 1.1.1.1 1996/01/11 14:14:31 mclareni Exp $
*
* $Log: acosx.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:31  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      FUNCTION ACOSX(X)
C     *****************
C-- CHECKS WITH X BOUNDS IN ACOS
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
      DATA ICALL/0/
      IF(ICALL.GT.0) GO TO 1
      ICALL=1
      PI=4.*ATAN(1.)
1     CONTINUE
      IF(ABS(X).LT.1.) THEN
        ACOSX=ACOS(X)
      ELSEIF(X.GE.1.) THEN
        ACOSX=0.
      ELSE
        ACOSX=PI
      ENDIF
      RETURN
      END
