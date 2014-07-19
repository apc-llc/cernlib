*
* $Id: timelf.F,v 1.1.1.1 1996/01/11 14:14:43 mclareni Exp $
*
* $Log: timelf.F,v $
* Revision 1.1.1.1  1996/01/11 14:14:43  mclareni
* Cojets
*
*
#include "cojets/pilot.h"
      SUBROUTINE TIMELF(TLEFT)
C     ************************
C-- GIVES TIME LEFT FOR EXECUTION
#if defined(CERNLIB_SINGLE)
      IMPLICIT REAL (A-H,O-Z)
#endif
#if defined(CERNLIB_DOUBLE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#endif
#include "cojets/itapes.inc"
      REAL TLE
      CALL TIMEL(TLE)
      TLEFT=TLE
      RETURN
      END
