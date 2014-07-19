*
* $Id: timed.F,v 1.1.1.1 1996/02/15 17:52:38 mclareni Exp $
*
* $Log: timed.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:38  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE TIMED (T)
C
C CERN PROGLIB# Z007    TIMED           .VERSION KERNCRY  1.03  840215
C ORIG. 18/01/84  W.W. SACLAY/DPHPE
C
      REAL  LAST, NOW
      SAVE  LAST
      DATA  LAST / 0. /

      NOW  = SECOND ()
      T    = NOW - LAST
      LAST = NOW
      RETURN
      END
#ifdef CERNLIB_CCGEN_TIMED
#undef CERNLIB_CCGEN_TIMED
#endif
#ifdef CERNLIB_TCGEN_TIMED
#undef CERNLIB_TCGEN_TIMED
#endif
