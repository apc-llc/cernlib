*
* $Id: timex.F,v 1.1.1.1 1996/02/15 17:52:38 mclareni Exp $
*
* $Log: timex.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:38  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE TIMEX (T)
C
C CERN PROGLIB# Z007    TIMEX           .VERSION KERNCRY  1.03  840215
C ORIG. 18/01/84  W.W. SACLAY/DPHPE
C
      T = SECOND ()
      RETURN
      END
#ifdef CERNLIB_CCGEN_TIMEX
#undef CERNLIB_CCGEN_TIMEX
#endif
#ifdef CERNLIB_TCGEN_TIMEX
#undef CERNLIB_TCGEN_TIMEX
#endif
