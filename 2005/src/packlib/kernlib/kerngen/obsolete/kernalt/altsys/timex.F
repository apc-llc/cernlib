*
* $Id: timex.F,v 1.1.1.1 1996/02/15 17:50:56 mclareni Exp $
*
* $Log: timex.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:56  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      SUBROUTINE TIMEX (T)
C
C CERN PROGLIB# Z007    TIMEX           .VERSION KERNALT  1.00  880212
C ORIG. 15/02/88, JZ
C
 
      DIMENSION    TT(2)
 
      TOT = ETIME (TT)
      T   = TT(1)
      RETURN
      END
#ifdef CERNLIB_CCGEN_TIMEL
#undef CERNLIB_CCGEN_TIMEL
#endif
#ifdef CERNLIB_TCGEN_TIMEX
#undef CERNLIB_TCGEN_TIMEX
#endif
