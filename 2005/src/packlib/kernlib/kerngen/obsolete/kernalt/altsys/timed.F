*
* $Id: timed.F,v 1.1.1.1 1996/02/15 17:50:56 mclareni Exp $
*
* $Log: timed.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:56  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      SUBROUTINE TIMED (T)
C
C CERN PROGLIB# Z007    TIMED           .VERSION KERNALT  1.01  880222
C ORIG. 22/02/88, JZ
C
      DIMENSION    TT(2), TCALL(2), ITCALL(2)
      EQUIVALENCE (ITCALL,TCALL)
      SAVE         TCALL
      DATA  ITCALL / 0, 0 /
 
      TOT = DTIME (TT)
      IF (ITCALL(1).NE.0)   THEN
          T = TT(1) - TCALL(1)
          RETURN
        ENDIF
 
      TOT = DTIME (TCALL)
      RETURN
      END
#ifdef CERNLIB_CCGEN_TIMEL
#undef CERNLIB_CCGEN_TIMEL
#endif
#ifdef CERNLIB_TCGEN_TIMED
#undef CERNLIB_TCGEN_TIMED
#endif
