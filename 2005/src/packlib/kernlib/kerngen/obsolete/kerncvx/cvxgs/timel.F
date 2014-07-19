*
* $Id: timel.F,v 1.1.1.1 1996/02/15 17:52:28 mclareni Exp $
*
* $Log: timel.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:28  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      OPTIONS  -sa
      SUBROUTINE TIMEL (T)
C
C CERN PROGLIB# Z007    TIMEL           .VERSION KERNCVX  1.09  910815
C ORIG. 01/01/89  AGA+MAV, Convex
C
      REAL*4       TLARG

      CALL TIMELCC (TLARG)
      T = TLARG
      RETURN
      END
#ifdef CERNLIB_TCGEN_TIMEL
#undef CERNLIB_TCGEN_TIMEL
#endif
#ifdef CERNLIB_CCGEN_TIMEL
#undef CERNLIB_CCGEN_TIMEL
#endif
