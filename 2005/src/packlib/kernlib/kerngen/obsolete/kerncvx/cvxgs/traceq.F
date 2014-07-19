*
* $Id: traceq.F,v 1.1.1.1 1996/02/15 17:52:29 mclareni Exp $
*
* $Log: traceq.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:29  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE TRACEQ (LUNP,LEV)
C
C CERN PROGLIB# N105    TRACEQ          .VERSION KERNCVX  1.00  890315
C ORIG. 01/01/89  AGA+MAV, Convex
C

      CALL TRACEBACK
      RETURN
      END
#ifdef CERNLIB_TCGEN_TRACEQ
#undef CERNLIB_TCGEN_TRACEQ
#endif
