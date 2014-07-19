*
* $Id: traceq.F,v 1.1.1.1 1996/02/15 17:52:38 mclareni Exp $
*
* $Log: traceq.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:38  mclareni
* Kernlib
*
*
#include "kerncry/pilot.h"
      SUBROUTINE TRACEQ (LUNP,LEV)
C
C CERN PROGLIB# N105    TRACEQ          .VERSION KERNCRY  1.07  880428
C ORIG. 12/06/83 JZ
C

      LUN = LUNP
#if defined(CERNLIB_QMCRU)
      IF (LUN.EQ.0)  LUN=6
#endif
#if !defined(CERNLIB_QMCRU)
      IF (LUN.EQ.0)  LUN=4L$OUT
#endif
      CALL TRBK (LUN)
      RETURN
      END
#ifdef CERNLIB_TCGEN_TRACEQ
#undef CERNLIB_TCGEN_TRACEQ
#endif
