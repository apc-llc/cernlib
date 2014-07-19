*
* $Id: traceq.F,v 1.1.1.1 1996/02/15 17:53:23 mclareni Exp $
*
* $Log: traceq.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:23  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE TRACEQ (LUNP,NP)
C
C CERN PROGLIB# N105    TRACEQ          .VERSION KERNIBM  2.28  891004
C ORIG. 12/09/89 JZ
C
C-    SUBROUTINE TO PRINT THE FORTRAN SUBROUTINE TRACE-BACK
C
      CALL ERRTRA
      RETURN
      END
#ifdef CERNLIB_TCGEN_TRACEQ
#undef CERNLIB_TCGEN_TRACEQ
#endif
