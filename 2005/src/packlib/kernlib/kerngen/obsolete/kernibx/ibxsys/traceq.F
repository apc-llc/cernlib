*
* $Id: traceq.F,v 1.1.1.1 1996/02/15 17:54:45 mclareni Exp $
*
* $Log: traceq.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:45  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE TRACEQ (LUNP,NP)
C
C CERN PROGLIB# N105    TRACEQ          .VERSION KERNIBX  1.00  900101
C ORIG. 12/09/89 JZ
C
C-    SUBROUTINE TO PRINT THE FORTRAN SUBROUTINE TRACE-BACK
C
      CALL ERRTRA
      RETURN
      END
#ifdef CERNLIB_CCGEN_TRACEQ
#undef CERNLIB_CCGEN_TRACEQ
#endif
#ifdef CERNLIB_TCGEN_TRACEQ
#undef CERNLIB_TCGEN_TRACEQ
#endif
