*
* $Id: traceq.F,v 1.1.1.1 1996/02/15 17:54:27 mclareni Exp $
*
* $Log: traceq.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:27  mclareni
* Kernlib
*
*
#include "kerndge/pilot.h"
      SUBROUTINE TRACEQ (LUN,LEV)
C
C CERN PROGLIB# N105    TRACEQ          .VERSION KERNDGE  1.01  860217
C ORIG. 03/02/86  JZ
C
 
      WRITE (LUN,9001)
      RETURN
 9001 FORMAT ('0TRACEQ.  IN-LINE TRACE-BACK STILL NOT AVAILABLE.')
      END
#ifdef CERNLIB_TCGEN_TRACEQ
#undef CERNLIB_TCGEN_TRACEQ
#endif
