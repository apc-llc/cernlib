*
* $Id: timex.F,v 1.1.1.1 1996/02/15 17:54:52 mclareni Exp $
*
* $Log: timex.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:52  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      SUBROUTINE TIMEX (TXQT)
C
C CERN PROGLIB# Z007    TIMEX           .VERSION KERNNOR  1.16  821202
C
C-    EXECUTION TIME OF JOB
C
      INTEGER*4 TUSED
C
      ITIM = TUSED (DUMMY)
C          CONVERT FROM JIFFIES (20MS) TO SECONDS
      TXQT = FLOAT(ITIM)/50.
      END
#ifdef CERNLIB_TCGEN_TIMEX
#undef CERNLIB_TCGEN_TIMEX
#endif
