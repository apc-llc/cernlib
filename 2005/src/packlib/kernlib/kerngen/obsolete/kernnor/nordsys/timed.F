*
* $Id: timed.F,v 1.1.1.1 1996/02/15 17:54:52 mclareni Exp $
*
* $Log: timed.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:52  mclareni
* Kernlib
*
*
#include "kernnor/pilot.h"
      SUBROUTINE TIMED (DELTA)
C
C CERN PROGLIB# Z007    TIMED           .VERSION KERNNOR  1.16  821202
C
C-     TIME ELAPSED SINCE LAST CALL
C
      INTEGER*4 TUSED
      DATA LAST / 0 /
C
C
C         TUSED GIVES THE CPU TIME OF JOB IN JIFFIES (20MS)
      ITIM = TUSED (IDEL)
      IDEL = ITIM  - LAST
      LAST = ITIM
C     JIFFIES TO SECONDS
      DELTA = FLOAT(IDEL) / 50.
      END
#ifdef CERNLIB_TCGEN_TIMED
#undef CERNLIB_TCGEN_TIMED
#endif
