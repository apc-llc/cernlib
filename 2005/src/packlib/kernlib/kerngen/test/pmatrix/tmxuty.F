*
* $Id: tmxuty.F,v 1.1.1.1 1996/02/15 17:54:57 mclareni Exp $
*
* $Log: tmxuty.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:57  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TMXUTY

#include "kerngent/mkcde.inc"

      DIMENSION CHECK(9)
      INTEGER TINF(4)

      DATA  CHECK/ 1., 0., 0., 0., 1., 0., 0., 0., 1./
      DATA  TINF/1000,4HXUTY,0,4H3X3 /

      ZERLEV=ZEROV(1)
      CALL NEWGUY ('MXUTY.','TMXUTY  ')

      CALL MXUTY (A,3)
      CALL MVERIF (1,A,CHECK,9)

C--      TIMING
      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL TIMED (TIMERD)

      DO 80 J=1,NTIMES
   80 CALL MXUTY (A,3)

      CALL TIMING (TINF)
      RETURN

      END
