*
* $Id: trot.F,v 1.1.1.1 1996/02/15 17:54:58 mclareni Exp $
*
* $Log: trot.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:58  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TROT

#include "kerngent/mkcde.inc"

      DIMENSION X(3),CHECK(3)
      INTEGER TINF(4)

      DATA  X/100.,20.,10./
      DATA  CHECK/  89.6232448,  48.6587504,  10./
      DATA  TINF/10000,4HROT ,0,4H    /

      CALL NEWGUY ('ROT.','TROT    ')
      ZERLEV = ZEROV(3)

      CALL ROT (X,.3,B)
      CALL MVERIF (1,B,CHECK,3)

C--      TIMING
      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL TIMED (TIMERD)
      DO 80 J=1,NTIMES
   80 CALL ROT (X,.3,B)
      CALL TIMING (TINF)
      RETURN

      END
