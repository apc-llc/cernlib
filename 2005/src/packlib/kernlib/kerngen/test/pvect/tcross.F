*
* $Id: tcross.F,v 1.1.1.1 1996/02/15 17:54:58 mclareni Exp $
*
* $Log: tcross.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:58  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TCROSS

#include "kerngent/mkcde.inc"

      DIMENSION X(3),Y(3),CHECK(9)
      INTEGER TINF(4)

      DATA  X/5., 2., 9./,  Y/7., 4., 3./
      DATA  CHECK/-30., 48., 6.,   30., -48., -6.,   3*0./
      DATA  TINF/10000,4HCROS,0,4H    /

      CALL NEWGUY ('CROSS.','TCROSS  ')
      ZERLEV = ZEROV(2)

      CALL CROSS (X,Y,B)
      CALL CROSS (Y,X,B(4))
      CALL CROSS (X,X,B(7))
      CALL MVERIF (1,B,CHECK,9)

C--      TIMING
      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL TIMED (TIMERD)
      DO 80 J=1,NTIMES
   80 CALL CROSS (X,Y,B)
      CALL TIMING (TINF)
      RETURN

      END
