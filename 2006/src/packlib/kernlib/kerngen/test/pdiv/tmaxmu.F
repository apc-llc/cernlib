*
* $Id: tmaxmu.F,v 1.1.1.1 1996/02/15 17:54:56 mclareni Exp $
*
* $Log: tmaxmu.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:56  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TMAXMU

#include "kerngent/mkcde.inc"

      DIMENSION CHECK(3)
      INTEGER TINF(4)
      DATA  CHECK / 100., 10., 50./
      DATA   TINF /1000, 4HAMAX, 50, 4H10*5/

      CALL NEWGUY ('AMAXMU.','TMAXMU  ')
      ZERLEV = ZEROV(1)
      I1=0

      DO 5 I=1,10
      II=I
      DO 5 J=1,10
      I1=I1+1
      A(I1)=INTG(II)
    5 II=II+10

      B(1)=AMAXMU (A,1,100,0)                                               100.
      B(2)=AMAXMU (A,10,1,10)                                                10.
      B(3)=AMAXMU (A,10,5,10)                                                50.
      CALL MVERIF (1,B,CHECK,3)

C--      TIMING
      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL TIMED (TIMERD)

      DO 80 J=1,NTIMES
   80 B(1)=AMAXMU (A,10,5,10)

      CALL TIMING (TINF)
      RETURN
      END
