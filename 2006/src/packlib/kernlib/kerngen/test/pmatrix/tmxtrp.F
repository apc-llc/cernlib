*
* $Id: tmxtrp.F,v 1.1.1.1 1996/02/15 17:54:57 mclareni Exp $
*
* $Log: tmxtrp.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:57  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TMXTRP

#include "kerngent/mkcde.inc"

      DIMENSION ICHECK(6)
      INTEGER TINF(4)

      DATA  ICHECK/ 0, 2, 4, 1, 3, 5/
      DATA  TINF/1000,4HXTRP,0,4H3X2 /

      CALL NEWGUY ('MXTRP.','TMXTRP  ')
      ZERLEV = ZEROV(1)

      CALL UCOPY (INTG,IA(2),5)
      IA(1)=0

      CALL MXTRP (IA,IB,3,2)
      CALL MVERII (1,IB,ICHECK,6)

C--      TIMING
      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL TIMED (TIMERD)

      DO 80 J=1,NTIMES
   80 CALL MXTRP (IA,IB,3,2)

      CALL TIMING (TINF)
      RETURN

      END
