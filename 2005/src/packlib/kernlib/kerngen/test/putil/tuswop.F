*
* $Id: tuswop.F,v 1.1.1.1 1996/02/15 17:54:58 mclareni Exp $
*
* $Log: tuswop.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:58  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TUSWOP

#include "kerngent/mkcde.inc"

      INTEGER TINF(4)
      DATA   TINF/1000,4HUSWP,100,4HWORD/

      CALL NEWGUY ('USWOP.','TUSWOP  ')
      CALL UZERO (A,1,100)
      CALL UZERO (B,1,100)
      CALL UCOPY (IBCD,A,47)
      CALL UCOPY (INTG,B,100)
      CALL USWOP (A,B,100)
      CALL USWOP (A(10),B(10),0)
      CALL MVERII (1,A,INTG,100)
      CALL MVERII (2,B,IBCD,47)

C--      TIMING
      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL TIMED (TIMERD)

      DO 80 J=1,NTIMES
   80 CALL USWOP (A,B,100)

      CALL TIMING (TINF)
      RETURN

      END
