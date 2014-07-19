*
* $Id: tuzero.F,v 1.1.1.1 1996/02/15 17:54:58 mclareni Exp $
*
* $Log: tuzero.F,v $
* Revision 1.1.1.1  1996/02/15 17:54:58  mclareni
* Kernlib
*
*
#include "kerngent/pilot.h"
      SUBROUTINE TUZERO

#include "kerngent/mkcde.inc"

      INTEGER CHECK(100)
      INTEGER TINF(4)

      DATA CHECK/6*0,3*4H    ,3*0,3*4H    ,0,3*4H    ,4*0,4H    ,0,-1
     +,          3*0,6*-1,5*0,5*-1,55*0/
      DATA   TINF /1000, 4HZERO, 1000, 4HWORD/

C ZERO   5 6       10 11 12          16          20 21 22 23    25 -----
C BLANK      7 8 9          13 14 15    17 18 19             24

C (-1)   26          30 31 32 33 34 35                41 42 43 44 45
C ZERO      27 28 29                   36 37 38 39 40                46

      CALL NEWGUY ('UZERO-UBLANK-UFILL-VZERO-VBLANK-VFILL.','TUZERO  ')
      CALL UZERO  (A,1,100)
      CALL UBLANK (A,7,19)
      CALL UZERO  (A,10,12)
      CALL UZERO  (A,16,16)
      CALL UBLANK (A,24,24)
      CALL MVERII (1,A,CHECK,25)

      CALL UCOPY (INTG,A,9)
      CALL VZERO (A,6)
      CALL VBLANK (A(7),3)
      CALL VZERO (A(8),0)
      CALL VBLANK (A(6),0)
      CALL MVERII (2,A,CHECK,20)

      CALL UFILL (A,30,35,-1)
      CALL UFILL (A,26,26,-1)
      CALL VFILL (A(28),0,-1)
      CALL VFILL (A(41),5,-1)
      CALL MVERII (3,A,CHECK,100)

C--      TIMING
      IF (ITIMES.EQ.0) RETURN
      NTIMES = ITIMES*TINF(1)
      TINF(1) = NTIMES
      CALL TIMED (TIMERD)

      DO 80 J=1,NTIMES
   80 CALL UZERO(A,1,1000)

      CALL TIMING (TINF)
      RETURN

      END
