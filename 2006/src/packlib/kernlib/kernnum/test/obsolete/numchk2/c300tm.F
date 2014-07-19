*
* $Id: c300tm.F,v 1.1.1.1 1996/02/15 17:48:39 mclareni Exp $
*
* $Log: c300tm.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:39  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
#if defined(CERNLIB_NEVER)
          SUBROUTINE C300TM(KNT,A,R)
          REAL      A(KNT), R(KNT)
#include "kernnumt/sysdat.inc"
          DATA      RANGE     / 2. /
          IF(KNTIME .GE. LMTIME) GOTO 80
          X = -RANGE/2.
          XINC = RANGE / FLOAT(KNT-1)
          DO 10  I = 1, KNT
             A(I) = X
             X = X + XINC
  10         CONTINUE
          CALL TIMEX(T0)
          DO 20 I = 1, KNT
             R(I) = ERF(A(I))
  20         CONTINUE
          CALL TIMEX(T1)
          T = (T1-T0) / FLOAT(KNT)
          KNTIME = KNTIME + 1
          WRITE( SPTIME(KNTIME), 8000 ) T
          RETURN
 
  80      WRITE(*,8001)
          RETURN
 
8000      FORMAT(12HC300 ERF    ,8X,1PE10.3)
8001      FORMAT( / 30H SPACE FOR TIMING INSUFFICIENT)
          END
#endif
