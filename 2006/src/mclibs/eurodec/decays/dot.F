*
* $Id: dot.F,v 1.1.1.1 1996/03/08 16:58:53 mclareni Exp $
*
* $Log: dot.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:53  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION DOT(I1,I2)
C.----------------------------------------------------------------------
C.
C.    DOT = PHAD(*,I1) . PHAD(*,I2)
C.    LAST UPDATE: 10/04/88
C.
C.----------------------------------------------------------------------
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION DPHAD1(4),DPHAD2(4),DDOT
#endif
#if !defined(CERNLIB_DOUBLE)
      DIMENSION DPHAD1(4),DPHAD2(4)
#endif
#include "eurodec/result.inc"
      DO 10 J=1,4
        DPHAD1(J)=PTEIL(J,I1)
   10 DPHAD2(J)=PTEIL(J,I2)
      DDOT=DPHAD1(4)*DPHAD2(4)-DPHAD1(1)*DPHAD2(1)-DPHAD1(2)*DPHAD2(2)
     &-DPHAD1(3)*DPHAD2(3)
      DOT=DDOT
      RETURN
      END
