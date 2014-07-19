*
* $Id: iton.F,v 1.1.1.1 1996/04/01 15:03:02 mclareni Exp $
*
* $Log: iton.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:02  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
#if defined(CERNLIB_CDC)
      FUNCTION ITON(IB,N)
C   COMPUTE ITON=IB**N WITHOUT USING EXPONENTIATION OR MULTIPLICATION
C   THUS PRESERVING 60-BIT INTEGERS ON CDC
      ITON=0
      IF(IB.EQ.0) RETURN
      IF(N) 1,2,3
    1 IF(ABS(IB).GT.1) RETURN
      ITON=IB**N
      RETURN
    2 ITON=1
      RETURN
    3 ITON=1
      DO 4 I=1,N
    4 ITON=INTP(ITON,IB)
      RETURN
      END
#endif
