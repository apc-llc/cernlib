*
* $Id: d110s2.F,v 1.1.1.1 1996/04/01 15:01:23 mclareni Exp $
*
* $Log: d110s2.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:23  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D110S2(M,X1,F1,X)
#include "gen/imp64.inc"
#include "iorc.inc"
      DIMENSION X1(M),F1(M),X(6)
      DO 900 I=1,M
         X(1)=X1(I)
         F1(I)=X(1)*SQRT(X(1)**2+X(2))
 900  CONTINUE
      RETURN
      END
C     ****** End of nested function evaluation for test number 1 ******

C     ****** Nested function evaluation for test number 2 ******
