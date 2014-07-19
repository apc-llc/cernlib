*
* $Id: d110s8.F,v 1.1.1.1 1996/04/01 15:01:23 mclareni Exp $
*
* $Log: d110s8.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:23  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D110S8(M,X1,F1,X)
#include "gen/imp64.inc"
#include "iorc.inc"
      DIMENSION X1(M),F1(M),X(6)

      SUM5=X(2)+X(3)+X(4)+X(5)+X(6)
      DO 1500 I=1,M
         X(1)=X1(I)
         F1(I)=X(1)+SUM5
1500  CONTINUE
      RETURN
C     ****** End of nested function evaluation for test number 2 ******
      END
