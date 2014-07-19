*
* $Id: bxyz.F,v 1.1.1.1 1996/02/15 17:53:25 mclareni Exp $
*
* $Log: bxyz.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:25  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE BXYZ(B,X)
C
C
      DIMENSION B(3),X(3)
C
C
      B(3)=ZFIELD(X(1),X(2),X(3))
      R=SQRT(X(1)**2+X(2)**2)
      B(1)=0.
      B(2)=0.
      IF(R.EQ.0.)GOTO 100
      BR=RFIELD(X(1),X(2),X(3))/R
      B(1)=X(1)*BR
      B(2)=X(2)*BR
  100 RETURN
      END
