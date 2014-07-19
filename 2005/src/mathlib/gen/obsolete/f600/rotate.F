*
* $Id: rotate.F,v 1.1.1.1 1996/04/01 15:03:16 mclareni Exp $
*
* $Log: rotate.F,v $
* Revision 1.1.1.1  1996/04/01 15:03:16  mclareni
* Mathlib gen
*
*
#include "sys/CERNLIB_machine.h"
#include "_gen/pilot.h"
      SUBROUTINE ROTATE (X, Y, CS, SN, N)
      REAL X(N), Y (N), CS, SN
C
      DO 10 J = 1,N
      XX = X(J)
      X(J) = XX*CS + Y(J)*SN
   10 Y(J) = Y(J)*CS - XX*SN
      RETURN
C     --------------------  END OF SVD ---------------------------------
      END
