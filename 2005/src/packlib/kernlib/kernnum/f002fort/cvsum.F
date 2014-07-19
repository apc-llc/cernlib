*
* $Id: cvsum.F,v 1.1.1.1 1996/02/15 17:48:53 mclareni Exp $
*
* $Log: cvsum.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:53  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          COMPLEX FUNCTION    CVSUM(N,X,X2)
          COMPLEX             X(*), X2(*), SUM
          SUM  =  (0.,0.)
          IF(N .LE. 0)  GOTO 20
#include "cxj.inc"
          LXJ  =  1
          DO 10     J  =  1, N
             SUM  =  SUM + X(LXJ)
             LXJ  =  LXJ + JX
  10         CONTINUE
  20      CVSUM  =  SUM
          RETURN
          END
