*
* $Id: rvsum.F,v 1.1.1.1 1996/02/15 17:48:53 mclareni Exp $
*
* $Log: rvsum.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:53  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          REAL FUNCTION       RVSUM(N,X,X2)
          REAL                X(*), X2(*), SUM
          SUM  =  0.
          IF(N .LE. 0)  GOTO 20
#include "rxj.inc"
          LXJ  =  1
          DO 10     J  =  1, N
             SUM  =  SUM + X(LXJ)
             LXJ  =  LXJ + JX
  10         CONTINUE
  20      RVSUM  =  SUM
          RETURN
          END
