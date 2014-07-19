*
* $Id: dvsum.F,v 1.1.1.1 1996/02/15 17:48:53 mclareni Exp $
*
* $Log: dvsum.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:53  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          DOUBLE PRECISION FUNCTION DVSUM(N,X,X2)
          DOUBLE PRECISION    X(*), X2(*), SUM
          SUM  =  0.D0
          IF(N .LE. 0)  GOTO 20
#include "dxj.inc"
          LXJ  =  1
          DO 10     J  =  1, N
             SUM  =  SUM + X(LXJ)
             LXJ  =  LXJ + JX
  10         CONTINUE
  20      DVSUM  =  SUM
          RETURN
          END
