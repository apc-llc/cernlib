*
* $Id: dsfinv.F,v 1.1.1.1 1996/02/15 17:49:05 mclareni Exp $
*
* $Log: dsfinv.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:05  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DSFINV(N,A,IDIM)
          DOUBLE PRECISION    A(IDIM,*),ZERO,     X, Y
          DOUBLE PRECISION    S31, S32, S33, DOTF
          CHARACTER*6         HNAME
          DOTF(X,Y,S31)  =  X*Y + S31
          DATA      HNAME               /  'DSFINV'  /
          DATA      ZERO      /  0.D0  /
          IF(IDIM .LT. N  .OR.  N .LE. 0)  GOTO 900
#include "sfinv.inc"
          RETURN
 900      CALL TMPRNT(HNAME,N,IDIM,0)
          RETURN
          END
