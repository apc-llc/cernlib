*
* $Id: dsfeqn.F,v 1.1.1.1 1996/02/15 17:49:04 mclareni Exp $
*
* $Log: dsfeqn.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:04  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          DSFEQN(N,A,IDIM,K,B)
          DOUBLE PRECISION    A(IDIM,*),B(IDIM,*),X,Y
          DOUBLE PRECISION    S21, S22, DOTF
          CHARACTER*6         HNAME
          DATA      HNAME               /  'DSFEQN'  /
          DOTF(X,Y,S21)  =  X*Y + S21
          IF(IDIM .LT. N  .OR.  N .LE. 0  .OR.  K .LE. 0)  GOTO 900
#include "sfeqn.inc"
          RETURN
 900      CALL TMPRNT(HNAME,N,IDIM,K)
          RETURN
          END
