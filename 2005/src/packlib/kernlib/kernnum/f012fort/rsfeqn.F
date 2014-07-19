*
* $Id: rsfeqn.F,v 1.1.1.1 1996/02/15 17:49:04 mclareni Exp $
*
* $Log: rsfeqn.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:04  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RSFEQN(N,A,IDIM,K,B)
          REAL                A(IDIM,*),B(IDIM,*),X,Y
          CHARACTER*6         HNAME
#if !defined(CERNLIB_NUMRDBLE)
          REAL                S21, S22, DOTF
          DOTF(X,Y,S21)  =  X*Y + S21
#endif
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    S21, S22, DOTF
          DOTF(X,Y,S21)  =  DBLE(X)*DBLE(Y) + S21
#endif
          DATA      HNAME               /  'RSFEQN'  /
          IF(IDIM .LT. N  .OR.  N .LE. 0  .OR.  K .LE. 0)  GOTO 900
#include "sfeqn.inc"
          RETURN
 900      CALL TMPRNT(HNAME,N,IDIM,K)
          RETURN
          END
