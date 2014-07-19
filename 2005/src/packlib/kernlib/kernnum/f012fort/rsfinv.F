*
* $Id: rsfinv.F,v 1.1.1.1 1996/02/15 17:49:05 mclareni Exp $
*
* $Log: rsfinv.F,v $
* Revision 1.1.1.1  1996/02/15 17:49:05  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RSFINV(N,A,IDIM)
          REAL                A(IDIM,*),ZERO,     X, Y
          CHARACTER*6         HNAME
#if !defined(CERNLIB_NUMRDBLE)
          REAL                S31, S32, S33, DOTF
          DOTF(X,Y,S31)  =  X*Y + S31
#endif
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    S31, S32, S33, DOTF
          DOTF(X,Y,S31)  =  DBLE(X)*DBLE(Y) + S31
#endif
          DATA      HNAME               /  'RSFINV'  /
          DATA      ZERO      /  0.  /
          IF(IDIM .LT. N  .OR.  N .LE. 0)  GOTO 900
#include "sfinv.inc"
          RETURN
 900      CALL TMPRNT(HNAME,N,IDIM,0)
          RETURN
          END
