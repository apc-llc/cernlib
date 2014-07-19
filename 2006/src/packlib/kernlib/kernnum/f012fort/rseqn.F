*
* $Id: rseqn.F,v 1.2 1999/09/08 08:05:14 mclareni Exp $
*
* $Log: rseqn.F,v $
* Revision 1.2  1999/09/08 08:05:14  mclareni
* A problem was reported in DSINV which failed on very small numbers, probably
* due to converting to single before a test. The conversion has been removed here
* and also in DSFACT. This resulted in mods to sfact.inc and sfactd.inc which
* meant that some other routines had to be tidied also.
*
* Revision 1.1.1.1  1996/02/15 17:49:05  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RSEQN(N,A,IDIM,IFAIL,K,B)
          REAL                A(IDIM,*), B(IDIM,*), ONE, X, Y
          CHARACTER*6         HNAME
#if !defined(CERNLIB_NUMRDBLE)
          REAL                S1, S21, S22,     DOTF
          DOTF(X,Y,S1)  =  X*Y + S1
#endif
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    S1, S21, S22,     DOTF
          DOTF(X,Y,S1)  =  DBLE(X)*DBLE(Y) + S1
#endif
          DATA      HNAME               /  'RSEQN '  /
          DATA      ZERO, ONE           /  0., 1. /
          IF(IDIM .LT. N  .OR.  N .LE. 0  .OR.  K .LT. 0)  GOTO 900
#include "sfact.inc"
#include "sfeqn.inc"
          RETURN
 900      CALL TMPRNT(HNAME,N,IDIM,K)
          RETURN
          END
