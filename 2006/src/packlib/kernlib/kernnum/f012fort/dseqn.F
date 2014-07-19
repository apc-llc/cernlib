*
* $Id: dseqn.F,v 1.2 1999/09/08 08:05:00 mclareni Exp $
*
* $Log: dseqn.F,v $
* Revision 1.2  1999/09/08 08:05:00  mclareni
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
          SUBROUTINE          DSEQN(N,A,IDIM,IFAIL,K,B)
          DOUBLE PRECISION    A(IDIM,*), B(IDIM,*),  ONE,  X, Y
          DOUBLE PRECISION    S1, S21, S22,       DOTF
          CHARACTER*6         HNAME
          DATA      HNAME               /  'DSEQN '  /
          DOTF(X,Y,S1)  =  X * Y + S1
          DATA      ZERO, ONE           /  0.D0, 1.D0 /
          IF(IDIM .LT. N  .OR.  N .LE. 0  .OR.  K .LT. 0)  GOTO 900
#include "sfact.inc"
#include "sfeqn.inc"
          RETURN
 900      CALL TMPRNT(HNAME,N,IDIM,K)
          RETURN
          END
