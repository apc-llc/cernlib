*
* $Id: rsfact.F,v 1.2 1999/09/08 08:05:15 mclareni Exp $
*
* $Log: rsfact.F,v $
* Revision 1.2  1999/09/08 08:05:15  mclareni
* A problem was reported in DSINV which failed on very small numbers, probably
* due to converting to single before a test. The conversion has been removed here
* and also in DSFACT. This resulted in mods to sfact.inc and sfactd.inc which
* meant that some other routines had to be tidied also.
*
* Revision 1.1.1.1  1996/02/15 17:49:04  mclareni
* Kernlib
*
*
#include "kernnum/pilot.h"
          SUBROUTINE          RSFACT(N,A,IDIM,IFAIL,DET,JFAIL)
          REAL                A(IDIM,*),DET,      ZERO,     ONE,X,Y
          REAL                G1,       G2
          REAL                SIZEF,  T
          CHARACTER*6         HNAME
#if !defined(CERNLIB_NUMRDBLE)
          REAL                S1,       DOTF
          DOTF(X,Y,S1)  =  X*Y + S1
#endif
#if defined(CERNLIB_NUMRDBLE)
          DOUBLE PRECISION    S1,       DOTF
          DOTF(X,Y,S1)  =  DBLE(X)*DBLE(Y) + S1
#endif
          SIZEF(X)     =  ABS(X)
#if defined(CERNLIB_NUME2465)
          DATA      G1, G2              /  1.E-1232, 1.E1232  /
#endif
#if defined(CERNLIB_NUME293)
          DATA      G1, G2              /  1.E-146, 1.E146  /
#endif
#if defined(CERNLIB_NUME75)
          DATA      G1, G2              /  1.E-37,  1.E37  /
#endif
#if defined(CERNLIB_NUME38)
          DATA      G1, G2              /  1.E-19,  1.E19  /
#endif
#if defined(CERNLIB_NUME999)
          DATA      ?????  G1, G2 NOT DEFINED  ?????
#endif
          DATA      HNAME               /  'RSFACT'  /
          DATA      ZERO, ONE           /  0., 1.  /
          IF(IDIM .LT. N  .OR.  N .LE. 0)  GOTO 900
#include "sfactd.inc"
          RETURN
 900      CALL TMPRNT(HNAME,N,IDIM,0)
          RETURN
          END
