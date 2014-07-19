*
* $Id: dsfact.F,v 1.2 1999/09/08 08:05:07 mclareni Exp $
*
* $Log: dsfact.F,v $
* Revision 1.2  1999/09/08 08:05:07  mclareni
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
          SUBROUTINE          DSFACT(N,A,IDIM,IFAIL,DET,JFAIL)
          DOUBLE PRECISION    A(IDIM,*),DET,      ZERO,     ONE,X,Y
          REAL                G1,       G2
          REAL                SIZEF,  T
          DOUBLE PRECISION    S1, DOTF
          CHARACTER*6         HNAME
          SIZEF(X)     =  ABS(SNGL(X))
          DOTF(X,Y,S1)  =  X * Y + S1
#if defined(CERNLIB_NUME2465)
          DATA      G1, G2              /  1.E-1232, 1.E1232  /
#endif
#if defined(CERNLIB_NUME279)
          DATA      G1, G2              /  1.E-139, 1.E139  /
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
          DATA      HNAME               /  'DSFACT'  /
          DATA      ZERO, ONE           /  0.D0, 1.D0  /
          IF(IDIM .LT. N  .OR.  N .LE. 0)  GOTO 900
#include "sfactd.inc"
          RETURN
 900      CALL TMPRNT(HNAME,N,IDIM,0)
          RETURN
          END
