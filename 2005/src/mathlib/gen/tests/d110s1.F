*
* $Id: d110s1.F,v 1.1.1.1 1996/04/01 15:01:23 mclareni Exp $
*
* $Log: d110s1.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:23  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D110S1(M,X2,F2,X)
#include "gen/imp64.inc"
#include "iorc.inc"
      DIMENSION X2(M),F2(M),X(6)
      EXTERNAL D110S2

      DO 800 I=1,M
         X(2)=X2(I)
         R=SQRT(X(2))
#if defined(CERNLIB_DOUBLE)
         F2(I)= R*EXP(X(2))*DGMLT1(D110S2,0D0,R,4,6,X)
#endif
#if !defined(CERNLIB_DOUBLE)
         F2(I)= R*EXP(X(2))*RGMLT1(D110S2,0.0,R,4,6,X)
#endif
 800  CONTINUE
      RETURN
      END

