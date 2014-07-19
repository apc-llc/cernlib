*
* $Id: d110s4.F,v 1.1.1.1 1996/04/01 15:01:23 mclareni Exp $
*
* $Log: d110s4.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:23  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D110S4(M,X5,F5,X)
#include "gen/imp64.inc"
#include "iorc.inc"
      DIMENSION X5(M),F5(M),X(6)
      EXTERNAL D110S5

      DO 1100 I=1,M
         X(5)=X5(I)
#if defined(CERNLIB_DOUBLE)
         F5(I)=DGMLT4(D110S5,0D0,X5(I),2,6,X)
#endif
#if !defined(CERNLIB_DOUBLE)
         F5(I)=RGMLT4(D110S5,0.0,X5(I),2,6,X)
#endif
1100  CONTINUE
      RETURN
      END

