*
* $Id: d110s5.F,v 1.1.1.1 1996/04/01 15:01:23 mclareni Exp $
*
* $Log: d110s5.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:23  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D110S5(M,X4,F4,X)
#include "gen/imp64.inc"
#include "iorc.inc"
      DIMENSION X4(M),F4(M),X(6)
      EXTERNAL D110S6

      DO 1200 I=1,M
         X(4)=X4(I)
#if defined(CERNLIB_DOUBLE)
         F4(I)=DGMLT3(D110S6,0D0,X4(I),2,6,X)
#endif
#if !defined(CERNLIB_DOUBLE)
         F4(I)=RGMLT3(D110S6,0.0,X4(I),2,6,X)
#endif
1200  CONTINUE
      RETURN
      END

