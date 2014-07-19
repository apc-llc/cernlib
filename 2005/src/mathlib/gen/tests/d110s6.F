*
* $Id: d110s6.F,v 1.1.1.1 1996/04/01 15:01:23 mclareni Exp $
*
* $Log: d110s6.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:23  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D110S6(M,X3,F3,X)
#include "gen/imp64.inc"
#include "iorc.inc"
      DIMENSION X3(M),F3(M),X(6)
      EXTERNAL D110S7

      DO 1300 I=1,M
         X(3)=X3(I)
#if defined(CERNLIB_DOUBLE)
         F3(I)=DGMLT2(D110S7,0D0,X3(I),2,6,X)
#endif
#if !defined(CERNLIB_DOUBLE)
         F3(I)=RGMLT2(D110S7,0.0,X3(I),2,6,X)
#endif
1300  CONTINUE
      RETURN
      END

