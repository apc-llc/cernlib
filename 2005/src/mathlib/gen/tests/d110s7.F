*
* $Id: d110s7.F,v 1.1.1.1 1996/04/01 15:01:23 mclareni Exp $
*
* $Log: d110s7.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:23  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D110S7(M,X2,F2,X)
#include "gen/imp64.inc"
#include "iorc.inc"
      DIMENSION X2(M),F2(M),X(6)
      EXTERNAL D110S8

      DO 1400 I=1,M
         X(2)=X2(I)
#if defined(CERNLIB_DOUBLE)
         F2(I)=DGMLT1(D110S8,0D0,X2(I),2,6,X)
#endif
#if !defined(CERNLIB_DOUBLE)
         F2(I)=RGMLT1(D110S8,0.0,X2(I),2,6,X)
#endif
1400  CONTINUE
      RETURN
      END

