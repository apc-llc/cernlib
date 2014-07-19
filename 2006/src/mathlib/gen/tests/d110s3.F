*
* $Id: d110s3.F,v 1.1.1.1 1996/04/01 15:01:23 mclareni Exp $
*
* $Log: d110s3.F,v $
* Revision 1.1.1.1  1996/04/01 15:01:23  mclareni
* Mathlib gen
*
*
#include "gen/pilot.h"
      SUBROUTINE D110S3(M,X6,F6,X)
#include "gen/imp64.inc"
#include "iorc.inc"
      DIMENSION X6(M),F6(M),X(6)
      EXTERNAL D110S4

      DO 1000 I=1,M
         X(6)=X6(I)
#if defined(CERNLIB_DOUBLE)
         F6(I)=DGMLT5(D110S4,0D0,X6(I),2,6,X)
#endif
#if !defined(CERNLIB_DOUBLE)
         F6(I)=RGMLT5(D110S4,0.0,X6(I),2,6,X)
#endif
1000  CONTINUE
      RETURN
      END

