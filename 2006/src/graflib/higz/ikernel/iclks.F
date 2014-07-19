*
* $Id: iclks.F,v 1.2 1996/09/25 14:58:40 couet Exp $
*
* $Log: iclks.F,v $
* Revision 1.2  1996/09/25 14:58:40  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:55  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/09 23/08/94  09.34.58  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ICLKS
      ENTRY      GCLKS
#include "higz/hiatt.inc"
      DIMENSION ICLSWK(10)
      IEND = INOPWK
      DO 10 I=1,IEND
         ICLSWK(I) = ILOPWK(I)
   10 CONTINUE
      DO 20 I=1,IEND
         CALL ICLWK(ICLSWK(I))
   20 CONTINUE
#if defined(CERNLIB_X11)
      CALL IXCLSDS
#endif
#if defined(CERNLIB_MACMPW)
      CALL IMCLSDS
#endif
#if defined(CERNLIB_PHIGS)
      CALL PCLPH
#endif
      END
