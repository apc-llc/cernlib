*
* $Id: iggrid.F,v 1.1.1.1 1996/02/14 13:10:24 mclareni Exp $
*
* $Log: iggrid.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:24  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/01 19/07/89  10.28.17  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGGRID(XG,YG)
*.===========>
*.
*. Align a point on the current defined grid.
*. This routine do nothing if no grid is defined.
*.
*..==========> (O.Couet)
#include "higz/higed.inc"
#include "higz/hiatt.inc"
*.______________________________________
*
      IF((IXSTEP*IXFACT.NE.0).AND.(IYSTEP*IYFACT.NE.0))THEN
         XGDX=(RWXMAX-RWXMIN)/FLOAT(IXSTEP*IXFACT)
         YGDY=(RWYMAX-RWYMIN)/FLOAT(IYSTEP*IYFACT)
         XG=FLOAT(INT((XG+(0.5*XGDX))/XGDX))*XGDX
         YG=FLOAT(INT((YG+(0.5*YGDY))/YGDY))*YGDY
      ENDIF
*
      END
