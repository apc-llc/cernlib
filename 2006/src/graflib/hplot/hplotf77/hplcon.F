*
* $Id: hplcon.F,v 1.1.1.1 1996/01/19 10:49:58 mclareni Exp $
*
* $Log: hplcon.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.17/08 29/09/93  11.59.27  by  O.Couet
*-- Author :
      SUBROUTINE HPLCON(IDD,ICRES,ITYPE)
*.==========>
*.  Subroutine to draw a contour plot based on a two dimensional
*.  scatter plot from HBOOK
*.
*.  IDD   - ID of two dimensional histogram
*.  ICRES - resolution of contour plot (number of contour lines)
*.          minimum 2, maximum 50
*.  ITYPE = 0 - use colour to distinguish contours
*.        = 1 - use line style to distinguish contours
*.        = 2 - line style and colour are the same for all contours
*.
*..=========>
      DIMENSION PAR(2)
*._____________________________
*
      PAR(1)=FLOAT(ICRES)
      PAR(2)=FLOAT(ITYPE)
      CALL HPLTAB(IDD,2,PAR,'CONT')
*
      END
