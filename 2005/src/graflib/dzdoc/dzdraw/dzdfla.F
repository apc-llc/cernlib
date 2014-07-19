*
* $Id: dzdfla.F,v 1.1.1.1 1996/03/04 16:13:09 mclareni Exp $
*
* $Log: dzdfla.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:09  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDFLA(X,Y,AL,AA)
*.
*...DZDFLA  draw an arrow
*.
*. INPUT     :  X,Y  coordinates of arrow
*.              AL   length
*.              AA   angle
*. CALLS     : DZDGPL
*. CALLED    : DZDLI2
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.**********************************************************************
*.
*
      REAL XF(4),YF(4)
      PARAMETER (DEGRAD = 0.0174533)
*
      XF(1) = X
      YF(1) = Y
      XF(4) = X
      YF(4) = Y
      AN1=AA+20.
      AN1 = AN1*DEGRAD
      XF(2)=X-AL*COS(AN1)
      YF(2)=Y-AL*SIN(AN1)
*
      AN1=AA- 20.
      AN1 = AN1*DEGRAD
      XF(3)=X-AL*COS(AN1)
      YF(3)=Y-AL*SIN(AN1)
      CALL DZDGPL(4,XF,YF)
      END
************************************************************************
