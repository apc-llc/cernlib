*
* $Id: dzdrot.F,v 1.1.1.1 1996/03/04 16:13:15 mclareni Exp $
*
* $Log: dzdrot.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:15  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDROT(X0,Y0,X1,Y1,ANG)
*.
*...DZDROT rotate X0, Y0 counterclockwise around origin
*.
*. INPUT     :  X0, Y0   points to be rotated
*. OUTPUT    :  X1, Y1   the rotated points
*.              ANG      rotation angle (in degree)
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.
      PARAMETER (DEGRAD = 0.0174533)
      AN1 = ANG*DEGRAD
      XX = X0 * COS(AN1) - Y0 * SIN(AN1)
      YY = X0 * SIN(AN1) + Y0 * COS(AN1)
      X1=XX
      Y1=YY
      END
*
************************************************************************
