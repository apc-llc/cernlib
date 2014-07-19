*
* $Id: dzdinb.F,v 1.1.1.1 1996/03/04 16:13:09 mclareni Exp $
*
* $Log: dzdinb.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:09  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      LOGICAL FUNCTION DZDINB(PX,PY, X0,X1,Y0,Y1)
*.
*...DZDINB look if PX, PY are in a box
*.
*. INPUT     : PX,PY       coordinates to be checked
*.             X0,X1,Y0,Y1 coordinates of box
*. OUTPUT    : DZDINB = .TRUE. if PX,PY are in box
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.**********************************************************************
*.
      IF(PX .GE. X0 .AND. PX .LE. X1
     &  .AND. PY .GE. Y0 .AND. PY .LE. Y1)THEN
        DZDINB = .TRUE.
      ELSE
        DZDINB = .FALSE.
      ENDIF
      END
************************************************************************
