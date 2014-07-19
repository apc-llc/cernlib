*
* $Id: dzdraw.F,v 1.1.1.1 1996/03/04 16:13:08 mclareni Exp $
*
* $Log: dzdraw.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:08  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDRAW(IXSTOR,LTOP,ILKACT
     & ,CHOPT)
*.
*...DZDRAW   draw a data structure on open workstations(s)
*.
*. INPUT     : IXSTOR   Store index
*.             LTOP     Link to top bank to be displayed
*.             ILKACT   the link where drawing of down banks starts
*.             CHOPT    Character option
*.                      'R' discriptor Rz-file available
*. OUTPUT    : none
*.
*. CALLS     : DZDRA1
*. CALLED    : DZDISP, USER
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.**********************************************************************
*.
      CHARACTER*(*) CHOPT
      LOGICAL REQINP
*---
      REQINP = .FALSE.
      CALL DZDRA1(IXSTOR,LTOP, ILKACT
     & ,CHOPT, XCUR, YCUR, REQINP,IFLCUR,YTOP,YBOTTM)
      RETURN
      END
*
************************************************************************
