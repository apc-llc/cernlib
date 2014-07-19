*
* $Id: dzdli2.F,v 1.1.1.1 1996/03/04 16:13:12 mclareni Exp $
*
* $Log: dzdli2.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:12  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDLI2(N,XP,YP,IATT)
*.
*...DZDLI2   draw a polyline with optionally an arrow at the end
*.
*. INPUT     :  N           number of coordinates (
*.              XP, YP      x-y values of polyline for (1..N-1)
*.              XP(N),YP(N) parameters for the arrow
*.
*. CALLS     : DZDFLA GPL
*. CALLED    : DZDBNK, DZDHEA, DZDLNK
*.
*. AUTHOR    : O.Schaile
*. VERSION   : 1.00
*. CREATED   : 11-Dec-87
*.********************************************************************
*.
#include "dzdprm.inc"
      REAL XP(2), YP(2)
      IF(N .LE. 1)GOTO 10
      IF(IFOMED.EQ.3)THEN
          N1=-N+1
          CALL DZDGPL(N1,XP,YP)
          GOTO 10
      ENDIF
      CALL DZDGPL(N-1,XP,YP)
      IF(XP(N) .GT. 1.E-10)THEN
        CALL DZDFLA(XP(N-1),YP(N-1),XP(N),YP(N))
      ENDIF
   10 CONTINUE
      END
***********************************************************************
