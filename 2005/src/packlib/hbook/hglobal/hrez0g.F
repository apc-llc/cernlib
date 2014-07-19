*
* $Id: hrez0g.F,v 1.1.1.1 1996/01/16 17:08:09 mclareni Exp $
*
* $Log: hrez0g.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:09  mclareni
* First import
*
*
#if defined(CERNLIB_VAX)
#include "hbook/pilot.h"
*CMZ :  4.10/05 21/02/89  16.14.55  by  Rene Brun
*-- Author :
      SUBROUTINE HREZ0G(IB,LB,JBANK)
*.==========>
*.           RESETS TO 0. DATA STRUCTURE POINTED BY JBANK
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
      DIMENSION IB(1),LB(1)
*.___________________________________________
      IF(JBANK.NE.0)THEN
         CALL VZERO(IB(JBANK+KNOENT),IB(JBANK-1)-1)
         LW=LB(JBANK)
         IF(LW.NE.0)THEN
            CALL VZERO(IB(LW+1),IB(LW-1))
            LN=LB(LW)
            IF(LN.NE.0)THEN
               CALL VZERO(IB(LN+1),IB(LN-1))
            ENDIF
         ENDIF
         JFUNC=LB(JBANK-1)
         IF(JFUNC.NE.0)CALL VZERO(IB(JFUNC+3),IB(JFUNC-1)-2)
      ENDIF
      END
#endif
