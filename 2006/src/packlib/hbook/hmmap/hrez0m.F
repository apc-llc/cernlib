*
* $Id: hrez0m.F,v 1.2 1997/02/25 14:09:18 couet Exp $
*
* $Log: hrez0m.F,v $
* Revision 1.2  1997/02/25 14:09:18  couet
* - The GRESET command didn't work.
*
* Revision 1.1.1.1  1996/01/16 17:08:09  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/11 17/08/94  12.45.54  by  Rene Brun
*-- Author :    Rene Brun   27/07/93
      SUBROUTINE HREZ0M(IB,LB,JBANK,KOF)
*.==========>
*.           RESETS TO 0. DATA STRUCTURE POINTED BY JBANK
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
      DIMENSION IB(1),LB(1)
*.___________________________________________
      JB=JBANK-KOF
      IF(JB.NE.0)THEN
         CALL VZERO(IB(JB+KNOENT),IB(JB-1)-1)
         LW=LB(JB)
         IF(LW.NE.0)THEN
            CALL VZERO(IB(LW+1),IB(LW-1))
            LN=LB(LW)
            IF(LN.NE.0)THEN
               CALL VZERO(IB(LN+1),IB(LN-1))
            ENDIF
         ENDIF
         JFUNC=LB(JB-1)
         IF(JFUNC.NE.0)CALL VZERO(IB(JFUNC+3),IB(JFUNC-1)-2)
      ENDIF
      END
