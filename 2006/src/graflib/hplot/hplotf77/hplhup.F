*
* $Id: hplhup.F,v 1.1.1.1 1996/01/19 10:50:09 mclareni Exp $
*
* $Log: hplhup.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:09  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.18/13 16/06/94  17.48.23  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE HPLHUP(X1,Y1,X2,Y2,YM1,YM2,LHBAR)
*.==========>
*.            Draws the histogram (for option "Update")
*.            Only when scale has not changed.
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl11.inc"
      LOGICAL LHBAR
*._____________________________
*
      IHTOLD = IHTYP
      IF((LHBAR).AND.(IHTYP.EQ.0))IHTYP = 1000
*
      CALL HPLATT(1)
*
      IF(LHBAR)THEN
         CALL IGSET('BORD',1.)
         CALL IGBOX(X1,X2,Y1,Y2)
      ELSE
         IF(IHBCOL.NE.0.OR.IHTYP.NE.0)THEN
            CALL IGSET('BORD',0.)
            CALL IGBOX(X1,X2,Y1,Y2)
         ENDIF
         XLINE(1) = X1
         XLINE(2) = X1
         XLINE(3) = X2
         XLINE(4) = X2
         YLINE(1) = YM1
         YLINE(2) = Y2
         YLINE(3) = Y2
         YLINE(4) = YM2
         CALL IPL(4,XLINE,YLINE)
      ENDIF
*
      IHTYP=IHTOLD
      END
