*
* $Id: ifdraw.F,v 1.2 1996/03/07 17:38:08 couet Exp $
*
* $Log: ifdraw.F,v $
* Revision 1.2  1996/03/07 17:38:08  couet
* IFALCO selection was wrong
*
* Revision 1.1.1.1  1996/02/14 13:10:30  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/08 18/01/94  14.31.23  by  O.Couet
*-- Author :    O.Couet   11/01/91
      SUBROUTINE IFDRAW(X,Y)
#include "higz/hifc2.inc"
#include "higz/hifc3.inc"
      LOGICAL FLAG1
      LOGICAL FLAG2,FLAG3
      XS=XL
      YS=YL
      FLAG2=FLAG1
      XL=X
      YL=Y
      FLAG1=X.LT.XMIN.OR.XMAX.LT.X.OR.Y.LT.YMIN.OR.YMAX.LT.Y
      IF (FLAG2.AND.FLAG1) THEN
         IF (XS.LT.XMIN.AND.X.LT.XMIN) RETURN
         IF (XMAX.LT.XS.AND.XMAX.LT.X) RETURN
         IF (YS.LT.YMIN.AND.Y.LT.YMIN) RETURN
         IF (YMAX.LT.YS.AND.YMAX.LT.Y) RETURN
      END IF
      XT=X
      YT=Y
      FLAG3=.FALSE.
      IF (FLAG2.OR.FLAG1) CALL IFCLIP(XS,YS,XT,YT,FLAG3)
      IF (FLAG3) RETURN
      IF (FLAG2) CALL IFMOV6(XS,YS)
      IX=IFIX(FX*(XT-XMIN)+.5)+IXMIN
      IY=IFIX(FY*(YT-YMIN)+.5)+IYMIN
      CALL IFDRW9(IX,IY)
      XD=IX
      YD=IY
      END
