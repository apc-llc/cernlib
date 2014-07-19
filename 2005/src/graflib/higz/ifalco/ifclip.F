*
* $Id: ifclip.F,v 1.2 1996/03/07 17:38:07 couet Exp $
*
* $Log: ifclip.F,v $
* Revision 1.2  1996/03/07 17:38:07  couet
* IFALCO selection was wrong
*
* Revision 1.1.1.1  1996/02/14 13:10:30  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/08 18/01/94  14.28.19  by  O.Couet
*-- Author :
      SUBROUTINE IFCLIP(X1,Y1,X2,Y2,FLAG)
#include "higz/hifc2.inc"
      LOGICAL FLAG
      IF (X1.LT.X2) THEN
         IF (X1.LT.XMIN) THEN
            Y1=Y1+(XMIN-X1)*(Y2-Y1)/(X2-X1)
            X1=XMIN
         END IF
         IF (XMAX.LT.X2) THEN
            Y2=Y2-(X2-XMAX)*(Y2-Y1)/(X2-X1)
            X2=XMAX
         END IF
      END IF
      IF (X2.LT.X1) THEN
         IF (X2.LT.XMIN) THEN
            Y2=Y2+(XMIN-X2)*(Y1-Y2)/(X1-X2)
            X2=XMIN
         END IF
         IF (XMAX.LT.X1) THEN
            Y1=Y1-(X1-XMAX)*(Y1-Y2)/(X1-X2)
            X1=XMAX
         END IF
      END IF
      FLAG=.TRUE.
      IF (Y1.LT.YMIN.AND.Y2.LT.YMIN) RETURN
      IF (YMAX.LT.Y1.AND.YMAX.LT.Y2) RETURN
      FLAG=.FALSE.
      IF (Y1.LT.Y2) THEN
         IF (Y1.LT.YMIN) THEN
            X1=X1+(YMIN-Y1)*(X2-X1)/(Y2-Y1)
            Y1=YMIN
         END IF
         IF (YMAX.LT.Y2) THEN
            X2=X2-(Y2-YMAX)*(X2-X1)/(Y2-Y1)
            Y2=YMAX
         END IF
      END IF
      IF (Y2.LT.Y1) THEN
         IF (Y2.LT.YMIN) THEN
            X2=X2+(YMIN-Y2)*(X1-X2)/(Y1-Y2)
            Y2=YMIN
         END IF
         IF (YMAX.LT.Y1) THEN
            X1=X1-(Y1-YMAX)*(X1-X2)/(Y1-Y2)
            Y1=YMAX
         END IF
      END IF
      END
