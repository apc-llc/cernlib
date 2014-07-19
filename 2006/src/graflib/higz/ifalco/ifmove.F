*
* $Id: ifmove.F,v 1.2 1996/03/07 17:38:13 couet Exp $
*
* $Log: ifmove.F,v $
* Revision 1.2  1996/03/07 17:38:13  couet
* IFALCO selection was wrong
*
* Revision 1.1.1.1  1996/02/14 13:10:31  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/08 18/01/94  14.31.23  by  O.Couet
*-- Author :
      SUBROUTINE IFMOVE(X,Y)
#include "higz/hifc2.inc"
#include "higz/hifc3.inc"
      LOGICAL FLAG1
      XL=X
      YL=Y
      FLAG1=X.LT.XMIN.OR.XMAX.LT.X.OR.Y.LT.YMIN.OR.YMAX.LT.Y
      IF (FLAG1) RETURN
      CALL IFMOV6(X,Y)
      END
