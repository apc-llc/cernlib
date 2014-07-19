*
* $Id: ifmov6.F,v 1.2 1996/03/07 17:38:11 couet Exp $
*
* $Log: ifmov6.F,v $
* Revision 1.2  1996/03/07 17:38:11  couet
* IFALCO selection was wrong
*
* Revision 1.1.1.1  1996/02/14 13:10:31  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/08 18/01/94  14.28.19  by  O.Couet
*-- Author :
      SUBROUTINE IFMOV6(X,Y)
#include "higz/hifc2.inc"
      IX=IFIX(FX*(X-XMIN)+.5)+IXMIN
      IY=IFIX(FY*(Y-YMIN)+.5)+IYMIN
      CALL IFMOV9(IX,IY)
      XD=IX
      YD=IY
      END
