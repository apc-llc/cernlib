*
* $Id: ifvwin.F,v 1.2 1996/03/07 17:38:19 couet Exp $
*
* $Log: ifvwin.F,v $
* Revision 1.2  1996/03/07 17:38:19  couet
* IFALCO selection was wrong
*
* Revision 1.1.1.1  1996/02/14 13:10:31  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.20/08 18/01/94  14.28.19  by  O.Couet
*-- Author :
      SUBROUTINE IFVWIN(P1,P2,P3,P4)
#include "higz/hifc2.inc"
      XMIN=P1
      XMAX=P2
      YMIN=P3
      YMAX=P4
      FX=FLOAT(IXMAX-IXMIN)/(XMAX-XMIN)
      FY=FLOAT(IYMAX-IYMIN)/(YMAX-YMIN)
      END
