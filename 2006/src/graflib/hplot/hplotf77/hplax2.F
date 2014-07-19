*
* $Id: hplax2.F,v 1.1.1.1 1996/01/19 10:49:57 mclareni Exp $
*
* $Log: hplax2.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:57  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.19/03 06/01/95  13.32.07  by  O.Couet
*-- Author :    O.Couet   19/10/90
      SUBROUTINE HPLAX2(PAR)
*.===========>
*.
*. Initialize the axis parameter for IGTABL (lego and Surface)
*. with the values of the HPLOT attributes. This routine is also
*. called by HPLFR3.
*.
*. _Input parameters:
*.
*.  REAL PAR : Array of attributes
*.
*..==========> (O.Couet)
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl6.inc"
#include "hplot/hpl9.inc"
#include "hplot/hpl11.inc"
#include "hplot/hpl13.inc"
      DIMENSION PAR(*)
*._____________________________
*
*
*              Axis division numbers.
*
      NDIVX = INT(RDIVX)
      NDIVY = INT(RDIVY)
      NDIVZ = INT(RDIVZ)
      IF (IXWIN.GT.1.AND.RDIVX.GT.0) THEN
         N2    = INT(NDIVX/100)
         N1    = MAX(NDIVX-(100*N2),1)
         NDIVX = (100*N2)+(N1/IXWIN)
      ENDIF
      IF (IYWIN.GT.1.AND.RDIVY.GT.0) THEN
         N2    = INT(NDIVY/100)
         N1    = MAX(NDIVY-(100*N2),1)
         NDIVY = (100*N2)+(N1/IYWIN)
      ENDIF
      IF (IYWIN.GT.1.AND.RDIVZ.GT.0) THEN
         N2    = INT(NDIVZ/100)
         N1    = MAX(NDIVZ-(100*N2),1)
         NDIVZ = (100*N2)+(N1/((IYWIN+1)/2))
      ENDIF
      PAR(1) = FLOAT(NDIVX)
      PAR(2) = FLOAT(NDIVY)
      PAR(3) = FLOAT(NDIVZ)
*
*              Axis color
*
      PAR(4) = FLOAT(IXCOL)
      PAR(5) = FLOAT(IYCOL)
      IF (RBCOL.GE.1000.) THEN
         PAR(6) = RBCOL
      ELSE
         PAR(6) = FLOAT(IYCOL)
      ENDIF
*
*              Axis label font
*
      PAR(11) = FLOAT(IHFONT(3))
*
*              Axis tick marks size
*
      IF ((YHIGH-YLOW).NE.0) THEN
         PAR(7) = XLTICK*2./(YHIGH-YLOW)
         PAR(8) = YLTICK*2./(YHIGH-YLOW)
         PAR(9) = YLTICK*2./(XHIGH-XLOW)
*
         PAR(10) = TVSIZ(7)*2./(YHIGH-YLOW)
         PAR(12) = XLVAL*(2./(YHIGH-YLOW))+1.9*PAR(7)
         PAR(13) = XLVAL*(2./(YHIGH-YLOW))+0.7*PAR(8)
         PAR(14) = YLVAL*(2./(XHIGH-XLOW))+PAR(9)
      ELSE
         CALL IGERR('Invalid world coordinates','HPLAX2')
      ENDIF
*
      END
