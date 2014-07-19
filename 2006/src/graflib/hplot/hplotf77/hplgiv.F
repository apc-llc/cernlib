*
* $Id: hplgiv.F,v 1.1.1.1 1996/01/19 10:50:08 mclareni Exp $
*
* $Log: hplgiv.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:08  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.04/12 16/01/89  12.08.16  by  O.Couet
*-- Author :
      SUBROUTINE HPLGIV(XL,YL,XH,YH)
*.==========>
*.       Returns the coordinates of the picture window
*..=========>
#include "hplot/hpl2.inc"
#include "hplot/hpl9.inc"
#include "hplot/quest.inc"
*._____________________________
      IQUEST(11)=NTWIN
      IQUEST(12)=NTHIST
      XL=XLOW
      YL=YLOW
      XH=XHIGH
      YH=YHIGH
      RQUEST(21)=XWMINI
      RQUEST(22)=XWMAXI
      RQUEST(23)=YWMINI
      RQUEST(24)=YWMAXI
      END

