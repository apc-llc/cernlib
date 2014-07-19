*
* $Id: hplscm.F,v 1.1.1.1 1996/01/19 10:50:11 mclareni Exp $
*
* $Log: hplscm.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:11  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.04/12 16/01/89  12.08.22  by  O.Couet
*-- Author :
      SUBROUTINE HPLSCM
*.==========>
*.            sets and selects transformation no 1 , which allows to
*.            use centimeters , according to XSIZ/YSIZ
*.            and the option VERT/HORI .
*.=========>
#include "hplot/quest.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
*._____________________________
*
      CALL IGRNG(XSIZ,YSIZ)
      NTWIN=1
      XYTWN=RQUEST(11)
      DXWN =RQUEST(12)
      DYWN =RQUEST(13)
*
      END

