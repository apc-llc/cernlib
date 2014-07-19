*
* $Id: hplkey.F,v 1.1.1.1 1996/01/19 10:50:09 mclareni Exp $
*
* $Log: hplkey.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:09  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 19/04/95  10.26.37  by  O.Couet
*-- Author :
      SUBROUTINE HPLKEY(XU,YU,ISYM,CHTXT)
*.==========>
*.            Routine to draw a symbol and its explanation
*.            (Cm are used for call to HPLTXT)
*..=========>
#include "hplot/hpl6.inc"
      CHARACTER*(*) CHTXT
      DIMENSION RTXFP(2)
*._____________________________
*
      CALL IGQ('TXFP',RTXFP)
*----
*     Decode position information
*----
      XK    = FLOARG(XU)
      YK    = FLOARG(YU)
      KORNT = 1
      KFLG  = 1
      CALL HPLSYM(XK,YK,1,ISYM,0,'CM  ')
*----
*     Calculate position for text
*----
      XK = XK+2.*TVSIZ(1)
      YK = YK-0.5*TVSIZ(5)
*---
*     Set font and precision
*----
      KFONT  = IABS(IHFONT(5))
      ITXFON = KFONT/10
      ITXPRE = MOD(KFONT,10)
      IF (IHFONT(5).LT.0) ITXFON = -ITXFON
      CALL ISTXFP(ITXFON,ITXPRE)
*
      CALL HPLTXT(XK,YK,CHTXT,5,KORNT,KFLG)
*
      CALL ISTXFP(INT(RTXFP(1)),INT(RTXFP(2)))
      END
