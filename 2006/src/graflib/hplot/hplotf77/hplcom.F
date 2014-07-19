*
* $Id: hplcom.F,v 1.1.1.1 1996/01/19 10:49:58 mclareni Exp $
*
* $Log: hplcom.F,v $
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 19/04/95  10.26.37  by  O.Couet
*-- Author :
      SUBROUTINE HPLCOM(XU,YU,CHTITL)
*.==========>
*.        HPLOT routine to write comments on the picture
*.        (Cm are used for call to HPLTXT)
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl6.inc"
      CHARACTER*(*) CHTITL
      DIMENSION RTXFP(2)
*._____________________________
*
*----
*     Return immediately if this routine is called before
*     the first HPLOT call
*----
      IF(IPLNUM.LT.0)THEN
         CALL HBUG('HPLCOM must be called after HPLOT','HPLCOM',0)
         RETURN
      ENDIF
*
      CALL IGQ('TXFP',RTXFP)
*
      XM = FLOARG(XU)
      YM = FLOARG(YU)
      XK = ABS(XM)
      YK = ABS(YM)
*
      KORNT = 1
      KFLG  = 1
      IF (XM.LT.0.) KFLG = 2
      IF (YM.LT.0.) KFLG = KFLG+1
*---
*       Set Comment font and precision
*----
      KFONT  = IABS(IHFONT(5))
      ITXFON = KFONT/10
      ITXPRE = MOD(KFONT,10)
      IF (IHFONT(5).LT.0) ITXFON = -ITXFON
      CALL ISTXFP(ITXFON,ITXPRE)
*
      CALL HPLTXT(XK,YK,CHTITL,5,KORNT,KFLG)
*
      CALL ISTXFP(INT(RTXFP(1)),INT(RTXFP(2)))
      END
