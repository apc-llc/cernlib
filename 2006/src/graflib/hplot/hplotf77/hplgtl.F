*
* $Id: hplgtl.F,v 1.1.1.1 1996/01/19 10:50:09 mclareni Exp $
*
* $Log: hplgtl.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:09  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 19/04/95  10.26.37  by  O.Couet
*-- Author :
      SUBROUTINE HPLGTL
*.==========>
*.            Draws global title
*.            (Cm are used for call to HPLTXT)
*..=========>
#include "hbook/hcbook.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl6.inc"
*
      CHARACTER*80 CHGTIT
      DIMENSION RTXFP(2)
*._____________________________
*
      IF(LGTIT.EQ.0.OR.LOGTIT)RETURN
      LOGTIT = .TRUE.
*
      CALL IGQ('TXFP',RTXFP)
*
      KORNT  = 1
      KFLG   = 2
*
      XK     = 0.5*XSIZ
      YK     = YSIZ-YGTIT-TVSIZ(2)
*
      NCHR   = 4*IQ(LGTIT-1)
      IF(NCHR.GT.80)NCHR=80
*
      KFONT  = IABS(IHFONT(2))
      IGTFON = KFONT/10
      IGTPRE = MOD(KFONT,10)
      IF(IHFONT(2).LT.0)IGTFON=-IGTFON
      CALL ISTXFP(IGTFON,IGTPRE)
*
      CHGTIT = ' '
      CALL UHTOC(IQ(LGTIT+1),4,CHGTIT,NCHR)
      CALL HPLTXT(XK,YK,CHGTIT,2,KORNT,KFLG)
*
      CALL ISTXFP(INT(RTXFP(1)),INT(RTXFP(2)))
      END
