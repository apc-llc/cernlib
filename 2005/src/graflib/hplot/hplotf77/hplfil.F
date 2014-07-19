*
* $Id: hplfil.F,v 1.2 1997/05/22 11:00:01 couet Exp $
*
* $Log: hplfil.F,v $
* Revision 1.2  1997/05/22 11:00:01  couet
* - The file name position was controlled with YLAB.
*
* Revision 1.1.1.1  1996/01/19 10:49:58  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 19/04/95  10.26.37  by  O.Couet
*-- Author :    O.Couet   13/01/89
      SUBROUTINE HPLFIL
*.==========>
*.
*.  Draw the current FILE name on the current histogram.
*.
*..==========> (O.Couet)
#include "hbook/hcdire.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl6.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl4.inc"
#include "hplot/hpl13.inc"
#include "hplot/hplstr.inc"
      DIMENSION RTXFP(2)
*._____________________________
*
      IF(.NOT.LOFILE.OR.IOPTU.NE.0)RETURN
*
      CALL IGQ('TXFP',RTXFP)
      TVCOM = TVSIZ(5)
*
*              Set file name font and precision
*
      NTWOLD = NTWIN
      KFONT  = IABS(IHFONT(5))
      ITXFON = KFONT/10
      ITXPRE = MOD(KFONT,10)
      IF(IHFONT(5).LT.0)ITXFON = -ITXFON
      CALL ISTXFP(ITXFON,ITXPRE)
      CHTEMP = ' '
      CHTEMP = HFNAME(ICDIR)
      IF(ITXFON.EQ.0)CALL IGTCCH(CHTEMP(1:96))
*
*              Draw the file name
*
      IF(TVSIZ(5).EQ.0.)TVSIZ(5) = 0.28
      IF(IFILE.EQ.1)THEN
         KFLG = 1
         XK   = XLOW
         YK   = YHIGH+0.8-XLVAL-TVSIZ(5)
      ELSEIF(IFILE.EQ.2)THEN
         KFLG = 3
         XK   = XHIGH
         YK   = YHIGH+0.8-XLVAL-TVSIZ(5)
      ELSEIF(IFILE.EQ.3)THEN
         KFLG = 1
         XK   = XLOW
         YK   = YLOW-0.8
      ELSEIF(IFILE.EQ.4)THEN
         KFLG = 3
         XK   = XHIGH
         YK   = YLOW-0.8
      ENDIF
      CALL HPLTXT(XK,YK,CHTEMP,5,1,KFLG)
*
      TVSIZ(5) = TVCOM
*
*              Reset NT
*
      IF(NTWIN.NE.NTWOLD)THEN
         NTWIN = NTWOLD
         CALL ISELNT(NTWOLD)
      ENDIF
      IF(LOZFL)CALL IZPICT(' ','O')
*
      CALL ISTXFP(INT(RTXFP(1)),INT(RTXFP(2)))
      END
